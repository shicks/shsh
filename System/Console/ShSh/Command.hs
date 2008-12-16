-- |Here we run commands.

{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Command ( runCommands, source ) where

import System.Console.ShSh.Builtins ( builtin )
import System.Console.ShSh.Foreign.Pwd ( getHomeDir )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn, oFlush, eFlush )
import System.Console.ShSh.Internal.IO ( newPipe, rGetContents )
import System.Console.ShSh.Internal.Process ( WriteStream(..),
                                              PipeState(..) )
import System.Console.ShSh.ShellError ( announceError )
import System.Console.ShSh.Shell ( Shell, ShellProcess, mkShellProcess,
                                   maybeCloseOut, subShell,
                                   runShellProcess, setEnv, getAllEnv,
                                   setFunction, getFunction,
                                   pipeShells, runInShell, getExitCode,
                                   withEnvironment, withExitHandler,
                                   getFlag, pipes, getAliases,
                                   withErrorsPrefixed, withPipes )

import Language.Sh.Glob ( expandGlob )
import Language.Sh.Parser ( parse, hereDocsComplete )
import Language.Sh.Syntax ( Command(..), AndOrList(..),
                            Pipeline(..), Statement(..),
                            CompoundStatement(..),
                            Word, Lexeme(..), Assignment(..) )
import qualified Language.Sh.Expansion as E

import System.Directory ( findExecutable, doesFileExist, getCurrentDirectory )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( Handle, IOMode(..), openFile, hGetLine, hIsEOF )

import Control.Monad.Trans ( liftIO )
import Control.Monad ( when, forM )
import Data.Monoid ( mempty )

import Debug.Trace ( trace )

-- |What to do on failure?
data OnErr = IgnoreE | CheckE

-- |Simply run a 'Command'.
runCommand :: OnErr -> Command -> Shell ExitCode
runCommand _ (Asynchronous list) = do runAsync $ withExitHandler $
                                               runList IgnoreE list
                                      return ExitSuccess
runCommand b (Synchronous list) = withExitHandler $ runList b list

runAsync :: Shell a -> Shell ExitCode
runAsync _ = fail "Asyncronous commands not yet supported"
             >> return ExitSuccess -- i.e. (false &) && echo 1

-- |Run an 'AndOrList'.
runList :: OnErr -> AndOrList -> Shell ExitCode
runList b (Singleton p) = runShellProcess $ pipeline b p
runList b (l :&&: p)    = do ec <- runList IgnoreE l
                             if ec == ExitSuccess
                                then runShellProcess $ pipeline b p
                                else return ec
runList b (l :||: p)    = do ec <- runList IgnoreE l
                             if ec /= ExitSuccess
                                then runShellProcess $ pipeline b p
                                else return ec

-- |Run a 'Pipeline'.  (or rather, return something that will)
pipeline :: OnErr -> Pipeline -> ShellProcess ()
pipeline b (Pipeline [s]) = runStatement b s
pipeline b (Pipeline (s:ss)) = pipeShells (runStatement IgnoreE s)
                                  (pipeline b $ Pipeline ss)
pipeline b (BangPipeline [s]) = notProcess (runStatement IgnoreE s)
pipeline b (BangPipeline (s:ss)) = pipeShells (runStatement IgnoreE s)
                                      (pipeline IgnoreE $ BangPipeline ss)

notProcess :: ShellProcess () -> ShellProcess ()
notProcess sp ip = do ec <- sp ip
                      case ec of
                        ExitSuccess -> return $ ExitFailure 1
                        ExitFailure _ -> return $ ExitSuccess

checkE :: OnErr -> Shell ExitCode -> Shell ExitCode
checkE CheckE sp = do ec <- sp
                      am_e <- getFlag 'e'
                      when (am_e && ec/=ExitSuccess) $ liftIO $ exitWith ec
                      return ec
checkE IgnoreE sp = sp

-- |Run a 'Statement'.
runStatement :: OnErr -> Statement -> ShellProcess ()
runStatement b (Compound c rs) ip = mkShellProcess `flip` ip $
                                    withEnvironment expandWord rs [] $
                                    runCompound b c
runStatement _ (FunctionDefinition s c rs) ip = mkShellProcess `flip` ip $
                                                do setFunction s c rs
                                                   return ExitSuccess
runStatement b (Statement ws rs as) ip =
    checkE b $ do ws' <- expandWords ws
                  case ws' of
                    [] -> mkShellProcess `flip` ip $
                          withEnvironment expandWord rs [] $ setVars as
                    ("local":xs) -> fail "can't do locals yet"
                    xs -> withEnvironment expandWord rs as $ run' b xs ip

runCompound b (For var list cs) =
    do ws <- expandWords list
       ecs <- forM ws $ \w -> do setEnv var w
                                 runCommands' b cs
       return $ if null ecs
                then ExitSuccess
                else head $ reverse $ ecs
runCompound b (If cond thn els) =
    do ec <- runCommands' IgnoreE cond
       case ec of
         ExitSuccess -> runCommands' b thn
         _           -> runCommands' b els
runCompound b (BraceGroup cs) = runCommands' b cs
runCompound _ c = fail $ "Control structure "++show c++" not yet supported."

run' :: OnErr -> [String] -> ShellProcess () -- list NOT EMPTY
run' b (name:args) ip =
    do func <- getFunction name
       case func of
         Just (f,rs) -> mkShellProcess `flip` ip $
                        withEnvironment expandWord rs pos $ runCompound b f
         Nothing     -> run'' (name:args) ip
    where pos = map (\(n,s)->show n:=map Literal s) $ zip [1..] args

run' _ [] ip = fail "how did an empty command get through?"
run'' (".":args) ip = run'' ("source":args) ip
run'' ("source":f:_) ip = do mkShellProcess (source f) ip
run'' ["source"] ip = do mkShellProcess (fail "filename argument required") ip
run'' (command:args) ip = do b <- builtin command
                             oFlush -- to behave like external commands we need to
                             eFlush -- flush stdout/err after builtins are run.
                             p <- pipes
                             withExitHandler $ case b of
                               Just b' -> withErrorsPrefixed command $ b' args ip
                               Nothing -> runWithArgs command args ip

-- at some point we need to use our own path here...
runWithArgs :: String -> [String] -> ShellProcess ()
runWithArgs cmd args ip
    = do exists <- liftIO $ doesFileExist cmd
         notWindows <- elem '/' `fmap` liftIO getCurrentDirectory
         exists_exe <- if notWindows
                       then return False
                       else liftIO $ doesFileExist (cmd++".exe")
         let path = if notWindows
                    then '/' `elem` cmd
                    else '/' `elem` cmd || '\\' `elem` cmd
         exe <- liftIO $ if path
                         then return $ if exists || exists_exe
                                       then Just cmd
                                       else Nothing
                         else findExecutable cmd
         case exe of
           Just fp -> runInShell fp args ip
           Nothing -> do if path && (exists || exists_exe)
                            then ePutStrLn $ cmd++": Permission denied"
                            else ePutStrLn $ cmd++": No such file or directory"
                         return $ ExitFailure 127


setVars [] = return ExitSuccess
setVars ((name:=word):as) = (setEnv name =<< expandWord word) >> setVars as

-- |We need to be able to do this in a sort of "half -v" mode in which
-- shell errors (bad substitution, parse, etc) cause it to quite, but
-- bad exitcodes are OK.
runCommands :: [Command] -> Shell ExitCode
runCommands = runCommands' CheckE

runCommands' :: OnErr -> [Command] -> Shell ExitCode
runCommands' b xs = mapM_ (runCommand b) xs >> getExitCode

-- |We want to set up a @Chan@ to read from a process.
captureOutput :: Shell a -> Shell String
captureOutput job = do (r,w) <- liftIO $ newPipe
                       withPipes (mempty { p_out = WUseHandle w }) $
                           job >> maybeCloseOut
                       liftIO $ rGetContents r

source :: FilePath -> Shell ExitCode
source f = do h <- liftIO $ openFile f ReadMode
              source' "" h
    where source' :: String -> Handle -> Shell ExitCode
          source' i h =
              do eof <- liftIO $ hIsEOF h
                 if eof
                    then getExitCode
                    else liftIO (hGetLine h) >>= \s -> do
                      am_v <- getFlag 'v'
                      when am_v $ ePutStrLn s
                      as <- getAliases
                      case parse as (i++s) of
                        Left (err,False) -> do
                           eof <- liftIO $ hIsEOF h
                           when eof $ fail err
                           source' (i++s++"\n") h
                        Left (err,True) -> do
                           ePutStrLn err -- refail at eof?
                           source' "" h
                        Right cs -> if hereDocsComplete cs
                                    then runCommands cs >> source' "" h
                                    else do eof <- liftIO $ hIsEOF h
                                            if eof then runCommands cs
                                                   else source' (i++s++"\n") h

-- |Functions to pass to the actual @Expansion@ module.
ef :: E.ExpansionFunctions Shell
ef = E.ExpansionFunctions { E.getAllEnv = getAllEnv,
                            E.setEnv = setEnv,
                            E.homeDir = liftIO . getHomeDir,
                            E.expandGlob = expandGlob,
                            E.commandSub = captureOutput .
                                           subShell . runCommands }

-- |Expand a single word into a single string; no field splitting
expandWord :: Word -> Shell String
expandWord = E.expandWord ef

-- |Expand a list of words into a list of strings; fields will be split.
expandWords :: [Word] -> Shell [String]
expandWords = E.expand ef
