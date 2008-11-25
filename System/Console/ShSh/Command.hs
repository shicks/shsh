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
                                   runShellProcess, setEnv, getEnv,
                                   pipeShells, runInShell, getExitCode,
                                   withEnvironment, withExitHandler,
                                   getFlag, pipes, getAliases,
                                   withPipes )

import Language.Sh.Parser ( parse )
import Language.Sh.Syntax ( Command(..), AndOrList(..),
                            Pipeline(..), Statement(..),
                            Word(..), Assignment(..) )
import qualified Language.Sh.Expansion as E

import System.Directory ( findExecutable, doesFileExist )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( Handle, IOMode(..), openFile, hGetLine, hIsEOF )

import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )
import Data.Monoid ( mempty )

-- |What to do on failure?
data OnErr = IgnoreE | CheckE

-- |Simply run a 'Command'.
runCommand :: Command -> Shell ExitCode
runCommand (Synchronous list) = withExitHandler $ runList CheckE list
runCommand (Asynchronous list) = do runAsync $ withExitHandler $
                                             runList IgnoreE list
                                    return ExitSuccess
runCommand c = fail $ "Control structure "++show c++" not yet supported."

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

-- |Run a 'Statement'.
runStatement :: OnErr -> Statement -> ShellProcess ()
runStatement IgnoreE s = run s
runStatement CheckE s = checkE $ run s

checkE :: ShellProcess () -> ShellProcess ()
checkE sp ip = do ec <- sp ip
                  am_e <- getFlag 'e'
                  when (am_e && ec/=ExitSuccess) $ liftIO $
                       exitWith ec
                  return ec

run :: Statement -> ShellProcess ()
run (Subshell _ _) _ = fail "Subshells not supported yet."
run (Statement ws rs as) ip = do ws' <- expandWords ws
                                 case ws' of
                                   [] -> mkShellProcess (setVars as) ip
                                   ("local":xs) -> fail "can't do locals yet"
                                   xs -> withEnvironment expandWord rs as $
                                         run' xs ip

run' :: [String] -> ShellProcess () -- list NOT EMPTY
run' (".":args) ip = run' ("source":args) ip
run' ("source":f:_) ip = do mkShellProcess (source f) ip
run' ["source"] ip = do mkShellProcess (fail "filename argument required") ip
run' (command:args) ip = do b <- builtin command
                            oFlush -- to behave like external commands we need to
                            eFlush -- flush stdout/err after builtins are run.
                            p <- pipes
                            case b of
                              Just b' -> b' args ip
                              Nothing -> runWithArgs command args ip

-- at some point we need to use our own path here...
runWithArgs :: String -> [String] -> ShellProcess ()
runWithArgs cmd args ip = do exists <- liftIO $ doesFileExist cmd
                             exe <- liftIO $ if path
                                             then return $ if exists
                                                           then Just cmd
                                                           else Nothing
                                             else findExecutable cmd
                             case exe of
                               Just fp -> runInShell fp args ip
                               Nothing -> notFound exists -- just fail...
    where path = '/' `elem` cmd
          notFound exists = if path && exists
                            then fail $ cmd++": Permission denied"
                            else fail $ cmd++": No such file or directory"

setVars [] = return ExitSuccess
setVars ((name:=word):as) = (setEnv name =<< expandWord word) >> setVars as

-- |We need to be able to do this in a sort of "half -v" mode in which
-- shell errors (bad substitution, parse, etc) cause it to quite, but
-- bad exitcodes are OK.
runCommands :: [Command] -> Shell ExitCode
runCommands xs = mapM_ runCommand xs >> getExitCode

-- |We want to set up a @Chan@ to read from a process.
captureOutput :: Shell a -> Shell String
captureOutput job = do (r,w) <- liftIO $ newPipe
                       withPipes (mempty { p_out = WUseHandle w }) $
                           job >> maybeCloseOut
                       liftIO $ rGetContents r

source :: FilePath -> Shell ExitCode
source f = do h <- liftIO $ openFile f ReadMode
              source' "" h

source' :: String -> Handle -> Shell ExitCode
source' i h = do eof <- liftIO $ hIsEOF h
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
                        Right cs -> runCommands cs >> source' "" h

-- |Functions to pass to the actual @Expansion@ module.
ef :: E.ExpansionFunctions Shell
ef = E.ExpansionFunctions { E.getEnv = getEnv,
                            E.setEnv = setEnv,
                            E.homeDir = liftIO . getHomeDir,
                            E.expandGlob = E.noGlobExpansion,
                            E.commandSub = captureOutput .
                                           subShell . runCommands }

-- |Expand a single word into a single string; no field splitting
expandWord :: Word -> Shell String
expandWord = E.expandWord ef

-- |Expand a list of words into a list of strings; fields will be split.
expandWords :: [Word] -> Shell [String]
expandWords = E.expand ef
