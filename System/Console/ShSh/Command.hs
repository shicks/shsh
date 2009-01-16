-- |Here we run commands.

{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Command ( runCommands, source, eval ) where

import System.Console.ShSh.Builtins ( builtin )
import System.Console.ShSh.Foreign.Pwd ( getHomeDir )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn, oFlush, eFlush,
                                iHandle, oHandle, eHandle )
import System.Console.ShSh.Internal.IO ( newPipe, rGetContents )
import System.Console.ShSh.Internal.Process ( WriteStream(..),
                                              PipeState(..) )
import System.Console.ShSh.ShellError ( announceError )
import System.Console.ShSh.Shell ( Shell, ShellProcess(..),
                                   withShellProcess, maybeCloseIn,
                                   maybeCloseOut, subShell, makeLocal,
                                   runShellProcess, setEnv, getAllEnv,
                                   setFunction, getFunction, setExitCode,
                                   setExport, getExports, getPositionals,
                                   pipeShells, runInShell, getExitCode,
                                   withEnvironment, withExitHandler,
                                   getFlag, getAliases, withPositionals,
                                   withErrorsPrefixed, withPipes )

import Language.Sh.Glob ( expandGlob, matchPattern )
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
import Control.Monad ( when, unless, forM )
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
runList b (Singleton p) = runShellProcess =<< pipeline b p
runList b (l :&&: p)    = do ec <- runList IgnoreE l
                             if ec == ExitSuccess
                                then runShellProcess =<< pipeline b p
                                else return ec
runList b (l :||: p)    = do ec <- runList IgnoreE l
                             if ec /= ExitSuccess
                                then runShellProcess =<< pipeline b p
                                else return ec

-- |Modifies a Shell ShellProcess to set the exit code.
sec :: Shell ShellProcess -> Shell ShellProcess
sec = fmap $ withShellProcess $ \job -> job >>= setExitCode

-- |Run a 'Pipeline'.  (or rather, return something that will)
pipeline :: OnErr -> Pipeline -> Shell ShellProcess
pipeline b (Pipeline [s]) = sec $ runStatement b s
pipeline b (Pipeline (s:ss)) = do s' <- runStatement IgnoreE s
                                  ss' <- pipeline b $ Pipeline ss
                                  return $ pipeShells s' ss'
pipeline b (BangPipeline [s]) = sec $ notProcess `fmap` runStatement IgnoreE s
pipeline b (BangPipeline (s:ss)) = do s' <- runStatement IgnoreE s
                                      ss' <- pipeline IgnoreE $ BangPipeline ss
                                      return $ pipeShells s' ss'

notProcess :: ShellProcess -> ShellProcess
notProcess = withShellProcess $ \sp ->
             do ec <- sp
                case ec of
                  ExitSuccess -> return $ ExitFailure 1
                  ExitFailure _ -> return $ ExitSuccess

checkE :: OnErr -> ShellProcess -> ShellProcess
checkE CheckE = withShellProcess $ \sp ->
                do ec <- sp
                   am_e <- getFlag 'e'
                   when (am_e && ec/=ExitSuccess) $ liftIO $ exitWith ec
                   return ec
checkE IgnoreE = id

withEnv = withEnvironment expandWord

-- |Run a 'Statement'.
runStatement :: OnErr -> Statement -> Shell ShellProcess
runStatement b (Compound c rs) = return $ BuiltinProcess $ withEnv rs [] $
                                                  runCompound b c
runStatement _ (FunctionDefinition s c rs) = return $ BuiltinProcess $
                                             do setFunction s c rs
                                                return ExitSuccess
runStatement _ (OrderedStatement ts) = fail $ "OrderedStatement got through: "
                                         ++ show ts
runStatement b (Statement ws rs as) = checkE b `fmap`
    do ws' <- expandWords ws
       case ws' of
         [] -> return $ BuiltinProcess $ withEnv rs [] $ setVars as
         ("local":xs) -> return $ BuiltinProcess $ setLocals xs
         ["export"] -> return $ BuiltinProcess $ withEnv rs as $ setExports []
         ("export":xs) -> return $ BuiltinProcess $
                          setExports xs -- NOTE: can't be in a withEnv
         (name:args) -> do func <- getFunction name
                           case func of   -- order of redirs matches dash
                             Just (f,rs') -> return $ BuiltinProcess $
                                               withEnv (rs++rs') as $
                                               withPositionals args $
                                               runCompound b f
                             Nothing      -> do job <- runBuiltinOrExe
                                                       name args
                                                return $
                                                  withShellProcess `flip` job $
                                                    \j -> withEnv rs as j

runCompound :: OnErr -> CompoundStatement -> Shell ExitCode
runCompound b (For var list cs) =
    do ws <- expandWords list
       ecs <- forM ws $ \w -> do setEnv var w
                                 runCommands' b cs
       return $ if null ecs
                then ExitSuccess
                else head $ reverse $ ecs
runCompound b (If cond thn els) =
    do ec <- runCommands' IgnoreE cond
       case ec of ExitSuccess -> runCommands' b thn
                  _           -> runCommands' b els
runCompound b (Case expr cases) = do e <- expandWord expr
                                     run b e cases
    where run _ _ [] = return ExitSuccess
          run b s ((ps,cs):xs) = do match <- check s ps
                                    if match then runCommands' b cs
                                             else run b s xs
          check s [] = return False
          check s (p:ps) = do p' <- expandPattern p
                              if matchPattern p' s then return True
                                                   else check s ps
runCompound b (While cond code) =
    do ec <- runCommands' IgnoreE cond
       case ec of ExitSuccess -> do runCommands' b code
                                    runCompound b $ While cond code
                  _           -> return ExitSuccess
runCompound b (Until cond code) =
    do ec <- runCommands' IgnoreE cond
       case ec of ExitSuccess -> return ExitSuccess
                  _           -> do runCommands' b code
                                    runCompound b $ Until cond code
runCompound b (BraceGroup cs) = runCommands' b cs
runCompound _ c = fail $ "Control structure "++show c++" not yet supported."

openHandles :: Shell () -- this is weird -> these shouldn't have side effects
openHandles = iHandle >> oHandle >> eHandle >> return ()

runBuiltinOrExe :: String -> [String] -> Shell ShellProcess
runBuiltinOrExe = run' where -- now this is just for layout...
  run' "." args = run' "source" args
  run' "source" (f:args) | null args = return $ BuiltinProcess $ source f
                         | otherwise = return $ BuiltinProcess $
                                                withPositionals args $ source f
  run' "source" [] = return $ BuiltinProcess $ fail "filename argument required"
  run' command args = do b <- builtin command
                         return $ case b of
                           Just b' -> BuiltinProcess $ withExitHandler $
                                      withErrorsPrefixed command $ do
                                        openHandles
                                        ec <- b' args -- after builtins are run.
                                        oFlush -- to behave like external
                                        eFlush -- commands, need to flush
                                        return ec
                           Nothing -> ExternalProcess $ withExitHandler $
                                        runWithArgs command args

-- at some point we need to use our own path here...
runWithArgs :: String -> [String] -> Shell ExitCode
runWithArgs cmd args
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
           Just fp -> runInShell fp args
           Nothing -> do if path && (exists || exists_exe)
                            then ePutStrLn $ cmd++": Permission denied"
                            else ePutStrLn $ cmd++": No such file or directory"
                         return $ ExitFailure 127


setVars [] = return ExitSuccess
setVars ((name:=word):as) = (setEnv name =<< expandWord word) >> setVars as

-- |These should really be in 'Builtins', but we want them outside the
-- 'withEnvironment'...  we could process in multiple stages...
setLocals [] = return ExitSuccess
setLocals (x:xs) = do let (name,val) = break (=='=') x
                      makeLocal name
                      unless (null val) $ setEnv name $ drop 1 val
                      setLocals xs -- poor man's mapM_ >> return ExitSuccess

setExports [] = getExports >>= mapM (oPutStrLn . (\(s,v) -> "export "++s++"="++v))
                >> return ExitSuccess
setExports xs = mapM_ se xs >> return ExitSuccess
    where se x = do let (name,val) = break (=='=') x
                    setExport name True
                    unless (null val) $ setEnv name $ drop 1 val

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

eval :: String -> Shell ExitCode
eval c = eval' "" $ lines c
    where eval' :: String -> [String] -> Shell ExitCode
          eval' i rest =
              do if null rest
                    then getExitCode
                    else do let ([s],rest') = splitAt 1 rest
                            am_v <- getFlag 'v'
                            when am_v $ ePutStrLn s
                            as <- getAliases
                            case parse as (i++s) of
                              Left (err,False) -> do
                                when (null rest') $ fail err
                                eval' (i++s++"\n") rest'
                              Left (err,True) -> do
                                ePutStrLn err -- refail at eof?
                                eval' "" rest'
                              Right cs -> if hereDocsComplete cs
                                          then runCommands cs >> eval' "" rest'
                                          else if null rest'
                                               then runCommands cs
                                               else eval' (i++s++"\n") rest'

source :: FilePath -> Shell ExitCode
source f = liftIO (readFile f) >>= eval

-- |Functions to pass to the actual @Expansion@ module.
ef :: E.ExpansionFunctions Shell
ef = E.ExpansionFunctions { E.getAllEnv = getAllEnv,
                            E.setEnv = setEnv,
                            E.homeDir = liftIO . getHomeDir,
                            E.expandGlob = expandGlob,
                            E.commandSub = captureOutput .
                                           subShell . runCommands,
                            E.positionals = getPositionals }

-- |Expand a single word into a single string; no field splitting
expandWord :: Word -> Shell String
expandWord = E.expandWord ef

-- |Expand a single word into a single string; no field splitting
expandPattern :: Word -> Shell Word
expandPattern = E.expandPattern ef

-- |Expand a list of words into a list of strings; fields will be split.
expandWords :: [Word] -> Shell [String]
expandWords = E.expand ef
