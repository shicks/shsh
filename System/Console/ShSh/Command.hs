{-# OPTIONS_GHC -Wall #-}

-- |Here we parse the shell AST and delegate all the builtins,
-- piping, command-running, etc.
module System.Console.ShSh.Command ( runCommands, source, eval ) where

import System.Console.ShSh.Builtins ( builtin, showAlias )
import System.Console.ShSh.Foreign.Pwd ( getHomeDir )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn, oFlush, eFlush,
                                iHandle, oHandle, eHandle )
import System.Console.ShSh.Internal.IO ( newPipe, rGetContents )
import System.Console.ShSh.Internal.Process ( WriteStream(..),
                                              PipeState(..) )
import System.Console.ShSh.Path ( findExecutable )
import System.Console.ShSh.ShellError ( failWith )
import System.Console.ShSh.Shell ( Shell, ShellProcess(..),
                                   withShellProcess,
                                   withChangeCodeHandler, withMaybeHandler,
                                   maybeCloseOut, withRedirects,
                                   subShell, makeLocal, finally, unsetEnv,
                                   runShellProcess, setEnv, getAllEnv,
                                   setFunction, getFunction, setExitCode,
                                   setExport, getExports, getPositionals,
                                   pipeShells, runInShell, getExitCode,
                                   withEnvironment, withExitHandler,
                                   getFlag, getAliases, withPositionals,
                                   withErrorsPrefixed, withPipes
                                   -- DEBUG!
                                 , envVars )

import Language.Sh.Glob ( expandGlob, matchPattern )
import Language.Sh.Parser ( parse, hereDocsComplete )
import Language.Sh.Pretty ( pretty )
import Language.Sh.Syntax ( Command(..), AndOrList(..),
                            Pipeline(..), Statement(..),
                            CompoundStatement(..),
                            Word, Redir, Assignment(..) )
import qualified Language.Sh.Expansion as E

import Control.Monad.Trans ( liftIO )
import Control.Monad ( when, unless, forM, forM_ )
import Data.Maybe ( catMaybes )
import Data.Monoid ( mempty )
import System.Exit ( ExitCode(..), exitWith )

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
pipeline _ (Pipeline []) = error "impossible"
pipeline _ (BangPipeline [s]) = sec $ notProcess `fmap` runStatement IgnoreE s
pipeline _ (BangPipeline (s:ss)) = do s' <- runStatement IgnoreE s
                                      ss' <- pipeline IgnoreE $ BangPipeline ss
                                      return $ pipeShells s' ss'
pipeline _ (BangPipeline []) = error "impossible"

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

withEnv ::[Redir] -> [Assignment] -> Shell ExitCode -> Shell ExitCode
withEnv = withEnvironment expandWord

withRedirs ::[Redir] -> Shell ExitCode -> Shell ExitCode
withRedirs = withRedirects expandWord

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
         [] -> return $ BuiltinProcess $ withRedirs rs $ setVars as
         ("unset":xs) -> return $ BuiltinProcess $ do forM_ xs unsetEnv
                                                      return ExitSuccess
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
                                     run e cases
    where run _ [] = return ExitSuccess
          run s ((ps,cs):xs) = do match <- check s ps
                                  if match then runCommands' b cs
                                           else run s xs
          check _ [] = return False
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
runCompound b (BraceGroup cs) = runCommands' b cs -- what to do here...?
                                `finally` maybeCloseOut -- ?????
runCompound b (Subshell cs) = runCompound b (BraceGroup cs) -- FOR NOW!!!
--runCompound _ c = fail $ "Control structure "++show c++" not yet supported."

openHandles :: Shell () -- this is weird -> these shouldn't have side effects
openHandles = iHandle >> oHandle >> eHandle >> return ()

runBuiltinOrExe :: String -> [String] -> Shell ShellProcess
runBuiltinOrExe = run' where -- now this is just for layout...
  run' "env#" _ = return $ BuiltinProcess $ do -- debug builtin
                   (e,l,p) <- envVars
                   oPutStrLn $ "environment\n" ++ unlines (map show e)
                   oPutStrLn $ "locals\n" ++ unlines (map show l)
                   oPutStrLn $ "positionals: " ++ show p
                   return ExitSuccess
  run' "." args = run' "source" args
  run' "source" (f:args) | null args = return $ BuiltinProcess $ source f
                         | otherwise = return $ BuiltinProcess $
                                                withPositionals args $ source f
  run' "source" [] = return $ BuiltinProcess $ fail "filename argument required"
  run' "command" args = return $ BuiltinProcess $ commandBuiltin args
  run' "type" args = return $ BuiltinProcess $ typeBuiltin args
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

runWithArgs :: String -> [String] -> Shell ExitCode
runWithArgs cmd args = do exe <- withChangeCodeHandler 127 $ findExecutable cmd
                          runInShell exe args

setVars :: [Assignment] -> Shell ExitCode
setVars [] = return ExitSuccess
setVars ((name:=word):as) = (setEnv name =<< expandWord word) >> setVars as

-- |These should really be in 'Builtins', but we want them outside the
-- 'withEnvironment'...  we could process in multiple stages...
setLocals :: [String] -> Shell ExitCode
setLocals [] = return ExitSuccess
setLocals (x:xs) = do let (name,val) = break (=='=') x
                      makeLocal name
                      unless (null val) $ setEnv name $ drop 1 val
                      setLocals xs -- poor man's mapM_ >> return ExitSuccess

setExports :: [String] -> Shell ExitCode
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

------------------------------------------------------------------------
-- I don't particularly like having this here, but where else can it go?

data Resolved = Executable FilePath
              | Builtin ([String] -> Shell ExitCode)
              | Function CompoundStatement [Redir]
              | Alias String
              | Keyword

resolveCommand :: String -> Shell [Resolved]
resolveCommand s = catMaybes `fmap` sequence -- special path?
                     [(fmap Alias . lookup s) `fmap` getAliases
                     ,return $ if s `elem` keywords then Just Keyword
                                                    else Nothing
                     ,fmap (uncurry Function) `fmap` getFunction s
                     ,fmap Builtin `fmap` builtin s
                     ,fmap Executable `fmap`
                           withMaybeHandler (findExecutable s)]

keywords :: [String]
keywords = ["for", "in", "do", "done", "while", "until",
            "if", "then", "elif", "fi", "case", "{", "}", "[[", "]]"]

--                       case lookup s as of
--                         Just a -> return $ Alias a
--                         Nothing -> do
--                           func <- getFunction s
--                           case func of
--                             Just (f,rs) -> return $ Function f rs
--                             Nothing -> do
--                               b <- builtin command
--                               case b of
--                                 Just b' -> return $ Builtin
--                                 Nothing -> do
--                                   exe <- withMaybeHandler $ findExecutable cmd
--                                   case exe of
--                                     Just fp -> return Executable fp
--                                     Nothing -> return NotFound

-- |This is a complicated builtin that we need to put here, since
-- it needs to call functions in this module...  We might later work
-- out a distinction between POSIX builtins and standard utils, as well
-- as the @-p@ path, but for now, we don't do much.
-- Additionally, bash's command -V tells whether something is hashed...
data CommandMode = Path | Verb | Basic | Empty
commandBuiltin :: [String] -> Shell ExitCode
commandBuiltin = run `flip` Empty
    where run [] _ = return ExitSuccess
          run (('-':a:[]):as) m = run as $ mode m a
          run (('-':a:a'):as) m = run (('-':a'):as) $ mode m a
          run (s:args) Empty = do rs <- resolveCommand s
                                  case take 1 rs of
                                    [Executable fp] -> runInShell fp args
                                    [Builtin b] -> b args
                                    _ -> failWith 127 $ s++": command not found"
          run (s:args) Path = run (s:args) Empty -- for now...?
          run ss Basic = anyM ss $ \s -> short s =<< resolveCommand s
          run ss Verb = anyM ss $ \s -> doType "command" s =<< resolveCommand s
          mode m c = case c of { 'p'->Path; 'v'->Basic; 'V'->Verb; _->m }
          anyM ss f = do res <- forM ss f
                         if any id res then return ExitSuccess
                                       else return $ ExitFailure 1
          short s (x:_:_) = short s [x]
          short _ [Executable fp] = oPutStrLn fp >> return True
          short s [Builtin _] = oPutStrLn s >> return True
          short s [Keyword] = oPutStrLn s >> return True
          short s [Function _ _] = oPutStrLn s >> return True
          short s [Alias s'] = showAlias s s' >> return True
          short _ [] = return False

typeBuiltin :: [String] -> Shell ExitCode
typeBuiltin ss = anyM $ \s -> doType "type" s =<< resolveCommand s
    where anyM f = do res <- forM ss f -- could probably use a clever fold
                      if any id res then return ExitSuccess
                                    else return $ ExitFailure 1

doType :: String -> String -> [Resolved] -> Shell Bool
doType c s (x:_:_) = doType c s [x]
doType _ s [Executable fp] = oPutStrLn (s++" is "++fp) >> return True
doType _ s [Builtin _] = oPutStrLn (s++" is a shell builtin") >> return True
doType _ s [Keyword] = oPutStrLn (s++" is a shell keyword") >> return True
doType _ s [Function f rs] = do oPutStrLn $ s++" is a function"
                                oPutStrLn $ pretty $ FunctionDefinition s f rs
                                return True
doType _ s [Alias s'] = do oPutStrLn $ s++" is aliased to `"++s'++"'"
                           return True
doType cmd s [] = do ePutStrLn $ "shsh: "++cmd++": "++s++": not found"
                     return False

-----

-- We have extra builtins that we have to have here for silly reasons...
-- (although we could put the pertinent things in a class to separate)
-- -> these are hardcoded into runStatement and runBuiltinOrExe -> ...
-- Currently these don't work properly with command/type.
-- specialBuiltins :: [(String,[String] -> ShellProcess)]

