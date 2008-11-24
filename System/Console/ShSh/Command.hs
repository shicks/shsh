-- |Here we run commands.

{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Command ( runCommand ) where

import System.Console.ShSh.Builtins ( builtin )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn, oFlush, eFlush )
import System.Console.ShSh.ShellError ( announceError )
import System.Console.ShSh.Shell ( Shell, ShellProcess, mkShellProcess,
                                   runShellProcess, setEnv,
                                   pipeShells, runInShell,
                                   withEnvironment, withExitHandler,
                                   getFlag, pipes )
import System.Console.ShSh.Expansion ( expandWord, expandWords )

import Language.Sh.Syntax ( Command(..), AndOrList(..),
                            Pipeline(..), Statement(..),
                            Assignment(..) )

import System.Directory ( findExecutable, doesFileExist )
import System.Process ( waitForProcess )
import System.Exit ( ExitCode(..), exitWith )

import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )

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
