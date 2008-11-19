\chapter{Command module}

Here we run commands.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Command ( runCommand ) where

import System.Console.ShSh.Builtins ( runBuiltin )
import System.Console.ShSh.Expansions ( expandWords, expandWord )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn )
import System.Console.ShSh.Parse.AST ( Command(..), AndOrList(..),
                                       Pipeline(..), Statement(..) )
import System.Console.ShSh.ShellError ( announceError )
import System.Console.ShSh.Shell ( Shell, pipeShells, runInShell, withOutRedirected,
                                   getEnv, setEnv, getAllEnv, withExitHandler,
                                   tryEnv, withEnv, getFlag, unsetFlag )
import System.Console.ShSh.Prompt ( prompt )
import System.Console.ShSh.Redirection ( Redir(..) )
import System.Directory ( findExecutable, doesFileExist )
import System.Process ( waitForProcess )
import System.Exit ( ExitCode(..), exitWith )

import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )

-- |Simply run a 'Command'.
runCommand :: Command -> Shell ExitCode
runCommand (Synchronous list) = withExitHandler $ runList True list
runCommand (Asynchronous list) = do runAsync $ withExitHandler $
                                             runList False list
                                    return ExitSuccess
runCommand c = fail $ "Command "++show c++" not yet supported."

runAsync :: Shell a -> Shell ()
runAsync _ = fail "Asyncronous commands not yet supported"

-- |Run an 'AndOrList'.  We pass a @Bool@ for whether to check the @-e@
-- option to exit on a nonzero exit code.
runList :: Bool -> AndOrList -> Shell ExitCode
runList b (Singleton p) = runPipeline b p
runList b (l :&&: p)    = do ec <- runList False l
                             if ec == ExitSuccess
                                then runPipeline b p
                                else return ec
runList b (l :||: p)    = do ec <- runList False l
                             if ec /= ExitSuccess
                                then runPipeline b p
                                else return ec

process' :: Expression -> Shell ExitCode -- do we quit or not?
process' (Builtin b args r) = runBuiltin b args r
process' (Cmd s ss _) = withExitHandler $ tryToRun s ss
process' (c1 :&&: c2) = do ec1 <- process' c1
                           if ec1 == ExitSuccess
                              then process' c2
                              else return ec1
process' (c1 :||: c2) = do ec1 <- process' c1
                           if ec1 /= ExitSuccess
                              then process' c2
                              else return ec1
process' (c1 :>>: c2) = do am_e <- getFlag 'e'
                           ec1 <- process' c1
                           if am_e && ec1 /= ExitSuccess
                              then liftIO $ exitWith ec1
                              else process' c2
-- This isn't quite right yet.  In real sh, the PARENS guard from
-- the effects of -e.  That is,
--   $ set -e
--   $ (false)   # doesn't exit
--   $ false     # exits
-- We want to replicate this behavior somehow...?  Maybe just
-- keep track of the nesting depth in process...?  This also means
-- that we can't just have the check happen in the EventLoop... it
-- needs to happen at "statement terminators" like ;, &, and EOL...?
-- Check the spec.
-- #ifdef HAVE_CREATEPROCESS
{-
process (c1 :|: (Cmd (c2:args))) h = 
    do (h',pid,pipes) <- runWithInput c2 args h
       process c1 h' -- assume c2 is a command for now...!
       liftIO $ hClose h' >> waitForPipes pipes >> waitForProcess pid
-}
process' (c1 :|: c2) =  -- pipeShells rethrows from c2...
    pipeShells (process' c1) (process' c2)
-- #endif
process' cmd = do oPutStrLn $ "I can't handle:  "++show cmd
                  return $ ExitFailure 1

run :: Statement -> Shell ExitCode
run (Builtin b args rs as) = do args' <- expandWords args
                                runBuiltin b args' rs as
run (Statement ws [] []) = do ws' <- expandWords ws
                              if null ws' then return ExitSuccess
                                          else runWithArgs (head ws') $ tail ws'
run (Statement ws rs as) = withEnvironment expandWord rs as $
                             run $ Statement ws [] []
run (Subshell _ _) = fail "Subshells not supported yet."

runWithArgs :: String -> [String] -> Shell ExitCode
runWithArgs cmd args = do exe <- liftIO $ findExecutable cmd -- use own path?
                          case exe of
                            Just fp -> run fp
                            Nothing -> notFound
    where notFound = do let path = '/' `elem` cmd
                        exists <- liftIO $ doesFileExist cmd
                        if path && exists
                           then fail $ cmd++": Permission denied"
                           else fail $ cmd++": No such file or directory"
          run x = do (_,_,_,pid) <- runInShell x args
                     liftIO $ waitForProcess pid

\end{code}
