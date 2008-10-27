\chapter{Command module}

Here we run commands.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Command ( process ) where

import System.Console.ShSh.Builtins ( runBuiltin )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Parse ( parseLine, Command(..) )
import System.Console.ShSh.Pipe ( runInShell, pipeShells, waitForPipes )
import System.Console.ShSh.PipeIO ( shPutStrLn )
import System.Console.ShSh.Shell ( Shell, sh_out,
                                   getEnv, setEnv, getAllEnv, withExitHandler,
                                   tryEnv, withEnv, getFlag, unsetFlag )
import System.Console.ShSh.Prompt ( prompt )
import System.Directory ( findExecutable, doesFileExist )
import System.Process ( proc, waitForProcess )
import System ( ExitCode(..), exitWith )
import Control.Monad.Trans ( liftIO )

process :: Command -> Shell ExitCode -- do we quit or not?
process (Builtin b args) = runBuiltin b args
process (Cmd (s:ss)) = withExitHandler $ tryToRun s ss
process EmptyCommand = do liftIO $ putStrLn ""; return ExitSuccess
process (c1 :&&: c2) = do ec1 <- process c1
                          if ec1 == ExitSuccess
                             then process c2
                             else return ec1
process (c1 :||: c2) = do ec1 <- process c1
                          if ec1 /= ExitSuccess
                             then process c2
                             else return ec1
process (c1 :>>: c2) = do am_e <- getFlag 'e'
                          ec1 <- process c1
                          if am_e && ec1 /= ExitSuccess
                             then liftIO $ exitWith ec1
                             else process c2
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
process (c1 :|: c2) =  -- pipeShells rethrows from c2...
    pipeShells (process c1) (process c2)
-- #endif
process cmd = do h <- sh_out
                 liftIO $ shPutStrLn h $ "I can't handle:  "++show cmd
                 return $ ExitFailure 1

tryToRun :: String -> [String] -> Shell ExitCode
tryToRun cmd args = do exe <- liftIO $ findExecutable cmd -- use own path?
                       case exe of
                         Just fp -> run fp
                         Nothing -> notFound
    where notFound = do let path = '/' `elem` cmd
                        exists <- liftIO $ doesFileExist cmd
                        if path && exists
                           then fail $ cmd++": Permission denied"
                           else fail $ cmd++": No such file or directory"
          run x = do (_,_,_,pid) <- runInShell $ proc x args
                     liftIO $ waitForProcess pid

{-
runWithInput :: String -> [String] -> Handle -> Shell (Handle,ProcessHandle,[Pipe])
runWithInput cmd args h = do exe <- liftIO $ findExecutable cmd -- use own path?
                             case exe of
                               Just fp -> run fp
                               Nothing -> notFound
    where notFound = do let path = '/' `elem` cmd
                        exists <- liftIO $ doesFileExist cmd
                        if path && exists
                           then fail $ cmd++": Permission denied"
                           else fail $ cmd++": No such file or directory"
          run x = do pipeOutputInput x args h
-}

\end{code}
