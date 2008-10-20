\chapter{Command module}

Here we run commands.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Command ( process ) where

import System.Console.ShSh.Builtins ( runBuiltin )
import System.Console.ShSh.Options ( setOpts, getFlag )
import System.Console.ShSh.Parse ( parseLine, Command(..) )
import System.Console.ShSh.Pipe ( pipeOutput, pipeOutputInput, waitForPipe, Pipe )
import System.Console.ShSh.Shell ( Shell, getEnv, setEnv, getAllEnv,
                                   tryEnv, withHandler )
import System.Console.ShSh.Prompt ( prompt )
import System.Directory ( findExecutable, doesFileExist )
import System.IO ( hFlush, hIsEOF, stdin, stdout, stderr, hGetLine,
                   Handle, hPutStrLn, hClose )
import System.Process ( runInteractiveProcess, waitForProcess, ProcessHandle )
import System ( ExitCode(..) )
import Control.Monad.Trans ( liftIO )

process :: Command -> Handle -> Shell ExitCode -- do we quit or not?
process (Builtin b args) h = runBuiltin b args h
process (Cmd (s:ss)) h = tryToRun s ss h
process EmptyCommand h = do liftIO $ putStrLn ""; return ExitSuccess
process (c1 :&&: c2) h = do ec1 <- process c1 h
                            if ec1 == ExitSuccess
                               then process c2 h
                               else return ec1
process (c1 :||: c2) h = do ec1 <- process c1 h
                            if ec1 /= ExitSuccess
                               then process c2 h
                               else return ec1
#ifdef HAVE_CREATEPROCESS
process (c1 :|: (Cmd (c2:args))) h = 
    do (h',pid,pipe) <- runWithInput c2 args h
       process c1 h' -- assume c2 is a command for now...!
       liftIO $ hClose h' >> waitForPipe pipe >> waitForProcess pid
#endif

process cmd h = do liftIO $ hPutStrLn h $ "I can't handle:  "++show cmd
                   return $ ExitFailure 1

tryToRun :: String -> [String] -> Handle -> Shell ExitCode
tryToRun cmd args h = do exe <- liftIO $ findExecutable cmd -- use own path?
                         case exe of
                           Just fp -> run fp
                           Nothing -> notFound
    where notFound = do let path = '/' `elem` cmd
                        exists <- liftIO $ doesFileExist cmd
                        if path && exists
                           then fail $ cmd++": Permission denied"
                           else fail $ cmd++": No such file or directory"
          run x = do pipeOutput x args h

runWithInput :: String -> [String] -> Handle -> Shell (Handle,ProcessHandle,Pipe)
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

\end{code}