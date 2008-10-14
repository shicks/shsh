\chapter{EventLoop module}

\begin{code}

module EventLoop ( eventLoop )
    where

import Shell ( Shell, getEnv, getAllEnv )
import Prompt ( prompt )
import System.Directory ( setCurrentDirectory, findExecutable, doesFileExist )
import System.IO ( hFlush, stdin, stdout, stderr )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.Process ( runProcess, waitForProcess )
import Control.Monad.Trans ( liftIO )

-- we should STRIP SPACES before and after
process :: [String] -> Shell Bool -- do we quit or not?
process ["exit"] = return True
process (s:ss) | s == "cd " = chDir ss >> return False
               | otherwise  = tryToRun s ss >> return False
process [] = return False

chDir :: [String] -> Shell ()
chDir (dir:_) = liftIO $ setCurrentDirectory dir
chDir []      = do Just home <- getEnv "HOME"
                   chDir [home]

tryToRun :: String -> [String] -> Shell ()
tryToRun cmd args = do exe <- liftIO $ findExecutable cmd -- use own path?
                       case exe of
                         Just fp -> run fp
                         Nothing -> notFound
    where notFound = do let path = '/' `elem` cmd
                        exists <- liftIO $ doesFileExist cmd
                        if path && exists
                           then err $ cmd++": Permission denied"
                           else err $ cmd++": No such file or directory"
          run x = do env <- getAllEnv
                     pid <- liftIO $ runProcess x args Nothing (Just env)
                                     Nothing Nothing Nothing
                     liftIO $ waitForProcess pid
                     return ()

err :: String -> Shell ()
err s = liftIO $ putStrLn $ "shsh: "++s

eventLoop :: Shell ()
eventLoop = do p <- prompt
               liftIO $ putStr p >> hFlush stdout
               s <- liftIO getLine -- use Haskeline eventually
               code <- process $ words s -- Will need to be smarter: ", \, etc
               if code then return () else eventLoop

\end{code}