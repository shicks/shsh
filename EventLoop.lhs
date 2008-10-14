\chapter{EventLoop module}

\begin{code}

module EventLoop ( eventLoop )
    where

import Shell ( Shell, getEnv, setEnv, getAllEnv )
import Prompt ( prompt )
import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          getDirectoryContents,
                          findExecutable, doesFileExist, doesDirectoryExist )
import List ( sort )
import System.IO ( hFlush, hIsEOF, stdin, stdout, stderr )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.Process ( runProcess, waitForProcess )
import Control.Monad.Trans ( liftIO )

-- we should STRIP SPACES before and after
process :: [String] -> Shell Bool -- do we quit or not?
process ["exit"] = return True
process ["pwd"] = do liftIO getCurrentDirectory >>= liftIO . putStrLn
                     return False
process ["ls"] = do let unboring ('.':_) = False
                        unboring _ = True
                    liftIO (getDirectoryContents ".") >>=
                           liftIO . putStr . unlines . sort . filter unboring
                    return False
process (s:ss) | s == "cd" = chDir ss >> return False
               | otherwise = tryToRun s ss >> return False
process [] = return False

chDir :: [String] -> Shell ()
chDir ("-":_) = do Just dir <- getEnv "OLDPWD"
                   chDir' dir
chDir (dir:_) = do chDir' dir
chDir []      = do Just home <- getEnv "HOME"
                   chDir [home]

chDir' dir = do exists <- liftIO $ doesDirectoryExist dir
                if exists
                   then go
                   else err $ "cd: "++dir++": No such file or directory"
    where go = do olddir <- liftIO $ getCurrentDirectory
                  liftIO $ setCurrentDirectory dir
                  newdir <- liftIO $ getCurrentDirectory
                  setEnv "PWD" newdir
                  setEnv "OLDPWD" olddir

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
               eof <- liftIO $ hIsEOF stdin
               if eof then liftIO $ putStrLn "" else continue
    where continue = do s <- liftIO getLine -- use Haskeline eventually
                        code <- process $ words s -- Needs smarter: ", \, etc
                        if code then liftIO $ putStrLn "" else eventLoop

\end{code}