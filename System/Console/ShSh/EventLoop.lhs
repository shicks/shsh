\chapter{EventLoop module}

\begin{code}

module System.Console.ShSh.EventLoop ( eventLoop )
    where


import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Parse ( parseLine, Command(..) )
import System.Console.ShSh.Shell ( Shell, getEnv, setEnv, getAllEnv,
                                   tryEnv, withHandler, setFlag, getFlag )
import System.Console.ShSh.Prompt ( prompt )
import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          getDirectoryContents,
                          findExecutable, doesFileExist, doesDirectoryExist )
import List ( sort )
import System.IO ( hFlush, hIsEOF, stdin, stdout, stderr )
import System.Process ( runProcess, waitForProcess )
import System ( ExitCode(..) )
import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )

process :: Command -> Shell Bool -- do we quit or not?
process (Builtin "set" []) = showEnv >> return False
process (Builtin "set" foo) = setOpts foo >> return False
process (Builtin "exit" _) = return True
process (Builtin "pwd" _) = do liftIO getCurrentDirectory >>= liftIO . putStrLn
                               return False
process (Builtin "ls" _) = do let unboring ('.':_) = False
                                  unboring _ = True
                              fs <- liftIO (getDirectoryContents ".")
                              liftIO $ putStr $ unlines $ sort $ filter unboring fs
                              return False
process (Builtin "cd" ss) = withHandler "cd" (chDir ss) >> return False
process (Cmd (s:ss)) = withHandler "" (tryToRun s ss) >> return False
process EmptyCommand = do liftIO $ putStrLn ""; return False
process (c1 :&&: c2) = do mok <- withHandler "" $ process c1 -- buggy
                          case mok of
                            Just True -> return True
                            Just False -> process c2
                            Nothing -> return False

process cmd = do liftIO $ putStrLn $ "I can't handle:  "++show cmd
                 return False

showEnv :: Shell ()
showEnv = liftIO $ putStrLn "Not yet implemented" -- not yet implemented




chDir :: [String] -> Shell ()
chDir ("-":_) = do dir <- tryEnv "OLDPWD"
                   chDir' dir
chDir (dir:_) = do chDir' dir
chDir []      = do home <- tryEnv "HOME"
                   chDir' home

chDir' :: String -> Shell ()
chDir' dir = do exists <- liftIO $ doesDirectoryExist dir
                if exists
                   then go
                   else fail $ dir++": No such file or directory"
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
                           then fail $ cmd++": Permission denied"
                           else fail $ cmd++": No such file or directory"
          run x = do env <- getAllEnv
                     pid <- liftIO $ runProcess x args Nothing (Just env)
                                     Nothing Nothing Nothing
                     ec <- liftIO (waitForProcess pid)
                     case ec of
                       ExitSuccess -> return ()
                       ExitFailure ef -> fail ""

eventLoop :: Shell ()
eventLoop = do p <- prompt
               liftIO $ putStr p >> hFlush stdout
               eof <- liftIO $ hIsEOF stdin
               if eof then liftIO $ putStrLn "" else continue
    where continue = do s <- liftIO getLine -- use Haskeline eventually
                        code <- case parseLine s of
                                Left e -> do liftIO $ putStrLn e
                                             return False -- ???
                                Right cmd -> process cmd
                        if code then liftIO $ putStrLn "" else eventLoop

\end{code}
