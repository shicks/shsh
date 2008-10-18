\chapter{EventLoop module}

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.EventLoop ( eventLoop )
    where

import System.Console.ShSh.Builtins ( runBuiltin )
import System.Console.ShSh.Options ( setOpts, getFlag )
import System.Console.ShSh.Parse ( parseLine, Command(..) )
import System.Console.ShSh.Shell ( Shell, getEnv, setEnv, getAllEnv,
                                   tryEnv, withHandler )
import System.Console.ShSh.Prompt ( prompt )
import System.Directory ( findExecutable, doesFileExist )
import System.IO ( hFlush, hIsEOF, stdin, stdout, stderr, hGetLine, Handle )
import System.Process ( runProcess, waitForProcess )
import System ( ExitCode(..) )
import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )

#ifdef HAVE_HASKELINE
import System.Console.Haskeline ( runInputT, getInputLine,
                                  defaultSettings, historyFile )
#endif

process :: Command -> Shell Bool -- do we quit or not?
process (Builtin b args) = runBuiltin b args
process (Cmd (s:ss)) = withHandler "" (tryToRun s ss) >> return False
process EmptyCommand = do liftIO $ putStrLn ""; return False
process (c1 :&&: c2) = do mok <- withHandler "" $ process c1 -- buggy
                          case mok of
                            Just True -> return True
                            Just False -> process c2
                            Nothing -> return False

process cmd = do liftIO $ putStrLn $ "I can't handle:  "++show cmd
                 return False

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

eventLoop :: Maybe Handle -> Shell ()
eventLoop h = do s' <- case h of
                         Nothing -> getInput =<< prompt
                         Just h' -> getNoninteractiveInput h'
                 case s' of
                   Nothing -> return ()
                   Just s  -> do am_v <- getFlag 'v'
                                 if am_v then liftIO $ putStrLn s
                                         else return ()
                                 code <- case parseLine s of
                                           Left e -> do liftIO $ putStrLn e
                                                        return False -- ???
                                           Right cmd -> process cmd
                                 if code
                                   then liftIO $ putStrLn "" -- exit
                                   else eventLoop h

-- What do we want to do with history?  Bash defines a $HISTFILE variable,
-- but that only deals with saving the history on exit - apparently readline
-- takes care of saving things automatically on a per-session basis.
-- We certainly don't want to clobber ~/.bash_history.  But it would be
-- good to have a variable such as $HISTFILE to deal with this...
getInput :: String -> Shell (Maybe String)
#ifdef HAVE_HASKELINE
getInput prompt =
    do mhome <- getEnv "HOME"
       liftIO $ runInputT (settings mhome) (getInputLine prompt)
    where settings (Just home) = defaultSettings
                                 { historyFile = Just $ home++"/.shsh_history" }
          settings Nothing     = defaultSettings
#else
getInput prompt = liftIO $ do putStr prompt
                              hFlush stdout
                              eof <- hIsEOF stdin
                              if eof
                                then putStrLn "" >> return Nothing -- exit
                                else Just `fmap` getLine
#endif

getNoninteractiveInput :: Handle -> Shell (Maybe String)
getNoninteractiveInput h = liftIO $ do eof <- hIsEOF h
                                       if eof then return Nothing
                                              else Just `fmap` hGetLine h

\end{code}
