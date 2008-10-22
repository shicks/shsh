\chapter{EventLoop module}

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.EventLoop ( eventLoop )
    where

import System.Console.ShSh.Command ( process )
import System.Console.ShSh.Expansions ( shellExpansions )
import System.Console.ShSh.Parse ( parseLine, Command(..) )
import System.Console.ShSh.Shell ( Shell, getEnv, getFlag )
import System.Console.ShSh.Prompt ( prompt )
import System.IO ( hFlush, hIsEOF, stdin, stdout, stderr, hGetLine, Handle )
import System ( ExitCode(..) )
import Control.Monad.Trans ( liftIO )

#ifdef HAVE_HASKELINE
import System.Console.Haskeline ( runInputT, getInputLine,
                                  defaultSettings, historyFile )
#endif

eventLoop :: Maybe Handle -> Shell ()
eventLoop h = do
  s' <- case h of
          Nothing -> getInput =<< prompt
          Just h' -> getNoninteractiveInput h'
  case s' of
    Nothing -> return ()
    Just s  -> do am_e <- getFlag 'e'
                  am_v <- getFlag 'v'
                  if am_v then liftIO $ putStrLn s
                          else return ()
                  s' <- shellExpansions s
                  code <- case parseLine s' of -- Later, add more to s' (PS2)
                            Left e -> do liftIO $ putStrLn e
                                         return $ ExitFailure 1 -- ????
                            Right cmd -> process cmd stdout
                  if am_e && code /= ExitSuccess
                    then fail ""
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
