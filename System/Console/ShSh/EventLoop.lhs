\chapter{EventLoop module}

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.EventLoop ( eventLoop, sourceProfile, source )
    where

import System.Console.ShSh.Command ( runCommand )
-- import System.Console.ShSh.Expansions ( shellExpansions )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn, oPutStr,
#ifndef HAVE_HASKELINE
                                oFlush, iIsEOF, iGetLine
#endif
                              )
import System.Console.ShSh.Parse ( parse )
import System.Console.ShSh.Shell ( Shell, getAliases,
                                   getEnv, getFlag, withHandler )
import System.Console.ShSh.Prompt ( prompt )
import System.IO ( hFlush, hIsEOF, openFile, IOMode(..),
                   stdin, stdout, stderr, hGetLine, Handle )
import System.Directory ( doesFileExist )
import System.FilePath ( (</>) )
import System.Exit ( ExitCode(..) )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Data.Maybe ( fromJust, isJust )

#ifdef HAVE_HASKELINE
import System.Console.Haskeline ( runInputT, getInputLine,
                                  defaultSettings, historyFile )
#endif

source :: FilePath -> Shell ()
source f = do h <- liftIO $ openFile f ReadMode
              eventLoop "" $ Just h

sourceProfile :: Shell ExitCode
sourceProfile = withHandler $ do dir <- getEnv "HOME"
                                 let file = dir </> ".shshrc"
                                 exists <- liftIO $ doesFileExist file
                                 when exists $ ePutStrLn ("Sourcing "++file) >>
                                               source file

eventLoop :: String -> Maybe Handle -> Shell ()
eventLoop i h = do
  s' <- case h of
          Nothing -> getInput =<< prompt i
          Just h' -> getNoninteractiveInput h'
  case s' of
    Nothing -> return ()
    Just s  -> do am_e <- getFlag 'e'
                  am_v <- getFlag 'v'
                  if am_v then ePutStrLn s
                          else return ()
                  as <- getAliases
                  case parse as (i++s) of -- Later, add more to s' (PS2)
                    Left err -> do when (isJust h) $ do
                                     eof <- liftIO $ hIsEOF $ fromJust h
                                     when eof $ fail $ show err
                                   eventLoop (i++s++"\n") h
                    Right cmd -> mapM_ runCommand cmd >> eventLoop "" h

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
getInput prompt = do oPutStr prompt
                     oFlush
                     eof <- iIsEOF
                     if eof then oPutStrLn "" >> return Nothing
                            else Just `fmap` iGetLine
#endif

-- I feel like we still need this for reading from script files,
-- although maybe it's not quite as it should be, since it looks
-- like this getds called with stdin as its arg when we're in
-- non-interactive mode, and in that case, it's possibly not right.
getNoninteractiveInput :: Handle -> Shell (Maybe String)
getNoninteractiveInput h = liftIO $ do eof <- hIsEOF h
                                       if eof then return Nothing
                                              else Just `fmap` hGetLine h

\end{code}
