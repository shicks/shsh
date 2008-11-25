{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.EventLoop ( eventLoop, sourceProfile )
    where

import System.Console.ShSh.Command ( runCommands, source )
-- import System.Console.ShSh.Expansions ( shellExpansions )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn, oPutStr,
#ifndef HAVE_HASKELINE
                                oFlush, iIsEOF, iGetLine
#endif
                              )
import System.Console.ShSh.Shell ( Shell, getAliases, getExitCode,
                                   getEnv, getFlag, withHandler )
import System.Console.ShSh.Prompt ( prompt )

import Language.Sh.Parser ( parse )

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

sourceProfile :: Shell ()
sourceProfile = do withHandler $ do dir <- getEnv "HOME"
                                    let file = dir </> ".shshrc"
                                    exists <- liftIO $ doesFileExist file
                                    when exists $ ePutStrLn ("Sourcing "++file) >>
                                         source file >> return ()
                   return ()

eventLoop :: String -> Maybe Handle -> Shell ExitCode
eventLoop i0 h = el i0
    where el i = do        -- we save exit code in case EOT sent later.
             s' <- case h of
                     Nothing -> getInput =<< prompt i
                     Just h' -> getNoninteractiveInput h'
             case s' of
               Nothing -> getExitCode
               Just s  -> do
                  am_v <- getFlag 'v'
                  when am_v $ ePutStrLn s
                  as <- getAliases
                  case parse as (i++s) of -- Later, add more to s' (PS2)
                    Left (err,False) -> do when (isJust h) $ do
                                             eof <- liftIO $ hIsEOF $ fromJust h
                                             when eof $ fail err
                                           el $ i++s++"\n"
                    Left (err,True) -> do when (isJust h) $ do
                                            eof <- liftIO $ hIsEOF $ fromJust h
                                            ePutStrLn err
                                            when eof $ fail err
                                          el ""
                    Right cs -> runCommands cs >> el ""

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
getInput prompt = do ePutStr prompt
                     eFlush
                     eof <- iIsEOF
                     if eof then ePutStrLn "" >> return Nothing
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
