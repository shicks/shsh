{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.EventLoop ( eventLoop, sourceProfile )
    where

import System.Console.ShSh.Command ( runCommands, source )
-- import System.Console.ShSh.Expansions ( shellExpansions )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn, oPutStr,
#ifndef HAVE_HASKELINE
                                ePutStr, eFlush, oFlush,
                                iIsEOF, iGetLine
#endif
                              )
import System.Console.ShSh.Shell ( Shell, getAliases, getExitCode,
                                   getEnv, getFlag, withHandler )
import System.Console.ShSh.Prompt ( prompt )

import Language.Sh.Parser ( parse, hereDocsComplete )

import System.IO ( hFlush, hIsEOF, openFile, IOMode(..),
                   stdin, stdout, stderr, hGetLine, Handle )
import System.Directory ( doesFileExist )
import System.FilePath ( (</>) )
import System.Exit ( ExitCode(..) )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Data.Maybe ( fromJust, isJust, fromMaybe, isNothing )

#ifdef HAVE_HASKELINE
import System.Console.Haskeline ( runInputT, getInputLine,
                                  defaultSettings, historyFile )
#endif

sourceProfile :: Shell ()
sourceProfile = do withHandler $ do dir <- getEnv "HOME"
                                    let file = dir </> ".shshrc"
                                    exists <- liftIO $ doesFileExist file
                                    when exists $ source file >> return ()
                   return ()

{-
cases:
  1. interactive vs. non-interactive
  2. string vs. EOF
  3. null i vs. non-null i
  4. fatal vs. non-fatal parse error
  5. completed vs. incomplete heredocs
-}

eventLoop :: String -> Maybe Handle -> Shell ExitCode
eventLoop i0 h = el i0
 where el i
         = do line <- case h of Nothing -> getInput =<< prompt i
                                Just h' -> getNoninteractiveInput h'
              --eof <- case h of Nothing -> return Nothing
              --                 Just h' -> Just `fmap` hIsEOF h'
              as <- getAliases
              am_v <- getFlag 'v'
              let cmd = i ++ fromMaybe "" line
                  interactive = isNothing h
                  (result,fatal) = case parse as cmd of
                                     Left (err,b) -> (Left err,b)
                                     Right cs -> (Right cs,hereDocsComplete cs)
              case line of
                Just s  -> when am_v $ ePutStrLn s
                Nothing -> return ()
       -- (Bool,Bool,Maybe Bool,Either ([Command],Bool) (String,Bool))
              case (line,null i,interactive,result,fatal) of
                (Nothing,True ,_    ,_       ,_    ) -> getExitCode
                (Nothing,False,False,Right cs,_    ) -> runCommands cs
                (Nothing,False,False,Left err,_    ) -> fail err
                (Nothing,False,True ,Right cs,_    ) -> runCommands cs >> el ""
                (Nothing,False,True ,Left err,_    ) -> el ""
                (Just s ,_    ,_    ,_       ,False) -> el (i++s++"\n")
                (Just _ ,_    ,_    ,Right cs,True ) -> runCommands cs >> el ""
                (Just _ ,_    ,False,Left err,True ) -> fail err
                (Just _ ,_    ,True ,Left err,True ) -> ePutStrLn err >> el ""

-- What do we want to do with history?  Bash defines a $HISTFILE variable,
-- but that only deals with saving the history on exit - apparently readline
-- takes care of saving things automatically on a per-session basis.
-- We certainly don't want to clobber ~/.bash_history.  But it would be
-- good to have a variable such as $HISTFILE to deal with this...
-- Notes: we want to change how history is saved depending on
--  1. if we're continuing, we should save it as one big string
--  2. if we're reading a HereDoc, we should ignore it
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
