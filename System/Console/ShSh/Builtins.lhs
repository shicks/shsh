\chapter{Builtins module}

This module exports a list of builtin commands and how we handle them.

\begin{code}

module System.Console.ShSh.Builtins ( BuiltinCommand(..), runBuiltin ) where

import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( forM_ )
import Data.List ( sort, sortBy )
import Data.Ord ( comparing )
import System.Console.ShSh.Builtins.Cd ( chDir )
import System.Console.ShSh.Builtins.Mkdir ( mkDir )
import System.Console.ShSh.Builtins.Exit ( exit )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Shell ( Shell, withHandler, getAllEnv )
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( Handle, hPutStr, hPutStrLn )
import Control.Monad.Trans ( liftIO )

data BuiltinCommand = Exit | Set | Pwd | Cd | Ls | MkDir
     deriving ( Enum, Eq, Ord, Show )

{- What else do we want...? list here:
  env
-}

-- This will have to change when we want to generalize the I/O...
-- The handle parameter is an input handle for 
runBuiltin :: BuiltinCommand -> [String] -> Handle -> Shell ExitCode

runBuiltin Exit _ _   = liftIO $ exitWith ExitSuccess -- message?
runBuiltin Set [] h   = showEnv h >> return ExitSuccess
runBuiltin Set foo h  = setOpts h foo
runBuiltin Pwd _ h    = do liftIO getCurrentDirectory >>= liftIO . hPutStrLn h
                           return ExitSuccess
runBuiltin Ls _ h     = do let unboring ('.':_) = False
                               unboring _ = True
                           fs <- liftIO (getDirectoryContents ".")
                           liftIO $ hPutStr h $ unlines $ sort $
                                    filter unboring fs
                           return ExitSuccess
runBuiltin Cd ss _    = withHandler "cd" (chDir ss) >> return ExitSuccess
runBuiltin MkDir ss _ = mkDir ss

-- The BASH version escapes dangerous values with single-quotes, i.e.
--   spaces, parens, etc..., make the output runnable.
showEnv :: Handle -> Shell ()
showEnv h = do env <- getAllEnv
               forM_ (sortBy (comparing fst) env) $ \(e,v) ->
                   liftIO $ hPutStrLn h $ e ++ "=" ++ v

\end{code}