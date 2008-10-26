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
import System.Console.ShSh.PipeIO ( shPutStrLn, shPutStr )
import System.Console.ShSh.Shell ( Shell, withHandler, getAllEnv, sh_out )
import System.Console.ShSh.ShellError ( withPrefix )
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( Handle, hPutStr, hPutStrLn )
import Control.Monad.Trans ( liftIO )

data BuiltinCommand = Exit | Set | Pwd | Cd | Ls | MkDir
     deriving ( Enum, Eq, Ord, Show )

{- What else do we want...? list here:
  rm
  env
-}

-- This will have to change when we want to generalize the I/O...
-- The handle parameter is an input handle for 
runBuiltin :: BuiltinCommand -> [String] -> Shell ExitCode

runBuiltin Exit _   = liftIO $ exitWith ExitSuccess -- message?
runBuiltin Set []   = showEnv >> return ExitSuccess
runBuiltin Set foo  = setOpts foo
runBuiltin Pwd _    = do cwd <- liftIO getCurrentDirectory
                         h <- sh_out
                         liftIO $ shPutStrLn h cwd
                         return ExitSuccess
runBuiltin Ls _     = do let unboring ('.':_) = False
                             unboring _ = True
                         fs <- liftIO (getDirectoryContents ".")
                         h <- sh_out
                         liftIO $ shPutStr h $ unlines $ sort $
                                  filter unboring fs
                         return ExitSuccess
runBuiltin Cd ss    = withHandler $ withPrefix "cd" $ chDir ss
runBuiltin MkDir ss = withHandler $ mkDir ss

-- The BASH version escapes dangerous values with single-quotes, i.e.
--   spaces, parens, etc..., make the output runnable.
showEnv :: Shell ()
showEnv = do h <- sh_out
             env <- getAllEnv
             forM_ (sortBy (comparing fst) env) $ \(e,v) ->
                 liftIO $ shPutStrLn h $ e ++ "=" ++ v

\end{code}