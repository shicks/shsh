\chapter{Builtins module}

This module exports a list of builtin commands and how we handle them.

\begin{code}

module System.Console.ShSh.Builtins ( BuiltinCommand(..), runBuiltin ) where

import Control.Monad ( forM_ )
import Data.List ( sort, sortBy )
import Data.Ord ( comparing )
import System.Console.ShSh.Builtins.Cd ( chDir )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Shell ( Shell, withHandler, getAllEnv )

import System.Directory ( getCurrentDirectory, getDirectoryContents )
import Control.Monad.Trans ( liftIO )

data BuiltinCommand = Exit | Set | Pwd | Cd | Ls
     deriving ( Enum, Eq, Ord, Show )

{- What else do we want...? list here:
  env
-}

-- This will have to change when we want to generalize the I/O...
runBuiltin :: BuiltinCommand -> [String] -> Shell Bool

runBuiltin Exit _    = return True
runBuiltin Set []    = showEnv >> return False
runBuiltin Set foo   = setOpts foo >> return False
runBuiltin Pwd _     = do liftIO getCurrentDirectory >>= liftIO . putStrLn
                          return False
runBuiltin Ls _      = do let unboring ('.':_) = False
                              unboring _ = True
                          fs <- liftIO (getDirectoryContents ".")
                          liftIO $ putStr $ unlines $ sort $ filter unboring fs
                          return False
runBuiltin Cd ss     = withHandler "cd" (chDir ss) >> return False

-- The BASH version escapes dangerous values with single-quotes, i.e.
--   spaces, parens, etc..., make the output runnable.
showEnv :: Shell ()
showEnv = do env <- getAllEnv
             forM_ (sortBy (comparing fst) env) $ \(e,v) ->
                 liftIO $ putStrLn $ e ++ "=" ++ v
               

\end{code}