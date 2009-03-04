{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Which ( which ) where

import System.Console.ShSh.IO ( oPutStrLn )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Path ( findExecutable )

import System.Exit ( ExitCode(..) )

{-# NOINLINE which #-}
which :: [String] -> Shell ExitCode
which cmds = do mapM_ (\cmd -> findExecutable cmd >>= oPutStrLn) cmds
                return ExitSuccess
