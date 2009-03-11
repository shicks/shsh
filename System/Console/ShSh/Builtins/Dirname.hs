{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Dirname ( dirname ) where

import System.Console.ShSh.IO ( oPutStrLn )
import System.Console.ShSh.Shell ( Shell )

import System.Exit ( ExitCode(..) )
import System.FilePath ( takeDirectory )

{-# NOINLINE dirname #-}
dirname :: [String] -> Shell ExitCode
dirname [f] = do case takeDirectory f of
                   "" -> oPutStrLn "."
                   d -> oPutStrLn d
                 return ExitSuccess
dirname [] = fail "missing operand"
dirname (_:b:_) = fail $ "extra operand `"++b++"'"
