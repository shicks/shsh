{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Seq ( runSeq ) where

import System.Console.ShSh.IO ( oPutStr )
import System.Console.ShSh.Shell ( Shell )

import System.Exit ( ExitCode(..) )

{-# NOINLINE runSeq #-}
runSeq :: [String] -> Shell ExitCode
runSeq args = do ns <- case args of
                    [l] -> return [1 :: Int .. read l]
                    [f,l] -> return [read f .. read l]
                    [f,d,l] -> return [read f, read f + read d .. read l]
                    _ -> fail "bad arguments!\nusage: seq [FIRST [INCREMENT]] LAST"
                 oPutStr $ unlines $ map show ns
                 return ExitSuccess
