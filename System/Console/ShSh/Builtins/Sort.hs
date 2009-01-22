{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Sort ( runSort ) where

import System.Console.ShSh.IO ( oPutStr )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flagOn )
import System.Console.ShSh.Builtins.Util ( readFilesOrStdin )

import Data.List ( sort )
import System.Exit ( ExitCode(..) )

{-# NOINLINE runSort #-}
runSort :: [String] -> Shell ExitCode
runSort = withArgs "sort" header args sort'
    where sort' fs =
              do x <- readFilesOrStdin fs
                 oPutStr $ unlines $ sort $ concat $ map lines x
                 return ExitSuccess
          header = "Usage: sort [file]...\n"++
                   "sort lines"
          args = [flagOn "n" [] 'z' "flags we don't support"]
