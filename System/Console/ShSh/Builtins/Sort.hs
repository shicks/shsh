{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Sort ( runSort ) where

import System.Console.ShSh.IO ( oPutStr )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flagOn, optSet, opt )
import System.Console.ShSh.Builtins.Util ( readFilesOrStdin )

import Control.Monad.Trans ( liftIO )
import Data.List ( sort )
import System.Exit ( ExitCode(..) )

{-# NOINLINE runSort #-}
runSort :: [String] -> Shell ExitCode
runSort = withArgs "sort" header args sort'
    where sort' fs =
              do x <- readFilesOrStdin fs
                 o <- opt 'o' "-"
                 let out = if o=="-" then oPutStr else liftIO . writeFile o
                 out $ unlines $ sort $ concat $ map lines x
                 return ExitSuccess
          header = "Usage: sort [file]...\n"++
                   "sort lines"
          args = [flagOn "n" [] 'z' "flags we don't support"
                 ,optSet "o" ["output"] 'o' "FILE" "output file"]
