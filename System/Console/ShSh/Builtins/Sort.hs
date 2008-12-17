{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Sort ( runSort ) where

import System.Console.ShSh.IO ( oPutStr, iGetContents )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flagOn )

import Control.Monad.Trans ( liftIO )
import Data.List ( sort )
import System.Exit ( ExitCode(..) )
import System.Console.GetOpt

runSort :: [String] -> Shell ExitCode
runSort = withArgs "sort" header args RequireOrder sort'
    where sort' origfs =
              do let fs = if null origfs then ["-"] else origfs
                     readf "-" = lines `fmap` iGetContents
                     readf f = lines `fmap` liftIO (readFile f)
                 x <- mapM readf fs
                 oPutStr $ unlines $ sort $ concat x
                 return ExitSuccess
          header = "Usage: sort [file]...\n"++
                   "sort lines"
          args = [flagOn "n" [] 'z' "flags we don't support"]
