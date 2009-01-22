{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Wc ( runWc ) where

import System.Console.ShSh.IO ( oPutStrLn, iGetContents )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flagOn )

import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(..) )

{-# NOINLINE runWc #-}
runWc :: [String] -> Shell ExitCode
runWc = withArgs "wc" header args wc'
    where wc' origfs =
              do let fs = if null origfs then ["-"] else origfs
                     readf "-" = lines `fmap` iGetContents
                     readf f = lines `fmap` liftIO (readFile f)
                 x <- mapM readf fs
                 oPutStrLn $ show $ length $ concat x
                 return ExitSuccess
          header = "Usage: wc [OPTION...] [FILE...]\n"++
                   "wc lines"
          args = [flagOn "l" [] 'l' "count number of lines"]
