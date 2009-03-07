{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Head ( runHead ) where

import System.Console.ShSh.IO ( oPutStr )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flag, flagOn, opt, optSet )
import System.Console.ShSh.Builtins.Util ( readFilesOrStdinWithFilename )

import Control.Monad ( when )
import System.Exit ( ExitCode(..) )

{-# NOINLINE runHead #-}
runHead :: [String] -> Shell ExitCode
runHead xxx = withArgs "head" header args head' xxx'
    where head' [] = head' ["-"]
          head' fs =
              do optQuiet  <- flag 'q'
                 optVerb <- flag 'v'
                 nnum <- read `fmap` opt 'n' (show nlines)
                 let headIt (f,c) =
                         do when (not optQuiet && (length fs > 1 || optVerb) ) $
                                 oPutStr $ "==> "++f++" <==\n"
                            oPutStr $ unlines $ take nnum $ lines c
                 xs <- readFilesOrStdinWithFilename fs
                 mapM_ headIt xs
                 return ExitSuccess
          header = "Usage: head [options...] args...\n"++
                   "pattern match on lines"
          args = [optSet "n" ["lines"] 'n' "N"
                             "print the first N lines of each file",
                  flagOn "q" ["quiet","silent"] 'q'
                             "never print headers giving file names",
                  flagOn "v" ["verbose"] 'v'
                             "always print headers giving file names"]
          xxx' = filter (not . numeric) xxx
          numeric ('-':s) = all (`elem` ['0'..'9']) s
          numeric _ = False
          nlines = findnlines $ filter numeric xxx
              where findnlines (('-':s):_) = read s
                    findnlines _ = 10 :: Int
