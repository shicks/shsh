{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Grep ( grep ) where

import System.Console.ShSh.IO ( oPutStr )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flag, flagOn )
import System.Console.ShSh.Builtins.Util ( readFilesOrStdinWithFilename )

import Control.Monad ( unless )
import Data.Maybe ( isJust )
import System.Exit ( ExitCode(..) )
import Text.Regex.PCRE.Light.Char8 ( match, compile, caseless )

{-# NOINLINE grep #-}
grep :: [String] -> Shell ExitCode
grep = withArgs "grep" header args grep'
    where grep' [] = fail "grep requires an argument!"
          grep' (reg:fs) =
              do optNoCase <- flag 'i'
                 optInvert <- flag 'v'
                 optWord   <- flag 'w'
                 optLine   <- flag 'x'
                 optQuiet  <- flag 'q' -- yay laziness!
                 optNums   <- flag 'n'
                 optNames  <- do bigH <- flag 'H'
                                 smallh <- flag 'h'
                                 return $ (length fs>1 || bigH) && not smallh
                 let matchIt r l = isJust $ match (compile r compOpt) l []
                     matchWord r l = or $ map (matchIt `flip` l)
                                          [b++r++e | b <- ["^",nonword]
                                                   , e <- ["$",nonword]]
                         where nonword = "[^0-9a-zA-Z]"
                     asLine r = if optLine then '^':r++"$" else r
                     grepIt l = xor optInvert $
                                if optWord then matchWord reg l
                                           else matchIt (asLine reg) l
                     compOpt = if optNoCase then [caseless] else []
                     xor a = if a then not else id
                     readStuff = do xs <- readFilesOrStdinWithFilename fs
                                    return $ concatMap cleanup xs
                         where cleanup (f,x) = map cleanup' $ zip [1::Int ..] $
                                                              lines x
                                 where cleanup' (n,l) =
                                         if optNums
                                            then (prefix++show n++":",l)
                                            else (prefix,l)
                                       prefix = if optNames then (f++":")
                                                            else ""
                 x <- filter (grepIt . snd) `fmap` readStuff
                 if null x
                     then return $ ExitFailure 1
                     else do unless optQuiet $ oPutStr $ unlines $
                                               map (uncurry (++)) x
                             return ExitSuccess
          header = "Usage: grep [options...] args...\n"++
                   "pattern match on lines"
          args = [flagOn "i" ["ignore-case"]   'i' "case insensitive grep",
                  flagOn "v" ["invert-match"]  'v' "invert match",
                  flagOn "w" ["word-regexp"]   'w' "match on words",
                  flagOn "x" ["line-regexp"]   'x' "match on lines",
                  flagOn "n" ["line-number"]   'n' "line numbers",
                  flagOn "H" ["with-filename"] 'H' "show file names",
                  flagOn "h" ["no-filename"]   'h' "don't show file names",
                  flagOn "q" ["quiet","silent"] 'q' "don't write to stdout",
                  flagOn "eF" [] 'z' "flags we don't support"]
