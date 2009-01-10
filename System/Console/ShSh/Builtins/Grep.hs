{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Grep ( grep ) where

import System.Console.ShSh.IO ( oPutStr )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flag, flagOn )
import System.Console.ShSh.Builtins.Util ( readFilesOrStdinWithFilename )

import Data.Bits ( (.|.) )
import System.Console.GetOpt
import System.Exit ( ExitCode(..) )
import Text.Regex.Posix ( Regex, compNewline, compIgnoreCase )
import Text.Regex.Base ( match, makeRegexOpts, defaultExecOpt )

grep :: [String] -> Shell ExitCode
grep = withArgs "grep" header args RequireOrder grep'
    where grep' [] = fail "grep requires an argument!"
          grep' (reg:fs) =
              do ami <- flag 'i'
                 amv <- flag 'v'
                 amw <- flag 'w'
                 amn <- flag 'n'
                 amH <- (|| length fs > 1) `fmap` flag 'H'
                 let matchIt r = match (makeRegexOpts compOpt
                                        defaultExecOpt r :: Regex)
                     nonword = "[^0-9a-zA-Z]"
                     grepIt l = xor amv $
                                if not amw
                                then matchIt reg l
                                else or $ map (matchIt `flip` l)
                                          [b++reg++e | b <- ["^",nonword]
                                                     , e <- ["$",nonword]]
                     compOpt = if ami then compNewline .|. compIgnoreCase
                                      else compNewline
                     xor a = if a then not else id
                     readStuff = do xs <- readFilesOrStdinWithFilename fs
                                    return $ concatMap cleanup xs
                         where cleanup (f,x) = map cleanup' $ zip [1::Int ..] $
                                                              lines x
                                   where cleanup' (n,l) =
                                             if amn then (prefix++show n++":",l)
                                                    else (prefix,l)
                                         prefix = if amH then (f++":") else ""
                 x <- filter (grepIt . snd) `fmap` readStuff
                 if null x
                     then return $ ExitFailure 1
                     else do oPutStr $ unlines $ map (uncurry (++)) x
                             return ExitSuccess
          header = "Usage: grep [-iv] args...\n"++
                   "grep it, silly."
          args = [flagOn "i" [] 'i' "case insensitive grep",
                  flagOn "v" [] 'v' "invert match",
                  flagOn "w" [] 'w' "match on words",
                  flagOn "n" [] 'n' "line numbers",
                  flagOn "H" [] 'H' "show file names",
                  flagOn "eF" [] 'z' "flags we don't support"]
