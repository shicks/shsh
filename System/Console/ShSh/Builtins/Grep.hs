{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Grep ( grep ) where

import System.Console.ShSh.IO ( oPutStr, iGetContents )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flag, flagOn )

import Control.Monad.Trans ( liftIO )
import Data.Bits ( (.|.) )
import System.Console.GetOpt
import System.Exit ( ExitCode(..) )
import Text.Regex.Posix ( match, Regex,
                          makeRegexOpts, compNewline, defaultExecOpt,
                          compIgnoreCase )

grep :: [String] -> Shell ExitCode
grep = withArgs "grep" header args RequireOrder $ \ws -> do
          ami <- flag 'i'
          amv <- flag 'v'
          let grepit reg = xor amv . match (makeRegexOpts compOpt
                                            defaultExecOpt reg :: Regex)
              compOpt = if ami then compNewline .|. compIgnoreCase
                               else compNewline
              xor a = if a then not else id
          case ws of
            [] -> fail "grep requires an argument!"
            [p] -> do x <- iGetContents
                      case filter (grepit p) $ lines x of
                        [] -> return $ ExitFailure 1
                        ls -> do oPutStr $ unlines ls
                                 return ExitSuccess
            (p:fs) -> do x <- mapM (liftIO . readFile) fs
                         let fm = zip fs $ map (filter (grepit p) . lines) x
                             pretty (f,ls) = if length fs > 1
                                             then map ((f++":")++) ls
                                             else ls
                         if null $ concatMap snd fm
                           then return $ ExitFailure 1
                           else do oPutStr $ unlines $ concatMap pretty fm
                                   return ExitSuccess
    where header = "Usage: grep [-iv] args...\n"++
                   "grep it, silly."
          args = [flagOn "i" [] 'i' "case insensitive grep",
                  flagOn "v" [] 'v' "invert match",
                  flagOn "HenwF" [] 'z' "flags we don't support"]
