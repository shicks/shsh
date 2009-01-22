{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Cmp ( cmp ) where

import System.Console.ShSh.Builtins.Args ( withArgs, flagOn, flag )
import System.Console.ShSh.Builtins.Util ( readFileOrStdin )
import System.Console.ShSh.Shell ( Shell )

import System.Exit ( ExitCode(..) )

{-# NOINLINE cmp #-}
cmp :: [String] -> Shell ExitCode
cmp = withArgs "cmp" header args run
    where run []  = fail "missing operand"
          run [s] = fail $ "missing operand after `"++s++"'"
          run [a,b] = do aa <- filter (/='\r') `fmap` readFileOrStdin a
                         bb <- filter (/='\r') `fmap` readFileOrStdin b
                         if length aa == length bb && aa == bb
                            then return ExitSuccess
                            else fail' $ unwords ["files",a,"and",b,
                                                  "differ.",
                                                  show $ length aa,
                                                  show $ length bb]
          run as = fail $ "too many arguments: "++ show (length as)
          fail' msg = do silent <- flag 's'
                         if silent then return $ ExitFailure 1
                                   else fail msg
          args = [flagOn "s" ["quiet","silent"] 's'
                         "No output; only exitcode." ]
          header = "Usage: cmp [OPTIONS...] FILE1 FILE2\n"++
                   "Compare two files byte by byte."
