{-# OPTIONS_GHC -Wall #-}

-- |Here we deal with the various command-line options, which can
-- also be set with the @set@ builtin.

-- |I've moved this all out of the Shell monad so that we can put
-- all the details of all the options into this module, and then
-- Shell can import it for, e.g. parsing command-line options.

-- |Alternately, it might be better to just split this into two
-- separate modules, one with just the data, and the other with
-- the monadic operations.

-- |The difficulty is that we'd like to be able to use something like
-- 'setOpts' defined here to parse the command-line flags.  So maybe
-- this should end up somewhere else...?

module System.Console.ShSh.Options ( setOpts ) where

import Control.Monad ( forM_ )
import System.Exit ( ExitCode(..) )

import System.Console.ShSh.IO ( oPutStrLn )
import System.Console.ShSh.Shell ( Shell, setFlag, unsetFlag, getFlag,
                                   modifyPositionals )

options :: [(Char,String)]
options = [('e',"errexit"),
           ('f',"noglob"),
           ('v',"verbose"),
           ('C',"noclobber"),
           ('x',"xtrace")]

swop :: [(String,Char)]
swop = map (\(a,b)->(b,a)) options

isOpt :: String -> Bool
isOpt ('-':_) = True
isOpt ('+':_) = True
isOpt _ = False

setOpts :: [String] -> Shell ExitCode
setOpts [] = return ExitSuccess
-- the "o" options are pesky...
setOpts ("-o":opt:ss) | isOpt opt = setOptLong opt >> setOpts ss
                      | otherwise = showOptsHuman >> setOpts (opt:ss)
setOpts ("+o":opt:ss) | isOpt opt = unsetOptLong opt >> setOpts ss
                      | otherwise = showOpts >> setOpts (opt:ss)
setOpts ["-o"] = showOptsHuman >> return ExitSuccess
setOpts ["+o"] = showOpts >> return ExitSuccess
setOpts (s:ss) = case s of
                   "--"     -> setPositionals ss
                   '-':c:cs -> setOpt c   >> setOpts (('-':cs):ss)
                   '+':c:cs -> unsetOpt c >> setOpts (('-':cs):ss)
                   _ -> setPositionals (s:ss)

setOpt :: Char -> Shell ()
setOpt c = case lookup c options of
             Just _ -> setFlag c
             Nothing -> fail $ '-':c:": invalid option"

unsetOpt :: Char -> Shell ()
unsetOpt c = case lookup c options of
               Just _ -> unsetFlag c
               Nothing -> fail $ '+':c:": invalid option"

setOptLong :: String -> Shell ()
setOptLong s = case lookup s swop of
                 Just c -> setFlag c
                 Nothing -> fail $ s++": invalid option name"

unsetOptLong :: String -> Shell ()
unsetOptLong s = case lookup s swop of
                   Just c -> unsetFlag c
                   Nothing -> fail $ s++": invalid option name"

showOptsHuman :: Shell ()
showOptsHuman = do forM_ options $ \(c,l) -> do
                     v <- getFlag c
                     oPutStrLn $ pad 16 l ++ (if v then "on" else "off")
    where pad n s | len < n = s++replicate (n-len) ' '
                  | otherwise = s++"  "
                  where len = length s

showOpts :: Shell ()
showOpts = do forM_ options $ \(c,l) -> do
              v <- getFlag c
              oPutStrLn $ "set " ++ (if v then "-" else "+") ++ "o " ++ l

setPositionals :: [String] -> Shell ExitCode
setPositionals ss = modifyPositionals (const ss) >> return ExitSuccess
