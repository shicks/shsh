{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternGuards #-}
-- |This module exports a list of builtin commands and how we handle them.

module System.Console.ShSh.Builtins ( builtin, showAlias ) where

import System.Console.ShSh.Builtins.Util ( readFilesOrStdin )

import System.Console.ShSh.Builtins.Cd ( chDir )
import System.Console.ShSh.Builtins.Cmp ( cmp )
import System.Console.ShSh.Builtins.Cp ( cp )
import System.Console.ShSh.Builtins.Date ( date )
import System.Console.ShSh.Builtins.Exit ( exit )
import System.Console.ShSh.Builtins.Grep ( grep )
import System.Console.ShSh.Builtins.Ls ( ls )
import System.Console.ShSh.Builtins.Mkdir ( mkDir )
import System.Console.ShSh.Builtins.Mv ( mv )
import System.Console.ShSh.Builtins.Rm ( rm )
import System.Console.ShSh.Builtins.Sort ( runSort )
import System.Console.ShSh.Builtins.Touch ( touch )
import System.Console.ShSh.Builtins.Umask ( umask )
import System.Console.ShSh.Builtins.Wc ( runWc )

import System.Console.ShSh.IO ( oPutStrLn, oPutStr, ePutStrLn )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Shell ( ShellT, Shell,
                                   getPositionals, modifyPositionals,
                                   getAlias, getAliases, setAlias,
                                   unsetAlias, unsetAllAliases,
                                   getAllEnv, getEnv )
import System.Console.ShSh.Util ( split )

import Control.Applicative ( (<**>) )
import Control.Arrow ( first, second )
import Control.Monad ( forM_, join )
import Control.Monad.Trans ( liftIO )
import Data.Char ( chr, isDigit )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Numeric ( readOct, readHex )
import System.Directory ( getCurrentDirectory )
import System.Exit ( ExitCode(..) )

{- What else do we want...? list here:
  rm
  env
-}

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \a -> g a >>= f -- compatibility
infixr 1 <=<

successfully :: ([String] -> ShellT e ()) -> [String] -> ShellT e ExitCode
successfully job args = job args >> return ExitSuccess

builtins :: [(String,[String] -> Shell ExitCode)]
builtins = [(":",const $ return ExitSuccess),
            ("alias",alias),("cat",cat),
            ("cd",chDir), ("cmp",cmp), ("cp",cp), ("date",date),
            ("echo",echo),("exec",const $ return ExitSuccess),
            ("exit", exit),
            ("false",const $ return $ ExitFailure 1),
            ("grep",grep), ("ls",ls),
            ("mkdir",mkDir), ("mv",mv), ("pwd",pwd), ("rm",rm),
            ("set",set), ("shift",shift), ("sort",runSort),
            ("touch", touch), ("true",const $ return ExitSuccess),
            ("unalias",unalias), ("umask",umask), ("wc",runWc)]
            
builtin :: String -> Shell (Maybe ([String] -> Shell ExitCode))
builtin b = do noBuiltin <- getEnv "NOBUILTIN"
               let r = case noBuiltin of
                         Just s  -> let nb = split ':' s
                                    in b `elem` nb
                         Nothing -> False
               return $ if r then Nothing
                             else lookup b builtins

alias, cat, echo, pwd, set, shift, unalias
    :: [String] -> Shell ExitCode

alias [] = showAliases
alias as = mapM_ mkAlias as >> return ExitSuccess

unalias ("-a":_) = unsetAllAliases >> return ExitSuccess
unalias as = mapM_ unsetAlias as >> return ExitSuccess

set [] = showEnv >> return ExitSuccess
set foo = setOpts foo

cat = successfully $ mapM_ oPutStr <=< readFilesOrStdin

echo = successfully $ readArgs False False
    where readArgs :: Bool -> Bool -> [String] -> Shell ()
          readArgs n e ("-":as) = oPutStr $ echo' n e "-" as
          readArgs n e (('-':a):as) | Just (n',e') <- foldl (<**>) (Just (n,e)) $ map parseArg a
                                                   = readArgs n' e' as
          readArgs n e as = oPutStr $ echo' n e (join $ take 1 as) (drop 1 as)
          parseArg :: Char -> Maybe ((Bool,Bool) -> (Bool,Bool))
          parseArg 'n' = Just $ first (|| True)
          parseArg 'e' = Just $ second (|| True)
          parseArg 'E' = Just $ second (&& False)
          parseArg _ = Nothing
          echo' :: Bool -> Bool -> String -> [String] -> String
          echo' True _ "" [] = ""
          echo' False _ "" [] = "\n"
          echo' n e "" (a:as) = ' ':echo' n e a as
          echo' n True ('\\':cs) as = esc n cs as
          echo' n e (c:cs) as = c:echo' n e cs as
          esc :: Bool -> String -> [String] -> String
          esc n ('0':a:b:c:cs) as | Just x <- fromOctal a b c = chr x:echo' n True cs as
          esc n ('x':a:b:cs) as | Just x <- fromHex a b = chr x:echo' n True cs as
          esc _ ('c':_) _ = ""
          esc n (a:cs) as | Just b <- lookup a codes = b:echo' n True cs as
          esc n cs as = '\\':echo' n True cs as -- anything else
          codes :: [(Char,Char)]
          codes = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),
                   ('r','\r'),('t','\t'),('v','\v'),('\\','\\')]
          fromOctal :: Char -> Char -> Char -> Maybe Int
          fromOctal a b c | a `elem` "012", [(x,"")] <- readOct $ a:b:c:"" = Just x
                          | otherwise = Nothing
          fromHex :: Char -> Char -> Maybe Int
          fromHex a b | [(x,"")] <- readHex $ a:b:"" = Just x
                      | otherwise = Nothing

pwd _ = do cwd <- liftIO getCurrentDirectory
           oPutStrLn cwd
           return ExitSuccess

shift [] = shift ["1"]
shift (s:_) | all isDigit s = do let n = read s
                                 p <- getPositionals
                                 if n > length p
                                    then return $ ExitFailure 1
                                    else do modifyPositionals $ drop n
                                            return ExitSuccess
            | otherwise     = fail $ s++": numeric argument required"

-- The BASH version escapes dangerous values with single-quotes, i.e.
--   spaces, parens, etc..., make the output runnable.
showEnv :: Shell ()
showEnv = do env <- getAllEnv
             forM_ (sortBy (comparing fst) env) $ \(e,v) ->
                 oPutStrLn $ e ++ "=" ++ v

showAlias :: String -> String -> Shell ()
showAlias s x = oPutStrLn $ s ++ "='" ++ concatMap f x ++ "'"
    where f '\'' = "'\"'\"'"
          f c = [c]

showAliases :: Shell ExitCode
showAliases = do as <- getAliases
                 forM_ as $ uncurry showAlias
                 return ExitSuccess

mkAlias :: String -> Shell ()
mkAlias a | null eqval = do aa <- getAlias name
                            case aa of
                              Just v  -> showAlias name v
                              Nothing -> ePutStrLn $
                                         "alias: "++name++" not found"
          | otherwise  = do setAlias name val
    where (name,eqval) = break (=='=') a
          val = drop 1 eqval
