{-# OPTIONS_GHC -Wall #-}
-- |This module exports a list of builtin commands and how we handle them.

module System.Console.ShSh.Builtins ( builtin ) where

import System.Console.ShSh.Builtins.Args ( withArgs, -- opt, optSet,
                                           flag, flagOn, flagOff )
import System.Console.ShSh.Builtins.Cd ( chDir )
import System.Console.ShSh.Builtins.Cmp ( cmp )
import System.Console.ShSh.Builtins.Cp ( cp )
import System.Console.ShSh.Builtins.Date ( date )
import System.Console.ShSh.Builtins.Exit ( exit )
import System.Console.ShSh.Builtins.Grep ( grep )
import System.Console.ShSh.Builtins.Mkdir ( mkDir )
import System.Console.ShSh.Builtins.Mv ( mv )
import System.Console.ShSh.Builtins.Rm ( rm )
import System.Console.ShSh.Builtins.Sort ( runSort )
import System.Console.ShSh.Builtins.Touch ( touch )

import System.Console.ShSh.IO ( oPutStrLn, oPutStr, ePutStrLn, iGetContents )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Shell ( ShellT, Shell,
                                   getPositionals, modifyPositionals,
                                   getAlias, getAliases, setAlias,
                                   unsetAlias, unsetAllAliases,
                                   getAllEnv, getEnv, unsetEnv )
import System.Console.ShSh.ShellError ( withPrefix )
import System.Console.ShSh.Util ( split )

import Control.Monad ( forM_, unless )
import Control.Monad.State ( put )
import Control.Monad.Trans ( liftIO )
import Data.Char ( chr, ord, isDigit )
import Data.List ( sort, sortBy )
import Data.Ord ( comparing )
import System.Console.GetOpt ( ArgOrder(..) )
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import System.Exit ( ExitCode(..) )

{- What else do we want...? list here:
  rm
  env
-}

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
            ("unalias",unalias),("unset",unset)]
            
builtin :: String -> Shell (Maybe ([String] -> Shell ExitCode))
builtin b = do noBuiltin <- getEnv "NOBUILTIN"
               let r = case noBuiltin of
                         Just s  -> let nb = split ':' s
                                    in b `elem` nb
                         Nothing -> False
               return $ if r then Nothing
                             else lookup b builtins

alias, cat, echo, ls, pwd, set, shift, unalias, unset
    :: [String] -> Shell ExitCode

alias [] = showAliases
alias as = mapM_ mkAlias as >> return ExitSuccess

unalias ("-a":_) = unsetAllAliases >> return ExitSuccess
unalias as = mapM_ unsetAlias as >> return ExitSuccess

set [] = showEnv >> return ExitSuccess
set foo = setOpts foo

cat [] = withPrefix "cat" $ do x <- iGetContents
                               oPutStr x
                               return ExitSuccess
cat fs = do mapM_ (\f -> liftIO (readFile f) >>= oPutStr) fs
            return ExitSuccess

-- |This is nice and all, but apparently -- is NOT supposed to be
-- recognized...
echo = withArgs "echo" header args RequireOrder $ successfully $ \ws -> do
          e <- flag 'e'
          oPutStr . unwords =<< if e then mapM escape ws else return ws
          n <- flag 'n'
          unless n $ oPutStrLn ""
    where escape [] = return []
          escape ('\\':rest) = esc rest
          escape (c:cs) = (c:) `fmap` escape cs
          esc [] = return "\\"
          esc ('0':a:b:c:rest) = (chr (64*ord a + 8*ord b + ord c):)
                                 `fmap` escape rest
          esc ('c':_) = put [('n',Nothing)] >> return [] --suppress '\n' too
          esc (a:rest) = case lookup a codes of
                           Just b -> (b:) `fmap` escape rest
                           Nothing -> ('\\':) `fmap` escape rest
          codes = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),
                   ('r','\r'),('t','\t'),('v','\v'),('\\','\\')]
          header = "Usage: echo [-neE] args...\n"++
                   "Print the arguments on the standard output, "++
                   "separated by spaces."
          args = [flagOn "n" [] 'n' "do not output trailing newline",
                  flagOn "e" [] 'e'
                    "enable interpretation of backslash escapes",
                  flagOff "E" [] 'e'
                    "disable interpretation of backslash escapes (default)"]

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

ls []  = do let unboring ('.':_) = False
                unboring _ = True
            fs <- liftIO (getDirectoryContents ".")
            oPutStr $ unlines $ sort $
                    filter unboring fs
            return ExitSuccess
ls _ = do oPutStrLn "TODO"
          return ExitSuccess

unset [] = return ExitSuccess
unset (a:as) = unsetEnv a >> unset as


-- The BASH version escapes dangerous values with single-quotes, i.e.
--   spaces, parens, etc..., make the output runnable.
showEnv :: Shell ()
showEnv = do env <- getAllEnv
             forM_ (sortBy (comparing fst) env) $ \(e,v) ->
                 oPutStrLn $ e ++ "=" ++ v

showAlias :: (String,String) -> Shell ()
showAlias (s,x) = oPutStrLn $ s ++ "='" ++ concatMap f x ++ "'"
    where f '\'' = "'\"'\"'"
          f c = [c]

showAliases :: Shell ExitCode
showAliases = do as <- getAliases
                 forM_ as showAlias
                 return ExitSuccess

mkAlias :: String -> Shell ()
mkAlias a | null eqval = do aa <- getAlias name
                            case aa of
                              Just v  -> showAlias (name,v)
                              Nothing -> ePutStrLn $
                                         "alias: "++name++" not found"
          | otherwise  = do setAlias name val
    where (name,eqval) = break (=='=') a
          val = drop 1 eqval
