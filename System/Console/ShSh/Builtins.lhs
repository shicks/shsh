\chapter{Builtins module}

This module exports a list of builtin commands and how we handle them.

\begin{code}

module System.Console.ShSh.Builtins ( builtin ) where

import System.Console.ShSh.Builtins.Args ( withArgs, -- opt, optSet,
                                           flag, flagOn, flagOff )
import System.Console.ShSh.Builtins.Cd ( chDir )
import System.Console.ShSh.Builtins.Mkdir ( mkDir )
import System.Console.ShSh.Builtins.Exit ( exit )

import Text.Regex.Posix ( (=~) )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( forM_, unless, ap )
import Control.Monad.State ( put )
import Data.Char ( chr, ord )
import Data.List ( sort, sortBy )
import Data.Ord ( comparing )
import System.Console.ShSh.IO ( oPutStrLn, oPutStr, ePutStrLn, iGetContents )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Shell ( ShellT, Shell,
                                   getAlias, getAliases, setAlias,
                                   withHandler, getAllEnv, getEnv,
                                   withEnvironment, pipes,
                                   ShellProcess, mkShellProcess )
import System.Console.ShSh.ShellError ( withPrefix )
import System.Console.ShSh.Expansions ( expandWord )
import System.Console.ShSh.Parse.AST ( Redir(..), Assignment(..),
                                       Word(..) )
import System.Console.ShSh.Util ( split )
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad.Trans ( liftIO )

{- What else do we want...? list here:
  rm
  env
-}

successfully :: ([String] -> ShellT e ()) -> [String] -> ShellT e ExitCode
successfully job args = job args >> return ExitSuccess

builtins :: [(String,[String] -> Shell ExitCode)]
builtins = [(".",source),("alias",alias),("cat",cat),
            ("cd",withPrefix "cd" . chDir),
            ("echo",echo),
            ("exec",const $ return ExitSuccess),
            ("exit",const $ liftIO $ exitWith ExitSuccess),
            ("false",const $ return $ ExitFailure 1),
            ("grep",grep),("ls",ls),
            ("mkdir",mkDir),("pwd",pwd),
            ("set",set),("source",source),
            ("true",const $ return ExitSuccess)]
            
builtin :: String -> Shell (Maybe ([String] -> ShellProcess ()))
builtin b = do noBuiltin <- getEnv "NOBUILTIN"
               let r = case noBuiltin of
                         Just s  -> let nb = split ':' s
                                    in b `elem` nb
                         Nothing -> False
               return $ if r then Nothing
                             else (mkShellProcess .) `fmap` lookup b builtins

source, alias, cat, echo, grep, pwd, set :: [String] -> Shell ExitCode

alias [] = showAliases
alias as = mapM_ mkAlias as >> return ExitSuccess

set [] = showEnv >> return ExitSuccess
set foo = setOpts foo

source _ = fail "don't know how to source yet!"

cat [] = withPrefix "cat" $ do x <- iGetContents
                               oPutStr x
                               return ExitSuccess
cat fs = do mapM_ (\f -> liftIO (readFile f) >>= oPutStr) fs
            return ExitSuccess

echo = withArgs "echo" header args $ successfully $ \ws -> do
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

grep []  = fail "grep requires an argument!"
grep [p] = do x <- iGetContents
              case filter (=~ p) $ lines x of
                [] -> return $ ExitFailure 1
                ls -> do oPutStr $ unlines ls
                         return ExitSuccess
grep (p:fs) = do x <- mapM (liftIO . readFile) fs
                 let fm = zip fs $ map (filter (=~ p) . lines) x
                     pretty (f,ls) = if length fs > 1
                                     then map ((f++":")++) ls
                                     else ls
                 if null $ concatMap snd fm
                    then return $ ExitFailure 1
                    else do oPutStr $ unlines $
                                    concatMap pretty fm
                            return ExitSuccess

pwd _ = do cwd <- liftIO getCurrentDirectory
           oPutStrLn cwd
           return ExitSuccess

ls []  = do let unboring ('.':_) = False
                unboring _ = True
            fs <- liftIO (getDirectoryContents ".")
            oPutStr $ unlines $ sort $
                    filter unboring fs
            return ExitSuccess
ls fs = do oPutStrLn "TODO"
           return ExitSuccess


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
mkAlias a | null eqval = do a <- getAlias name
                            case a of
                              Just v  -> showAlias (name,v)
                              Nothing -> ePutStrLn $
                                         "alias: "++name++" not found"
          | otherwise  = do setAlias name val
    where (name,eqval) = break (=='=') a
          val = drop 1 eqval

\end{code}
