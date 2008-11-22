\chapter{Builtins module}

This module exports a list of builtin commands and how we handle them.

\begin{code}

module System.Console.ShSh.Builtins ( builtin ) where

import System.Console.ShSh.Builtins.Cd ( chDir )
import System.Console.ShSh.Builtins.Mkdir ( mkDir )
import System.Console.ShSh.Builtins.Exit ( exit )

import Text.Regex.Posix ( (=~) )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( forM_ )
import Data.List ( sort, sortBy )
import Data.Ord ( comparing )
import System.Console.ShSh.IO ( oPutStrLn, oPutStr, ePutStrLn, iGetContents )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Shell ( Shell, getAlias, getAliases, setAlias,
                                   withHandler, getAllEnv, getEnv,
                                   withEnvironment, pipes,
                                   ShellProcess, mkShellProcess )
import System.Console.ShSh.ShellError ( withPrefix )
import System.Console.ShSh.Expansions ( expandWord )
import System.Console.ShSh.Parse.AST ( Redir(..), Assignment(..),
                                       Word(..) )
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad.Trans ( liftIO )

{- What else do we want...? list here:
  rm
  env
-}

successfully :: ([String] -> Shell ()) -> [String] -> Shell ExitCode
successfully job args = job args >> return ExitSuccess

builtins :: [(String,[String] -> Shell ExitCode)]
builtins = [(".",source),("alias",alias),("cat",cat),
            ("cd",withPrefix "cd" . chDir),
            ("echo",successfully $ oPutStrLn . unwords),
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

split :: Eq a => a -> [a] -> [[a]]
split c [] = [[]]
split c (c':cs) | c==c'     = []:split c cs
                | otherwise = case split c cs of
                                [] -> [[c]]
                                (s:ss) -> (c:s):ss

source, alias, cat, grep, pwd, set :: [String] -> Shell ExitCode

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
