\chapter{Builtins module}

This module exports a list of builtin commands and how we handle them.

\begin{code}

module System.Console.ShSh.Builtins ( BuiltinCommand(..), runBuiltin ) where

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
                                   withHandler, getAllEnv, setEnv,
                                   withEnvironment )
import System.Console.ShSh.ShellError ( withPrefix )
import System.Console.ShSh.Expansions ( expandWord )
import System.Console.ShSh.Parse.AST ( Redir(..), Assignment(..),
                                       Word(..), BuiltinCommand(..) )
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad.Trans ( liftIO )

{- What else do we want...? list here:
  rm
  env
-}

-- This will have to change when we want to generalize the I/O...
-- The handle parameter is an input handle for 
runBuiltin :: BuiltinCommand
           -> [String] -> [Redir] -> [Assignment]
           -> Shell ExitCode

runBuiltin Alias [] rs as = withEnvironment expandWord rs as showAliases
runBuiltin Alias as _ _ = mapM_ alias as >> return ExitSuccess -- doesn't support > yet!!!
runBuiltin Cd ss _ _ = withPrefix "cd" $ chDir ss
runBuiltin Set [] _ _  = showEnv >> return ExitSuccess -- !!! need a withRedirections...
runBuiltin Set foo _ _ = setOpts foo
runBuiltin Source _ _ _ = fail "don't know how to source yet!"
runBuiltin SetVarInternal _ _ [] = return ExitSuccess
runBuiltin SetVarInternal args rs ((name:=word):as)
              = do setEnv name =<< expandWord word
                   runBuiltin SetVarInternal args rs as
-- anyrthing else (doesn't need to change state)
runBuiltin b a rs as = withEnvironment expandWord rs as $ run b a

run :: BuiltinCommand -> [String] -> Shell ExitCode
run Exec _   = return ExitSuccess
run Exit _   = liftIO $ exitWith ExitSuccess -- message?
run Echo ss  = do oPutStrLn $ unwords ss
                  return ExitSuccess
run Cat []   = withPrefix "cat" $
               do x <- iGetContents
                  oPutStr x
                  return ExitSuccess
run Cat fs   = do mapM_ (\f -> liftIO (readFile f) >>= oPutStr) fs
                  return ExitSuccess
run Grep []  = fail "grep requires an argument!"
run Grep [p] = do x <- iGetContents
                  case filter (=~ p) $ lines x of
                    [] -> return $ ExitFailure 1
                    ls -> do oPutStr $ unlines ls
                             return ExitSuccess
run Grep (p:fs) = do x <- mapM (liftIO . readFile) fs
                     let fm = zip fs $ map (filter (=~ p) . lines) x
                         pretty (f,ls) = if length fs > 1
                                         then map ((f++":")++) ls
                                         else ls
                     if null $ concatMap snd fm
                        then return $ ExitFailure 1
                        else do oPutStr $ unlines $
                                        concatMap pretty fm
                                return ExitSuccess
run Pwd _  = do cwd <- liftIO getCurrentDirectory
                oPutStrLn cwd
                return ExitSuccess
run Ls []  = do let unboring ('.':_) = False
                    unboring _ = True
                fs <- liftIO (getDirectoryContents ".")
                oPutStr $ unlines $ sort $
                         filter unboring fs
                return ExitSuccess
run Ls fs = do oPutStrLn "TODO"
               return ExitSuccess
run MkDir ss = withHandler $ mkDir ss
run Tru _  = return ExitSuccess
run Fals _ = return $ ExitFailure 1


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

alias :: String -> Shell ()
alias a | null eqval = do a <- getAlias name
                          case a of
                            Just v  -> showAlias (name,v)
                            Nothing -> ePutStrLn $ "alias: "++name++" not found"
        | otherwise  = do setAlias name val
    where (name,eqval) = break (=='=') a
          val = drop 1 eqval

\end{code}
