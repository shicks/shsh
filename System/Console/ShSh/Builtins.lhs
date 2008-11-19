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
import System.Console.ShSh.IO ( oPutStrLn, oPutStr, iGetContents )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Shell ( Shell, withHandler, getAllEnv, setEnv )
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

runBuiltin Exec _ _ _   = return ExitSuccess
runBuiltin Exit _ _ _   = liftIO $ exitWith ExitSuccess -- message?
runBuiltin Set [] _ _   = showEnv >> return ExitSuccess
runBuiltin Set foo _ _  = setOpts foo
runBuiltin Echo ss _ _  = do oPutStrLn $ unwords ss
                             return ExitSuccess
runBuiltin Cat [] _ _   = withPrefix "cat" $
                          do x <- iGetContents
                             oPutStr x
                             return ExitSuccess
runBuiltin Cat fs _ _   = do mapM_ (\f -> liftIO (readFile f) >>= oPutStr) fs
                             return ExitSuccess
runBuiltin Grep [] _ _  = fail "grep requires an argument!"
runBuiltin Grep [p] _ _ = do x <- iGetContents
                             case filter (=~ p) $ lines x of
                               [] -> return $ ExitFailure 1
                               ls -> do oPutStr $ unlines ls
                                        return ExitSuccess
runBuiltin Grep (p:fs) _ _ = do x <- mapM (liftIO . readFile) fs
                                let fm = zip fs $ map (filter (=~ p) . lines) x
                                    pretty (f,ls) = if length fs > 1
                                                    then map ((f++":")++) ls
                                                    else ls
                                if null $ concatMap snd fm
                                   then return $ ExitFailure 1
                                   else do oPutStr $ unlines $
                                                   concatMap pretty fm
                                           return ExitSuccess
runBuiltin Pwd _ _ _  = do cwd <- liftIO getCurrentDirectory
                           oPutStrLn cwd
                           return ExitSuccess
runBuiltin Ls [] _ _  = do let unboring ('.':_) = False
                               unboring _ = True
                           fs <- liftIO (getDirectoryContents ".")
                           oPutStr $ unlines $ sort $
                                    filter unboring fs
                           return ExitSuccess
runBuiltin Ls fs _ _ = do oPutStrLn "TODO"
                          return ExitSuccess
runBuiltin Cd ss _ _  = withHandler $ withPrefix "cd" $ chDir ss
runBuiltin MkDir ss _ _ = withHandler $ mkDir ss
runBuiltin Tru _ _ _ = return ExitSuccess
runBuiltin Fals _ _ _ = return $ ExitFailure 1
runBuiltin SetVarInternal _ rs [] = return ExitSuccess
runBuiltin SetVarInternal args rs ((name:=word):as)
                     = do setEnv name =<< expandWord word
                          runBuiltin SetVarInternal args rs as

-- The BASH version escapes dangerous values with single-quotes, i.e.
--   spaces, parens, etc..., make the output runnable.
showEnv :: Shell ()
showEnv = do env <- getAllEnv
             forM_ (sortBy (comparing fst) env) $ \(e,v) ->
                 oPutStrLn $ e ++ "=" ++ v

\end{code}
