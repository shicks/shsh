\chapter{Builtins module}

This module exports a list of builtin commands and how we handle them.

\begin{code}

module System.Console.ShSh.Builtins ( BuiltinCommand(..), runBuiltin, toBuiltin ) where

import Text.Regex.Posix ( (=~) )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( forM_ )
import Data.List ( sort, sortBy )
import Data.Ord ( comparing )
import System.Console.ShSh.Builtins.Cd ( chDir )
import System.Console.ShSh.Builtins.Mkdir ( mkDir )
import System.Console.ShSh.Builtins.Exit ( exit )
import System.Console.ShSh.IO ( oPutStrLn, oPutStr, iGetContents )
import System.Console.ShSh.Options ( setOpts )
import System.Console.ShSh.Redirection ( Redir )
import System.Console.ShSh.Shell ( Shell, withHandler, getAllEnv )
import System.Console.ShSh.ShellError ( withPrefix )
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad.Trans ( liftIO )

data BuiltinCommand = Exec | Exit | Set | Pwd | Cd | Ls | MkDir
                    | Echo | Cat | Grep
     deriving ( Enum, Eq, Ord, Show )

{- What else do we want...? list here:
  rm
  env
-}

-- This will have to change when we want to generalize the I/O...
-- The handle parameter is an input handle for 
runBuiltin :: BuiltinCommand -> [String] -> [Redir] -> Shell ExitCode

runBuiltin Exec _ _   = return ExitSuccess
runBuiltin Exit _ _   = liftIO $ exitWith ExitSuccess -- message?
runBuiltin Set [] _   = showEnv >> return ExitSuccess
runBuiltin Set foo _  = setOpts foo
runBuiltin Echo ss _  = do oPutStrLn $ unwords ss
                           return ExitSuccess
runBuiltin Cat [] _  = do x <- iGetContents
                          oPutStr x
                          return ExitSuccess
runBuiltin Cat fs _  = do mapM_ (\f -> liftIO (readFile f) >>= oPutStr) fs
                          return ExitSuccess
runBuiltin Grep [] _  = fail "grep requires an argument!"
runBuiltin Grep [p] _  = do x <- iGetContents
                            case filter (=~ p) $ lines x of
                              [] -> return $ ExitFailure 1
                              ls -> do oPutStr $ unlines ls
                                       return ExitSuccess
runBuiltin Grep (p:fs) _  = do x <- mapM (liftIO . readFile) fs
                               let fm = zip fs $ map (filter (=~ p) . lines) x
                                   pretty (f,ls) = if length fs > 1
                                                   then map ((f++":")++) ls
                                                   else ls
                               if null $ concatMap snd fm
                                  then return $ ExitFailure 1
                                  else do oPutStr $ unlines $ concatMap pretty fm
                                          return ExitSuccess
runBuiltin Pwd _ _    = do cwd <- liftIO getCurrentDirectory
                           oPutStrLn cwd
                           return ExitSuccess
runBuiltin Ls [] _    = do let unboring ('.':_) = False
                               unboring _ = True
                           fs <- liftIO (getDirectoryContents ".")
                           oPutStr $ unlines $ sort $
                                    filter unboring fs
                           return ExitSuccess
runBuiltin Ls fs _   = do oPutStrLn "TODO"
                          return ExitSuccess
runBuiltin Cd ss _    = withHandler $ withPrefix "cd" $ chDir ss
runBuiltin MkDir ss _ = withHandler $ mkDir ss

-- The BASH version escapes dangerous values with single-quotes, i.e.
--   spaces, parens, etc..., make the output runnable.
showEnv :: Shell ()
showEnv = do env <- getAllEnv
             forM_ (sortBy (comparing fst) env) $ \(e,v) ->
                 oPutStrLn $ e ++ "=" ++ v

toBuiltin :: String -> Maybe BuiltinCommand
toBuiltin "exec" = Just Exec -- currently a no-op...
toBuiltin "exit" = Just Exit
toBuiltin "set" = Just Set
toBuiltin "pwd" = Just Pwd
toBuiltin "echo" = Just Echo
toBuiltin "cat" = Just Cat
toBuiltin "grep" = Just Grep
toBuiltin "cd" = Just Cd
toBuiltin "ls" = Just Ls
toBuiltin "mkdir" = Just MkDir
toBuiltin _ = Nothing

\end{code}
