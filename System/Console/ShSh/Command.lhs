\chapter{Command module}

Here we run commands.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Command ( runCommand ) where

import System.Console.ShSh.Builtins ( runBuiltin )
import System.Console.ShSh.Expansions ( expandWords, expandWord )
import System.Console.ShSh.IO ( ePutStrLn, oPutStrLn )
import System.Console.ShSh.Parse.AST ( Command(..), AndOrList(..),
                                       Pipeline(..), Statement(..) )
import System.Console.ShSh.ShellError ( announceError )
import System.Console.ShSh.Shell ( Shell, pipeShells, runInShell,
                                   withEnvironment, withExitHandler,
                                   getFlag )
import System.Directory ( findExecutable, doesFileExist )
import System.Process ( waitForProcess )
import System.Exit ( ExitCode(..), exitWith )

import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )

-- |Simply run a 'Command'.
runCommand :: Command -> Shell ExitCode
runCommand (Synchronous list) = withExitHandler $ runList True list
runCommand (Asynchronous list) = do runAsync $ withExitHandler $
                                             runList False list
                                    return ExitSuccess
runCommand c = fail $ "Control structure "++show c++" not yet supported."

runAsync :: Shell a -> Shell ExitCode
runAsync _ = fail "Asyncronous commands not yet supported"
             >> return ExitSuccess -- i.e. (false &) && echo 1

-- |Run an 'AndOrList'.  We pass a @Bool@ for whether to check the @-e@
-- option to exit on a nonzero exit code.
runList :: Bool -> AndOrList -> Shell ExitCode
runList b (Singleton p) = runPipeline b p
runList b (l :&&: p)    = do ec <- runList False l
                             if ec == ExitSuccess
                                then runPipeline b p
                                else return ec
runList b (l :||: p)    = do ec <- runList False l
                             if ec /= ExitSuccess
                                then runPipeline b p
                                else return ec

-- |Run a 'Pipeline'.  We need to keep passing along the @Bool@.
runPipeline :: Bool -> Pipeline -> Shell ExitCode
runPipeline b (Pipeline [s]) = runStatement b s
runPipeline b (Pipeline (s:ss)) = pipeShells (runStatement False s)
                                      (runPipeline b $ Pipeline ss)

-- |Run a 'Statement'.
runStatement :: Bool -> Statement -> Shell ExitCode
runStatement True s = do am_e <- getFlag 'e'
                         ec <- run s
                         when (am_e && ec/=ExitSuccess) $ liftIO $ exitWith ec
                         return ec
runStatement False s = run s

run :: Statement -> Shell ExitCode
run (Builtin b args rs as) = do args' <- expandWords args
                                runBuiltin b args' rs as
run (Statement ws [] []) = do ws' <- expandWords ws
                              if null ws' then return ExitSuccess
                                          else runWithArgs (head ws') $ tail ws'
run (Statement ws rs as) = withEnvironment expandWord rs as $
                             run $ Statement ws [] []
run (Subshell _ _) = fail "Subshells not supported yet."

runWithArgs :: String -> [String] -> Shell ExitCode
runWithArgs cmd args = do exe <- liftIO $ findExecutable cmd -- use own path?
                          case exe of
                            Just fp -> run fp
                            Nothing -> notFound
    where notFound = do let path = '/' `elem` cmd
                        exists <- liftIO $ doesFileExist cmd
                        if path && exists
                           then fail $ cmd++": Permission denied"
                           else fail $ cmd++": No such file or directory"
          run x = do (_,_,_,pid) <- runInShell x args
                     liftIO $ waitForProcess pid

\end{code}
