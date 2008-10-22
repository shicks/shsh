\chapter{Shell module}

This is where we do stuff.

\begin{code}

{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses #-}

module System.Console.ShSh.Shell ( Shell, ShellT,
                                   getEnv, setEnv, getAllEnv,
                                   tryEnv, withEnv,
                                   getFlag, setFlag, unsetFlag, getFlags,
                                   runShell, withHandler,
                                   withSubState, withSubStateCalled, (.~) )
    where

import Control.Monad ( MonadPlus, mzero )
import Control.Monad.Error ( ErrorT, runErrorT,
                             MonadError, throwError, catchError )
import Control.Monad.State ( MonadState, get, put, runStateT,
                             StateT, evalStateT, gets, modify )
import Control.Monad.Trans ( MonadIO, lift, liftIO )
import Data.List ( lookup, union, (\\) )
import Data.Maybe ( fromMaybe, isJust )
import System.Directory ( getCurrentDirectory )
import System ( ExitCode(..) )
import System.Environment ( getEnvironment )

import System.Console.ShSh.ShellError ( ShellError, catchS, announceError,
                                        exitCode, prefixError )


-- I might want to look into using ST to thread the state...?
data ShellState e = ShellState {
      environment :: [(String,String)],
      aliases     :: [(String,String)],
      functions   :: [(String,String)],
      extra       :: e
    }

newtype ShellT e a = Shell (ErrorT ShellError (StateT (ShellState e) IO) a)
    deriving ( Functor, Monad, MonadIO, MonadError ShellError )

instance MonadState e (ShellT e) where
    get = Shell $ gets extra
    put a = Shell $ modify $ \s -> s { extra = a }

type Shell = ShellT ()

-- Simple routines to update associative list elements.
update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update x y [] = [(x,y)]
update x y ((x',y'):xs) | x'==x     = (x',y):xs
                        | otherwise = (x',y'):update x y xs

updateWith :: Eq a => a -> (Maybe b -> b) -> [(a,b)] -> [(a,b)]
updateWith x f [] = [(x,f Nothing)]
updateWith x f ((x',y'):xs) | x'==x     = (x',f (Just y')):xs
                            | otherwise = (x',y'):updateWith x f xs

-- We don't really need to be so flexible here... could just use Maybe...
getEnv :: MonadPlus m => String -> Shell (m String)
getEnv s = Shell $ do e <- gets environment
                      return $ case lookup s e of -- this is StateT's return
                                 Just x  -> return x   -- MonadPlus's return
                                 Nothing -> mzero

tryEnv :: String -> Shell String
tryEnv s = Shell $ do e <- gets environment
                      case lookup s e of
                        Just x  -> return x
                        Nothing -> fail $ s++" not set"

setEnv :: String -> String -> Shell ()
setEnv s x = Shell $ modify $ \st ->
             st { environment = update s x (environment st) }

withEnv :: String -> (String -> String) -> Shell () -- could have safe one too
withEnv s f = do e <- tryEnv s
                 setEnv s (f e)

getAllEnv :: Shell [(String,String)]
getAllEnv = Shell $ gets environment

-- Flag commands - moved from Options
setFlag :: Char -> Shell ()
setFlag c = withEnv "-" (`union`[c])

unsetFlag :: Char -> Shell ()
unsetFlag c = withEnv "-" (\\[c])

getFlag :: Char -> Shell Bool
getFlag c = elem c `fmap` tryEnv "-"

getFlags :: Shell String
getFlags = tryEnv "-"


parseFlags :: IO String
parseFlags = return "" -- start with no flags set, for now...
                       -- Later we'll get these with getopt

runShell :: Shell a -> IO ExitCode
runShell (Shell s) = do e <- getEnvironment
                        f <- parseFlags -- better way to integrate these
                        cwd <- getCurrentDirectory
                        let e' = updateWith "PWD" (fromMaybe cwd) $
                                 updateWith "-" (fromMaybe f) e
                        result <- evalStateT (runErrorT s)
                                  (ShellState e' [] [] ()) -- "empty state"
                        case result of
                          Right _  -> return ExitSuccess
                          Left err -> do announceError err
                                         return $ ExitFailure 1

--runShell :: Shell a -> IO a
--runShell (Shell s) = do e <- getEnvironment
--                        return $ evalState s e

-- It seems like this has the nice property that it both threads the
-- state properly, AND is atomic, so that state changes don't go through
-- if we fail...
convertState :: ShellState e -> e' -> ShellState e'
convertState (ShellState e a f _) x = ShellState e a f x

withSubState :: ShellT e a -> e -> Shell a
withSubState (Shell sub) e = Shell $ do
  s <- get
  (result,s') <- liftIO $ runStateT (runErrorT sub) $ convertState s e
  case result of
    Right a  -> do put $ convertState s' ()
                   return a
    Left err -> throwError err

withSubStateCalled :: String -> ShellT e a -> e -> Shell a
withSubStateCalled name (Shell sub) e = Shell $ do
  s <- get
  (result,s') <- liftIO $ runStateT (runErrorT sub) $ convertState s e
  case result of
    Right a  -> do put $ convertState s' ()
                   return a
    Left err -> throwError $ prefixError name err

{-
withSubState' :: ShellT e a -> e -> Shell a
withSubState' (Shell sub) e = Shell $ do
  s <- get
  let s' = ShellState (environment s) (aliases s) (functions s) e
  sub' <- lift $ runErrorT sub
  case sub' of
    Left err -> fail err
    Right a  -> return $ do -- now we're up to the StateT monad
                  (result,s'') <- lift $ runStateT a
                  put $ ShellState (environment s'') (aliases s'')
                                   (functions s'') ()
                  return result

withSubStateCalled' :: String -> ShellT e a -> e -> Shell a
withSubStateCalled' name (Shell sub) e = Shell $ do
  s <- get
  let s' = ShellState (environment s) (aliases s) (functions s) e
  sub' <- lift $ runErrorT sub
  case sub' of
    Left err -> fail $ name++": "++err
    Right a  -> return $ do -- now we're up to the StateT monad
                  (result,s'') <- lift $ runStateT a
                  put $ ShellState (environment s'') (aliases s'')
                                   (functions s'') ()
                  return result
-}

-- No longer trying to preserve return type... maybe another function?
withHandler :: Shell a -> Shell ExitCode
withHandler s = do ame <- getFlag 'e'
                   catchError (catchS (s >> return ExitSuccess)
                               $ \_ -> return ExitSuccess)
                       $ \e -> if ame
                               then throwError e
                               else do announceError e
                                       return $ exitCode e

-- This is a bit gratuitous.
infixl 9 .~
(.~) = flip

\end{code}
