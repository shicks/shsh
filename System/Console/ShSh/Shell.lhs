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
                                   getShellState,
                                   getPState, putPState, modifyPState,
                                   runShell, startShell,
                                   withHandler, withExitHandler, failOnError,
                                   pipeState,
                                   sh_out, sh_in, sh_err, sh_io, closeOut,
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
import Data.Monoid ( Monoid, mempty )
import System.Directory ( getCurrentDirectory )
import System ( ExitCode(..) )
import System.Environment ( getEnvironment )
import System.IO ( stdin, stdout, stderr )

import System.Console.ShSh.PipeIO ( PipeState(..), noPipes, shSafeClose,
                                    ShellHandle, streamToHandle )
import System.Console.ShSh.ShellError ( ShellError, catchS, announceError,
                                        exitCode, prefixError, exit )

-- I might want to look into using ST to thread the state...?
data ShellState e = ShellState {
      environment :: [(String,String)],
      aliases     :: [(String,String)],
      functions   :: [(String,String)],
      pipeState   :: PipeState, -- ???
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
getEnv :: MonadPlus m => String -> ShellT a (m String)
getEnv s = Shell $ do e <- gets environment
                      return $ case lookup s e of -- this is StateT's return
                                 Just x  -> return x   -- MonadPlus's return
                                 Nothing -> mzero

tryEnv :: String -> ShellT a String
tryEnv s = Shell $ do e <- gets environment
                      case lookup s e of
                        Just x  -> return x
                        Nothing -> fail $ s++" not set"

setEnv :: String -> String -> ShellT a ()
setEnv s x = Shell $ modify $ \st ->
             st { environment = update s x (environment st) }

withEnv :: String -> (String -> String) -> ShellT a () -- could have safe one too
withEnv s f = do e <- tryEnv s
                 setEnv s (f e)

getAllEnv :: ShellT a [(String,String)]
getAllEnv = Shell $ gets environment

-- Flag commands - moved from Options
setFlag :: Char -> ShellT a ()
setFlag c = withEnv "-" (`union`[c])

unsetFlag :: Char -> ShellT a ()
unsetFlag c = withEnv "-" (\\[c])

getFlag :: Char -> ShellT a Bool
getFlag c = elem c `fmap` tryEnv "-"

getFlags :: ShellT a String
getFlags = tryEnv "-"


parseFlags :: IO String
parseFlags = return "" -- start with no flags set, for now...
                       -- Later we'll get these with getopt

getPState :: ShellT a PipeState
getPState = Shell $ gets pipeState

-- Maybe shouldn't be able to do this...?
putPState :: PipeState -> ShellT a ()
putPState p = Shell $ modify $ \s -> s { pipeState = p }

modifyPState :: (PipeState -> PipeState) -> ShellT a ()
modifyPState f = Shell $ modify $ \s -> s { pipeState = f $ pipeState s }

getShellState :: ShellT e (ShellState e)
getShellState = Shell $ get

startState :: Monoid e => IO (ShellState e)
startState = do e <- liftIO getEnvironment
                f <- liftIO parseFlags -- better way to integrate these
                cwd <- liftIO getCurrentDirectory
                let e' = updateWith "PWD" (fromMaybe cwd) $
                         updateWith "-" (fromMaybe f) e
                return $ ShellState e' [] [] noPipes mempty

startShell :: Monoid e => ShellT e a -> IO ExitCode
startShell a = do s <- startState
                  runShell a s

runShell :: ShellT e a -> ShellState e -> IO ExitCode
runShell (Shell s) e = do result <- evalStateT (runErrorT s) e
                          case result of
                            Right _  -> return ExitSuccess
                            Left err -> do announceError err
                                           return $ exitCode err

failOnError :: ExitCode -> ShellT e ExitCode
failOnError ExitSuccess = return ExitSuccess
failOnError (ExitFailure n) = exit n

sh_in :: ShellT e ShellHandle
sh_in = Shell $ (streamToHandle stdin . p_in) `fmap` gets pipeState
sh_out :: ShellT e ShellHandle
sh_out = Shell $ (streamToHandle stdout . p_out) `fmap` gets pipeState
sh_err :: ShellT e ShellHandle
sh_err = Shell $ (streamToHandle stderr . p_err) `fmap` gets pipeState

closeOut :: ShellT e ()
closeOut = do h <- sh_out
              liftIO $ shSafeClose h

sh_io :: ShellT e ShellHandle -> (ShellHandle -> IO a) -> ShellT e a
sh_io h f = do h' <- h
               liftIO $ f h'

--runShell :: Shell a -> IO a
--runShell (Shell s) = do e <- getEnvironment
--                        return $ evalState s e

-- It seems like this has the nice property that it both threads the
-- state properly, AND is atomic, so that state changes don't go through
-- if we fail...
convertState :: ShellState e -> e' -> ShellState e'
convertState (ShellState e a f p _) x = ShellState e a f p x

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

withExitHandler :: Shell ExitCode -> Shell ExitCode
withExitHandler s = do ame <- getFlag 'e'
                       catchError (catchS s $ \_ -> return ExitSuccess)
                           $ \e -> if ame
                                   then throwError e
                                   else do announceError e
                                           return $ exitCode e

-- This is a bit gratuitous.
infixl 9 .~
(.~) = flip

\end{code}
