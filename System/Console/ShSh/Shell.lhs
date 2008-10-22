\chapter{Shell module}

This is where we do stuff.

\begin{code}

{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses #-}

module System.Console.ShSh.Shell ( Shell, ShellT,
                                   getEnv, setEnv, getAllEnv, runShell,
                                   tryEnv, withEnv, withHandler,
                                   withSubState, withSubStateCalled, (.~) )
    where

import Control.Monad ( MonadPlus, mzero )
import Control.Monad.Error ( ErrorT, runErrorT )
import Control.Monad.State ( MonadState, get, put, runStateT,
                             StateT, evalStateT, gets, modify )
import Control.Monad.Trans ( MonadIO, lift, liftIO )
import Data.List ( lookup )
import Data.Maybe ( fromMaybe, isJust )
import System.Directory ( getCurrentDirectory )
import System ( ExitCode(..) )
import System.Environment ( getEnvironment )

-- I might want to look into using ST to thread the state...?
data ShellState e = ShellState {
      environment :: [(String,String)],
      aliases     :: [(String,String)],
      functions   :: [(String,String)],
      extra       :: e
    }

newtype ShellT e a = Shell (ErrorT String (StateT (ShellState e) IO) a)
    deriving ( Functor, Monad, MonadIO )

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
    Left err -> fail err

withSubStateCalled :: String -> ShellT e a -> e -> Shell a
withSubStateCalled name (Shell sub) e = Shell $ do
  s <- get
  (result,s') <- liftIO $ runStateT (runErrorT sub) $ convertState s e
  case result of
    Right a  -> do put $ convertState s' ()
                   return a
    Left err -> fail $ name++": "++err

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

withHandler :: String -> Shell a -> Shell (Maybe a)
withHandler h (Shell s)
 = Shell $ do ame <- (elem 'e' . fromMaybe "" . lookup "-") `fmap` gets environment
              let die err = if ame
                            then fail $ prefix err
                            else do announceError $ prefix err
                                    return Nothing
              result <- lift $ runErrorT s
              case result of
                Right a  -> return $ Just a
                Left err -> die err
   where prefix x = if null h || null x then x else h++": "++x

announceError "" = return ()
announceError e = liftIO $ putStrLn $ "shsh: "++e

-- This is a bit gratuitous.
infixl 9 .~
(.~) = flip

\end{code}
