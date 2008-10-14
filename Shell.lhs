\chapter{Shell module}

This is where we do stuff.

\begin{code}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shell ( Shell, getEnv, setEnv, getAllEnv, runShell,
               tryEnv, withHandler
             )
    where

import Control.Monad ( MonadPlus, mzero )
import Control.Monad.Error ( ErrorT, runErrorT )
import Control.Monad.State ( StateT, evalStateT, get, modify )
--import Control.Monad.State ( State, evalState, get, modify )
import Control.Monad.Trans ( MonadIO, lift, liftIO )
import Data.List ( lookup )
import Data.Maybe ( fromMaybe )
import System.Directory ( getCurrentDirectory )
import System.Environment ( getEnvironment )

-- I might want to look into using ST to thread the state...?
type Env = [(String,String)]
newtype Shell a = Shell (ErrorT String (StateT Env IO) a)
    deriving ( Functor, Monad, MonadIO )

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
getEnv s = Shell $ do e <- get
                      return $ case lookup s e of -- this is StateT's return
                                 Just x  -> return x   -- MonadPlus's return
                                 Nothing -> mzero

tryEnv :: String -> Shell String
tryEnv s = Shell $ do e <- get
                      case lookup s e of
                        Just x  -> return x
                        Nothing -> fail $ s++" not set"

setEnv :: String -> String -> Shell ()
setEnv s x = Shell $ modify $ update s x

getAllEnv :: Shell [(String,String)]
getAllEnv = Shell $ get

runShell :: Shell a -> IO a
runShell (Shell s) = do e <- getEnvironment
                        cwd <- getCurrentDirectory
                        let e' = updateWith "PWD" (fromMaybe cwd) e
                        result <- evalStateT (runErrorT s) e
                        case result of
                          Right a  -> return a
                          Left err -> do putStrLn $ "shsh: "++err
                                         return undefined

withHandler :: String -> Shell a -> Shell (Maybe a)
withHandler h (Shell s)
    = Shell $ do result <- lift $ runErrorT s
                 case result of
                   Right a  -> return $ Just a
                   Left err -> do liftIO $ putStrLn $ "shsh: "++h++": "++err
                                  return Nothing

\end{code}