\chapter{Shell module}

This is where we do stuff.

\begin{code}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shell ( Shell, getEnv, setEnv, getAllEnv, runShell
             )
    where

import Control.Monad ( MonadPlus, mzero )
import Control.Monad.State ( StateT, evalStateT, get, modify )
--import Control.Monad.State ( State, evalState, get, modify )
import Control.Monad.Trans ( MonadIO )
import Data.List ( lookup )
import System.Environment ( getEnvironment )

-- I might want to look into using ST to thread the state...?
type Env = [(String,String)]
newtype Shell a = Shell (StateT Env IO a)
    deriving ( Functor, Monad, MonadIO )
--newtype Shell a = Shell (State Env a)
--    deriving ( Functor, Monad )

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

setEnv :: String -> String -> Shell ()
setEnv s x = Shell $ modify $ update s x

getAllEnv :: Shell [(String,String)]
getAllEnv = Shell $ get

runShell :: Shell a -> IO a
runShell (Shell s) = do e <- getEnvironment
                        evalStateT s e

--runShell :: Shell a -> IO a
--runShell (Shell s) = do e <- getEnvironment
--                        return $ evalState s e

\end{code}