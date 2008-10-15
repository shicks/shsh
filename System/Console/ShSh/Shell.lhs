\chapter{Shell module}

This is where we do stuff.

\begin{code}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Console.ShSh.Shell ( Shell, getEnv, setEnv, getAllEnv, runShell,
                                   tryEnv, withEnv, withHandler )
    where

import Control.Monad ( MonadPlus, mzero )
import Control.Monad.Error ( ErrorT, runErrorT )
import Control.Monad.State ( StateT, evalStateT, gets, modify )
import Control.Monad.Trans ( MonadIO, lift, liftIO )
import Data.List ( lookup )
import Data.Maybe ( fromMaybe, isJust )
import System.Directory ( getCurrentDirectory )
import System ( ExitCode(..) )
import System.Environment ( getEnvironment )

-- I might want to look into using ST to thread the state...?
data ShellState = ShellState {
      environment :: [(String,String)],
      aliases     :: [(String,String)],
      functions   :: [(String,String)]
    }

newtype Shell a = Shell (ErrorT String (StateT ShellState IO) a)
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
                                  (ShellState e [] []) -- "empty state"
                        case result of
                          Right _  -> return ExitSuccess
                          Left err -> do announceError err
                                         return $ ExitFailure 1

--runShell :: Shell a -> IO a
--runShell (Shell s) = do e <- getEnvironment
--                        return $ evalState s e

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

\end{code}
