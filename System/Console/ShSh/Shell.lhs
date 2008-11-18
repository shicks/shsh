\chapter{Shell module}

This is where we do stuff.

\begin{code}

{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses,
             TypeSynonymInstances #-}

module System.Console.ShSh.Shell ( Shell, ShellT,
                                   getEnv, setEnv, getAllEnv,
                                   tryEnv, withEnv,
                                   getFlag, setFlag, unsetFlag, getFlags,
                                   getShellState, runShell_, runShell,
                                   pipeShells, runInShell,
                                   startShell,
                                   withOutRedirected, withErrRedirected,
                                   withPipes, runWithPipes, runWithPipes_,
                                   withHandler, withHandler_,
                                   withExitHandler, failOnError,
                                   pipeState, closeOut, maybeCloseOut,
                                   withSubState, withSubStateCalled, (.~) )
    where

import Control.Concurrent ( forkIO )
import Control.Monad ( MonadPlus, mzero, mplus, when )
import Control.Monad.Error ( ErrorT, runErrorT,
                             MonadError, throwError, catchError )
import Control.Monad.State ( MonadState, get, put, runStateT,
                             StateT, evalStateT, gets, modify )
import Control.Monad.Trans ( MonadIO, lift, liftIO )
import Data.List ( lookup, union, (\\) )
import Data.Maybe ( fromMaybe, isJust, listToMaybe )
import Data.Monoid ( Monoid, mempty, mappend )
import System.Directory ( getCurrentDirectory, getHomeDirectory )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnvironment )
import System.IO ( stdin, stdout, stderr, openFile, IOMode(..) )

import System.Console.ShSh.IO ( MonadSIO, iHandle, oHandle, eHandle )
import System.Console.ShSh.Internal.IO ( ReadHandle, WriteHandle,
                                         rSafeClose, wSafeClose, newPipe,
                                         fromReadHandle, fromWriteHandle,
                                         toReadHandle, toWriteHandle,
                                         wIsOpen, rIsOpen )
import System.Console.ShSh.Internal.Process ( ProcessHandle,
                                              launch, toWriteStream,
                                              Pipe, PipeState(..), waitForPipe,
                                              ReadStream(..), WriteStream(..),
                                              fromReadStream, fromWriteStream
                                            )
import System.Console.ShSh.ShellError ( ShellError, catchS, announceError,
                                        exitCode, prefixError, exit )

-- I might want to look into using ST to thread the state...?

data ShellState e = ShellState {
      environment :: [(String,String)],
      aliases     :: [(String,String)],
      functions   :: [(String,String)],
      pipeState   :: PipeState,
      extra       :: e
    }

type InnerShell e = ErrorT ShellError (StateT (ShellState e) IO)
newtype ShellT e a = Shell ((InnerShell e) a)
    deriving ( Functor, Monad, MonadIO, MonadSIO, MonadError ShellError )

instance MonadSIO (InnerShell e) where
    iHandle = (fromReadStream stdin . p_in) `fmap` gets pipeState
    oHandle = (fromWriteStream stdout . p_out) `fmap` gets pipeState
    eHandle = (fromWriteStream stderr . p_err) `fmap` gets pipeState

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

class Maybeable a m where
    toMaybe :: m -> Maybe a
instance Maybeable a (Maybe a) where
    toMaybe = id
instance Maybeable a [a] where
    toMaybe = listToMaybe
instance Maybeable a a where
    toMaybe = Just

class Stringy m s where
    maybeToStringy :: Maybe String -> String -> m s
instance (Monad m,MonadPlus n) => Stringy m (n String) where
    maybeToStringy (Just x) _ = return $ return x
    maybeToStringy Nothing  _ = return $ mzero
instance Monad m => Stringy m String where
    maybeToStringy (Just x) _ = return x
    maybeToStringy Nothing  s = fail $ s ++ " not set"

-- |I've gone a bit overboard on the flexibility here.  If we just want
-- a string, we'll get a fail.  If we put it in a MonadPlus then we
-- guarantee no failure.
getEnv :: Stringy (InnerShell a) s => String -> ShellT a s
getEnv s = Shell $ do e <- gets environment
                      maybeToStringy (lookup s e) s

-- |This is a simpler version because we also give a default.
tryEnv :: String -> String -> ShellT a String
tryEnv d s = Shell $ gets environment >>= return . fromMaybe d . lookup s

-- |Not much here - we don't care if the thing is currently defined or not:
-- we just set it either way.
setEnv :: String -> String -> ShellT a ()
setEnv s x = Shell $ modify $ \st ->
             st { environment = update s x (environment st) }

-- |Remove the environment variable from the list.
unsetEnv :: String -> ShellT e ()
unsetEnv s = Shell $ modify $ \st ->
             st { environment = filter ((/=s).fst) (environment st) }

-- |This is a bit more complicated, but basically what it says is that
-- we can give it any function from something string-like to something else
-- stringlike.  So @String->String@, @String->Maybe String@, or
-- @[String]->String@ are all okay.  The instance conditions are that the
-- argument needs to be generatable from the environment (i.e. a MonadPlus
-- or a plain string if we allow failure) and that we know how to convert
-- the output into a maybe.  
withEnv :: (Stringy (InnerShell e) a,Maybeable String b) =>
           String -> (a -> b) -> ShellT e () -- could have safe one too
withEnv s f = do e <- getEnv s
                 case toMaybe $ f e of
                   Just v  -> setEnv s v
                   Nothing -> unsetEnv s

getAllEnv :: ShellT a [(String,String)]
getAllEnv = Shell $ gets environment

-- Flag commands - moved from Options
setFlag :: Char -> ShellT a ()
setFlag c = withEnv "-" (`union`[c])

unsetFlag :: Char -> ShellT a ()
unsetFlag c = withEnv "-" (\\[c])

getFlag :: Char -> ShellT a Bool
getFlag c = elem c `fmap` getEnv "-"

getFlags :: ShellT a String
getFlags = getEnv "-"

parseFlags :: IO String
parseFlags = return "" -- start with no flags set, for now...
                       -- Later we'll get these with getopt

getShellState :: ShellT e (ShellState e)
getShellState = Shell $ get

startState :: Monoid e => IO (ShellState e)
startState = do e <- liftIO getEnvironment
                f <- liftIO parseFlags -- better way to integrate these
                cwd <- liftIO getCurrentDirectory
                home <- liftIO getHomeDirectory
                let e' = updateWith "SHELL" (const "shsh") $
                         updateWith "PWD" (fromMaybe cwd) $
                         updateWith "HOME" (fromMaybe home) $
                         updateWith "-" (fromMaybe f) e
                return $ ShellState e' [] [] mempty mempty

startShell :: Monoid e => ShellT e a -> IO ExitCode
startShell a = do s <- startState
                  runShell a s

runShell :: ShellT e a -> ShellState e -> IO ExitCode
runShell (Shell s) e = do result <- evalStateT (runErrorT s) e
                          case result of
                            Right _  -> return ExitSuccess
                            Left err -> do announceError err
                                           return $ exitCode err

-- |This is handy because @runShell@ changes the return type, so there's
-- no way to easily ignore it by doing anything on the /inside/ of the
-- computation.
runShell_ :: ShellT e a -> ShellState e -> IO ()
runShell_ s e = runShell s e >> return ()

failOnError :: ExitCode -> ShellT e ExitCode
failOnError ExitSuccess = return ExitSuccess
failOnError (ExitFailure n) = exit n

closeOut :: ShellT e ()
closeOut = do h <- oHandle
              liftIO $ wSafeClose h

maybeCloseOut :: ShellT e ()
maybeCloseOut = do h <- oHandle
                   open <- liftIO $ wIsOpen h
                   when open $ liftIO $ wSafeClose h

maybeCloseIn :: ShellT e ()
maybeCloseIn = do h <- iHandle
                  open <- liftIO $ rIsOpen h
                  when open $ liftIO $ rSafeClose h

waitForPipes :: ShellT e ()
waitForPipes = Shell $ do p <- gets pipeState
                          liftIO $ mapM_ waitForPipe (openPipes p)
                          modify $ \s -> s { pipeState = p { openPipes = [] } }

-- |This function is the only way to change the PipeState.  In that sense,
-- it's more of a Reader type, but since we're already in a StateT, there's
-- not really any point in changing it.  It works by returning a computation
-- in which the pipes are locally changed.  There's gotta be a better way?
-- (the difficulty is that the result /must/ be in the Shell monad, since we
-- need to get at the /current/ pipe state.  But we don't want to allow it
-- to be used to change the pipe state, so the effects need to be bounded...?
withPipes :: PipeState -> ShellT e a -> ShellT e a
withPipes p (Shell s) = Shell $ do
                          ps <- gets pipeState
                          modify $ \st -> st { pipeState = ps `mappend` p }
                          ret <- s
                          modify $ \st -> st { pipeState = ps }
                          return ret

withOutRedirected :: FilePath -> ShellT e a -> ShellT e a
withOutRedirected f j =
    do h <- liftIO $ openFile f WriteMode
       withPipes (mempty { p_out = toWriteStream h }) j

withErrRedirected :: FilePath -> ShellT e a -> ShellT e a
withErrRedirected f j =
    do h <- liftIO $ openFile f WriteMode
       withPipes (mempty { p_err = toWriteStream h }) j

-- |This is a convenience function to act like runShell . withPipes
runWithPipes :: PipeState -> ShellState e -> ShellT e a -> IO ExitCode
runWithPipes p e s = runShell (withPipes p s) e

runWithPipes_ :: PipeState -> ShellState e -> ShellT e a -> IO ()
runWithPipes_ p e s = runWithPipes p e s >> return ()

computationWithPipes :: PipeState -> ShellT e a -> ShellT e (IO ExitCode)
computationWithPipes p s = Shell $ do
                             p' <- gets pipeState
                             e <- get
                             return $ runWithPipes (p' `mappend` p) e s

computationWithPipes_ :: PipeState -> ShellT e a -> ShellT e (IO ())
computationWithPipes_ p s = do c <- computationWithPipes p s
                               return $ c >> return ()

runInShell :: String -> [String] -> ShellT e (Maybe WriteHandle, Maybe ReadHandle,
                                              Maybe ReadHandle, ProcessHandle)
runInShell c args = Shell $ do ps <- gets pipeState
                               (ps',a,b,c,d) <- liftIO $ launch c args ps
                               modify $ \s -> s { pipeState = ps' }
                               return (a,b,c,d)

pipeShells :: ShellT e a -> ShellT e ExitCode -> ShellT e ExitCode
pipeShells source dest = do
  (r,w) <- liftIO newPipe
  let sp = mempty { p_out = WUseHandle w }
      dp = mempty { p_in  = RUseHandle r }
  s <- computationWithPipes_ sp $ source >> maybeCloseOut
  d <- computationWithPipes  dp $ dest -- >> maybeCloseIn -- do we want this?
  liftIO $ forkIO  s
  liftIO $ d

--runShell :: Shell a -> IO a
--runShell (Shell s) = do e <- getEnvironment
--                        return $ evalState s e

-- It seems like this has the nice property that it both threads the
-- state properly, AND is atomic, so that state changes don't go through
-- if we fail...
convertState :: ShellState e -> e' -> ShellState e'
convertState (ShellState e a f p _) x = ShellState e a f p x

-- |This is the main worker function for working in a substate.
withSubState' :: (ShellError -> ShellError) -- ^an error processor
              -> ShellT e a                 -- ^a computation
              -> e                          -- ^an initial state
              -> ShellT e' a
withSubState' f (Shell sub) e = Shell $ do
  s <- get
  let saveExtra = extra s
  (result,s') <- liftIO $ runStateT (runErrorT sub) $ convertState s e
  case result of
    Right a  -> do put $ convertState s' saveExtra
                   return a
    Left err -> throwError $ f err -- apply the processor and rethrow

-- |This uses the identity as the error processor.
withSubState :: ShellT e a -> e -> ShellT e' a
withSubState = withSubState' id

-- |This prefixes errors with a @String@ and is provided for convenience.
withSubStateCalled :: String -> ShellT e a -> e -> ShellT e' a
withSubStateCalled name = withSubState' $ prefixError name

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

withHandler_ :: Shell a -> Shell ()
withHandler_ s = withHandler s >> return ()

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
