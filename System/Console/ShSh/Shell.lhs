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
                                   setAlias, getAlias, getAliases,
                                   getFlag, setFlag, unsetFlag, getFlags,
                                   getShellState, runShell_, runShell,
                                   pipeShells, runInShell,
                                   startShell,
                                   withOutRedirected, withErrRedirected,
                                   withPipes, runWithPipes, runWithPipes_,
                                   withHandler, withHandler_,
                                   withExitHandler, failOnError,
                                   pipeState, closeOut, maybeCloseOut,
                                   withSubState, withSubStateCalled,
                                   withEnvironment )
    where

import Control.Concurrent ( forkIO )
import Control.Monad ( MonadPlus, mzero, mplus, when, foldM )
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
import System.IO ( stdin, stdout, stderr, openFile, IOMode(..), Handle )

import System.Console.ShSh.IO ( MonadSIO, iHandle, oHandle, eHandle )
import System.Console.ShSh.Internal.IO ( ReadHandle, WriteHandle,
                                         rSafeClose, wSafeClose, newPipe,
                                         fromReadHandle, fromWriteHandle,
                                         toReadHandle, toWriteHandle,
                                         wIsOpen, rIsOpen )
import System.Console.ShSh.Internal.Process ( ProcessHandle, launch,
                                              toWriteStream, toReadStream,
                                              Pipe, PipeState(..), waitForPipe,
                                              ReadStream(..), WriteStream(..),
                                              fromReadStream, fromWriteStream
                                            )
import System.Console.ShSh.ShellError ( ShellError, catchS, announceError,
                                        exitCode, prefixError, exit )
import System.Console.ShSh.Parse.AST ( Word, Redir(..), Assignment(..) )

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
    deriving ( Functor, Monad, MonadSIO, MonadError ShellError )

catchIO :: MonadIO m => IO a -> m a
catchIO j = do me <- liftIO (fmap Right j `catch` \e -> return $ Left e)
               case me of
                 Right x -> return x
                 Left e -> fail $ show e

instance MonadIO (ShellT e) where
    liftIO j = Shell $ catchIO j

unshell :: ShellT e a -> InnerShell e a
unshell (Shell s) = s

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
getEnv :: Stringy (InnerShell e) s => String -> ShellT e s
getEnv s = Shell $ do e <- gets environment
                      maybeToStringy (lookup s e) s

-- |This is a simpler version because we also give a default.
tryEnv :: String -> String -> ShellT e String
tryEnv d s = Shell $ gets environment >>= return . fromMaybe d . lookup s

-- |Not much here - we don't care if the thing is currently defined or not:
-- we just set it either way.
setEnv :: String -> String -> ShellT e ()
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

-- *Alias commands
setAlias :: String -> String -> ShellT e ()
setAlias s x = Shell $ modify $ \st ->
               st { aliases = update s x (aliases st) }

getAlias :: Stringy (InnerShell e) s => String -> ShellT e s
getAlias s = Shell $ do e <- gets environment
                        maybeToStringy (lookup s e) s

getAliases :: ShellT e [(String,String)]
getAliases = Shell $ gets aliases

-- *Flag commands - moved from Options
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
startState = do e <- getEnvironment
                f <- parseFlags -- better way to integrate these
                cwd <- getCurrentDirectory
                home <- getHomeDirectory
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
  (result,s') <- catchIO $ runStateT (runErrorT sub) $ convertState s e
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
-- I don't think we ever want to use this?????
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

assign :: (Word -> Shell String) -> [(String,String)]
       -> Assignment -> InnerShell () [(String,String)]
assign expand e (n:=w) = do v <- unshell $ expand w
                            return $ update n v e

redir :: (Word -> Shell String) -> PipeState -> Redir
      -> InnerShell () PipeState
redir expand p (n:>w) = do f <- unshell $ expand w
                           h <- catchIO $ openFile f WriteMode
                           return $ toFile n h p
redir expand p (n:>>w) = do f <- unshell $ expand w
                            h <- catchIO $ openFile f AppendMode
                            return $ toFile n h p
redir expand p (n:<w) = do f <- unshell $ expand w
                           h <- catchIO $ openFile f ReadMode
                           return $ fromFile n h p
redir expand p (n:<>w) = do f <- unshell $ expand w -- probably doesn't work...?
                            h <- catchIO $ openFile f ReadWriteMode
                            return $ fromFile n h p
redir _ _ r = fail $ show r ++ " not yet supported"

toFile :: Int -> Handle -> PipeState -> PipeState
toFile 1 h p = p { p_out = toWriteStream h }
toFile 2 h p = p { p_err = toWriteStream h }
toFile _ _ _ = undefined

fromFile :: Int -> Handle -> PipeState -> PipeState
fromFile 0 h p = p { p_in = toReadStream h }
fromFile _ _ _ = undefined

withEnvironment :: (Word -> Shell String) -> [Redir] -> [Assignment]
                -> Shell ExitCode -> Shell ExitCode
withEnvironment exp rs as (Shell sub) = Shell $ do
  s <- get
  p <- flip (foldM (redir exp)) rs =<< gets pipeState
  e <- flip (foldM (assign exp)) as =<< gets environment
  (result,_) <- catchIO $ runStateT (runErrorT sub) $
                s { pipeState = p, environment = e }
  case result of
    Right a  -> return a
    Left err -> throwError err

\end{code}
