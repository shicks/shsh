{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses,
             TypeSynonymInstances #-}

-- |This is where we do stuff.

module System.Console.ShSh.Shell ( Shell, ShellT,
                                   getEnv, setEnv, getAllEnv,
                                   tryEnv, withEnv, unsetEnv,
                                   setFunction, getFunction,
                                   setAlias, getAlias, getAliases,
                                   getExitCode,
                                   getFlag, setFlag, unsetFlag, getFlags,
                                   getShellState, runShell_, runShell,
                                   pipeShells, runInShell, withPipes,
                                   startShell, subShell,
                                   withHandler, withHandler_,
                                   withExitHandler, failOnError,
                                   pipeState, closeOut, maybeCloseOut,
                                   withSubState, withErrorsPrefixed,
                                   withEnvironment,
                                   mkShellProcess, runShellProcess,
                                   ShellProcess, PipeInput(..),
                                   pipes -- debugging...
                                 )
    where

import Control.Applicative ( (<|>) )
import Control.Concurrent ( forkIO, MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Monad ( MonadPlus, mzero, mplus, when, foldM, join )
import Control.Monad.Error ( ErrorT, runErrorT,
                             MonadError, throwError, catchError )
import Control.Monad.State ( MonadState, get, put, runStateT,
                             StateT, evalStateT, gets, modify )
import Control.Monad.Trans ( MonadIO, lift, liftIO )
import Data.Bits ( Bits )
import Data.Char ( isDigit )
import Data.List ( lookup, union, unionBy, (\\) )
import Data.Maybe ( fromMaybe, fromJust, isJust, listToMaybe )
import Data.Monoid ( Monoid, mempty, mappend )
import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          getHomeDirectory, doesFileExist )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnvironment )
import System.IO ( stdin, stdout, stderr, openFile, hClose, IOMode(..), Handle )
import System.Process ( waitForProcess )

import System.Console.ShSh.IO ( MonadSIO, iHandle, oHandle, eHandle )
import System.Console.ShSh.Internal.IO ( ReadHandle, WriteHandle,
                                         rSafeClose, wSafeClose, newPipe,
                                         fromReadHandle, fromWriteHandle,
                                         toReadHandle, toWriteHandle,
                                         wIsOpen, rIsOpen, wPutStrLn )
import System.Console.ShSh.Internal.Process ( launch,
                                              toWriteStream, toReadStream,
                                              PipeState(..),
                                              ReadStream(..), WriteStream(..),
                                              fromReadStream, fromWriteStream
                                            )
import System.Console.ShSh.ShellError ( ShellError, catchS, announceError,
                                        exitCode, prefixError, exit )
import System.Console.ShSh.Compat ( on )

import Language.Sh.Syntax ( Word, CompoundStatement,
                            Redir(..), Assignment(..) )

data VarFlags = VarFlags { exported :: Bool, readonly :: Bool }

exportedVar :: VarFlags
exportedVar = VarFlags True False


instance Monoid VarFlags where
    mempty = VarFlags False False
    VarFlags a b `mappend` VarFlags a' b' = VarFlags (a||a') (b||b')

data ShellState e = ShellState {
      environment :: [(String,(VarFlags,String))],
      locals      :: [(String,(VarFlags,Maybe String))], -- undefined vars too
      positionals :: [String],
      aliases     :: [(String,String)],
      functions   :: [(String,(CompoundStatement,[Redir]))],
      pipeState   :: PipeState,
      extra       :: e
    }
emptyState :: Monoid e => ShellState e
emptyState = ShellState [] [] [] [] [] mempty mempty

convertState :: ShellState e -> e' -> ShellState e'
convertState (ShellState e l pp a f p _) x = ShellState e l pp a f p x

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
    iHandle = (fromReadStream  stdin  . p_in)  `fmap` gets pipeState
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

alter :: Eq a => a -> (Maybe b -> Maybe b) -> [(a,b)] -> [(a,b)]
alter x f [] = case f Nothing of
                 Just y  -> [(x,y)]
                 Nothing -> []
alter x f ((x0,y0):xs) | x0==x     = case f $ Just y0 of
                                       Just y' -> (x,y'):alter x f xs
                                       Nothing -> alter x f xs
                       | otherwise = (x0,y0):alter x f xs

-- More elaborate updateWith...
alterM :: (Monad m,Functor m,Eq a)
       => a -> (Maybe b -> m (Maybe b)) -> [(a,b)] -> m [(a,b)]
alterM x f [] = do y <- f Nothing
                   return $ case y of Nothing -> []
                                      Just y' -> [(x,y')]
alterM x f ((x0,y0):xs) | x0==x = do y <- f $ Just y0
                                     xs' <- alterM x f xs
                                     return $ case y of
                                       Nothing -> xs'
                                       Just y' -> (x,y'):xs'
                        | otherwise = ((x0,y0):) `fmap` alterM x f xs

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
getEnv "#" = Shell $ do p <- gets positionals
                        maybeToStringy (Just $ show $ length p) "#"
getEnv "0" = Shell $ maybeToStringy Nothing "0"
getEnv s | all isDigit s = Shell $ do p <- gets positionals
                                      let n = read s
                                      if n>length p
                                         then maybeToStringy Nothing s
                                         else maybeToStringy (Just $ p!!n) s
         | otherwise = Shell $ do e <- gets environment
                                  l <- gets locals
                                  let x = join $ snd `fmap` lookup s l
                                          <|> (Just . snd) `fmap` lookup s e
                                  maybeToStringy x s

-- |This is a simpler version because we also give a default.
tryEnv :: String -> String -> ShellT e String
tryEnv d s = fromMaybe d `fmap` getEnv s

-- |Not much here - we don't care if the thing is currently defined or not:
-- we just set it either way.
-- Now that we have local variables, it's a bit tricker.  Which do we set
-- here?  Should we try to be smart?  What about hidden layers of locals?
-- We could have a weird surprise from: A=10 (A=5 (global A=7) -> A=5?).
-- We'll make this one global, I guess...
-- NOTE: even bash gets confused here!  
{-
$ cat >sourcable <<EOF
A=10
echo hello, $A
echo goodbye, $B
B=50
echo really this time, $B
EOF
$ B=100 . sourcable > foo # note, this works
$ echo $B  # nothing
$ . sourcable >/dev/null
$ echo $B  # now it's 50...
-}
-- We could be a bit smarter, but we might as well just try to emulate
-- bash's behavior... (i.e. we could set the global version also no 
-- matter what)

setVarHelper :: (Monad m, Functor m)
             => String -> a -> [(String,(VarFlags,a))]
             -> m [(String,(VarFlags,a))]
setVarHelper n v xs = alterM n f xs
    where f Nothing  = return $ Just (mempty,v)
          f (Just (flags,x)) | readonly flags = fail $ n++": is read only"
                             | otherwise = return $ Just (flags,v)

-- |Now this is more general - we can unset vars with it!  It also treats
-- locals correctly.
setEnv :: Maybeable String a => String -> a -> ShellT e ()
setEnv s x = Shell $ do let mx = toMaybe x
                        e <- gets environment
                        l <- gets locals
                        e' <- case mx of
                                Just x' -> setVarHelper s x' e
                                Nothing -> return $ filter ((/=s).fst) e
                        l' <- setVarHelper s mx l
                        case lookup s l of
                          Just v -> modify $ \st -> st { locals = l' }
                          Nothing -> modify $ \st -> st { environment = e' }

setFunction :: String -> CompoundStatement -> [Redir] -> ShellT e ()
setFunction s c rs = Shell $ modify $ \st -> st { functions = update s (c,rs) $
                                                              functions st }

getFunction :: String -> Shell (Maybe (CompoundStatement,[Redir]))
getFunction s = Shell $ lookup s `fmap` gets functions

getExitCode :: Shell ExitCode
getExitCode = do e <- getEnv "?"
                 case e of
                   Just s -> if (not $ null $ filter (not . isDigit) s)
                             then fail $ "$? not set properly: "++s
                             else case read s of
                                    0 -> return ExitSuccess
                                    n -> return $ ExitFailure n
                   Nothing -> return ExitSuccess

makeLocal :: String -> ShellT e ()
makeLocal var = Shell $ do x <- unshell $ getEnv var
                           l <- setVarHelper var (Just x) =<< gets locals
                           modify $ \st -> st { locals = l }

-- |Remove the environment variable from the list.
unsetEnv :: String -> ShellT e ()
unsetEnv = setEnv `flip` (Nothing :: Maybe String)

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
                 setEnv s $ (toMaybe $ f e :: Maybe String)

joinStringSet :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
joinStringSet = unionBy ((==) `on` fst)

getAllEnv :: ShellT a [(String,String)]
getAllEnv = Shell $ do l <- gets locals
                       e <- gets environment
                       let e' = map (\(a,(b,c))->(a,Just c)) e
                           l' = map (\(a,(b,c))->(a,c)) l
                       return $ map (\(a,b)->(a,fromJust b)) $
                                filter (isJust.snd) $
                                unionBy ((==)`on`fst) l' e'


-- *Alias commands
setAlias :: String -> String -> ShellT e ()
setAlias s x = Shell $ modify $ \st ->
               st { aliases = update s x (aliases st) }

getAlias :: Stringy (InnerShell e) s => String -> ShellT e s
getAlias s = Shell $ do e <- gets aliases
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
                pwdval <- case lookup "PWD" e of
                          Nothing -> return cwd
                          Just v -> do setCurrentDirectory v
                                       v' <- getCurrentDirectory
                                       if v' == cwd then return v else return cwd
                                    `catch` \_ -> return cwd
                setCurrentDirectory cwd
                home <- getHomeDirectory
                let e' = update "-" (mempty,f) $
                         map (\(a,b)->(a,(exportedVar,b))) $
                         update "SHELL" "shsh" $ update "PWD" pwdval $
                         alter "HOME" (<|> Just home) e
                return $ emptyState { environment = e' }

startShell :: Monoid e => ShellT e ExitCode -> IO ExitCode
startShell a = runShell a =<< startState

runShell :: ShellT e ExitCode -> ShellState e -> IO ExitCode
runShell (Shell s) e = do result <- evalStateT (runErrorT s) e
                          case result of
                            Right ec  -> return ec
                            Left err -> do announceError err
                                           return $ exitCode err

-- |This is handy because @runShell@ changes the return type, so there's
-- no way to easily ignore it by doing anything on the /inside/ of the
-- computation.
runShell_ :: ShellT e a -> ShellState e -> IO ()
runShell_ s e = runShell (s >> return ExitSuccess) e >> return ()

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
-- Minor problem: sleep 1 | false -> closeChan: chan not open => how?!?

maybeCloseIn :: ShellT e ()
maybeCloseIn = do h <- iHandle
                  open <- liftIO $ rIsOpen h
                  when open $ liftIO $ rSafeClose h

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

runInShell :: String -> [String] -> ShellProcess e
runInShell c args ip = Shell $ do ps <- gets pipeState
                                  (mh,_,_,wait) <- catchIO $ launch c args $
                                                                fixInput ip ps
                                  unshell $ returnHandle ip mh
                                  -- do we need to wait for any open pipes...?
                                  catchIO wait

pipes :: ShellT e PipeState
pipes = Shell $ gets pipeState

--------
-------
-- Problem!!!!!  ShellProcess has a Shell Shell.  Setting the pipeState in
-- the outershell, then running the inner shell, will NOT get the job done.
-- We lose the pipes before running the actual job :-/.
-- So we need to rethink this.  Another option would be to use MVar's to
-- get the necessary info where it needs to go, but we may not want to
-- fork, in which case it seems silly...?  Or maybe not...

-- |Here's YET ANOTHER way to do things...
-- The idea is that when we start a process, we may want it to give
-- us a writehandle back out.
data PipeInput = PipeInput (MVar WriteHandle) (MVar ExitCode) | InheritInput

-- how about we pass a Maybe Shell in to the consumer instead of all this
-- mvar nonsense?
type ShellProcess e = PipeInput -> ShellT e ExitCode

forkPipe :: ShellProcess e -> ShellT e (WriteHandle,MVar ExitCode)
forkPipe job = do mvh <- liftIO $ newEmptyMVar
                  mve <- liftIO $ newEmptyMVar
                  forkShell $ job (PipeInput mvh mve) >>= liftIO . putMVar mve
                  h <- liftIO $ takeMVar mvh
                  return (h,mve)

fixInput :: PipeInput -> PipeState -> PipeState
fixInput InheritInput p = p
fixInput (PipeInput _ _) p = p { p_in = RCreatePipe }

returnHandle :: PipeInput -> Maybe WriteHandle -> ShellT e ()
returnHandle InheritInput _ = return ()
returnHandle (PipeInput _ _) Nothing = fail "no WriteHandle given"
returnHandle (PipeInput m _) (Just h) = liftIO $ putMVar m h

setExitCode :: ExitCode -> ShellT e ExitCode
setExitCode ExitSuccess = setEnv "?" "0" >> return ExitSuccess
setExitCode (ExitFailure n) = setEnv "?" (show n) >> return (ExitFailure n)

runShellProcess :: ShellProcess e -> ShellT e ExitCode
runShellProcess sp = do sp InheritInput >>= setExitCode

mkShellProcess :: ShellT e ExitCode -> ShellProcess e
mkShellProcess job (PipeInput h _) = do (r,w) <- liftIO newPipe
                                        liftIO $ putMVar h w
                                        let p = mempty { p_in = RUseHandle r }
                                        withPipes p job >>= setExitCode
mkShellProcess job InheritInput = job

forkShell :: ShellT e a -> ShellT e ()
forkShell job = Shell $ do s <- get
                           let job' = job >> return ExitSuccess
                           catchIO $ forkIO $ runShell job' s >> return ()
                           return ()

-- |This isolates a shell...  but we'll eventually need to do more...?
subShell :: ShellT e ExitCode -> ShellT e ExitCode
subShell job = Shell $ do s <- get
                          catchIO $ runShell job s

pipeShells :: ShellProcess e -> ShellProcess e -> ShellProcess e
pipeShells source dest pi = do
  (h,e) <- forkPipe dest
  withPipes (mempty { p_out = WUseHandle h }) $ source pi >> maybeCloseOut
  liftIO (takeMVar e) >>= setExitCode -- this should work....?

--runShell :: Shell a -> IO a
--runShell (Shell s) = do e <- getEnvironment
--                        return $ evalState s e

-- It seems like this has the nice property that it both threads the
-- state properly, AND is atomic, so that state changes don't go through
-- if we fail...

-- |This is the main worker function for working in a substate.
withSubState :: ShellT e a                 -- ^a computation
             -> e                          -- ^an initial state
             -> ShellT e' a
withSubState (Shell sub) e = Shell $ do
  s <- get
  let saveExtra = extra s
  (result,s') <- catchIO $ runStateT (runErrorT sub) $ convertState s e
  case result of
    Right a  -> do put $ convertState s' saveExtra
                   return a
    Left err -> throwError err -- apply the processor and rethrow

-- |Do something with the errors
withErrors :: (ShellError -> ShellError) -- ^an error processor
           -> ShellT e a -> ShellT e a
withErrors f job = catchError job $ \e -> throwError $ f e

withErrorsPrefixed :: String -> ShellT e a -> ShellT e a
withErrorsPrefixed s = withErrors $ prefixError s

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
withExitHandler s = catchError (catchS s $ \_ -> return ExitSuccess)
                    $ \e -> do announceError e
                               return $ exitCode e

assign :: (Word -> Shell String) -> [(String,(VarFlags,Maybe String))]
       -> Assignment -> InnerShell () [(String,(VarFlags,Maybe String))]
assign expand e (n:=w) = do v <- unshell $ expand w
                            setVarHelper n (Just v) e

redir :: (Word -> Shell String) -> PipeState -> Redir
      -> InnerShell () (PipeState, IO ())
redir expand p (n:>w) = do f <- unshell $ expand w
                           exists <- catchIO $ doesFileExist f
                           noclobber <- unshell $ getFlag 'C'
                           when (exists && noclobber) $
                                fail $ f++": cannot overwrite existing file"
                           h <- catchIO $ openFile f WriteMode
                           return (toFile n h p, hClose h)
redir expand p (n:>|w) = do f <- unshell $ expand w
                            h <- catchIO $ openFile f WriteMode
                            return (toFile n h p, hClose h)
redir expand p (2:>&1) = return (p { p_err = p_out p }, return ())
redir expand p (1:>&2) = return (p { p_out = p_err p }, return ())
redir expand p (n:>>w) = do f <- unshell $ expand w
                            h <- catchIO $ openFile f AppendMode
                            return (toFile n h p, hClose h)
redir expand p (n:<w) = do f <- unshell $ expand w
                           h <- catchIO $ openFile f ReadMode
                           return (fromFile n h p, hClose h)
redir expand p (n:<>w) = do f <- unshell $ expand w -- probably doesn't work...?
                            h <- catchIO $ openFile f ReadWriteMode
                            return (fromFile n h p, hClose h)
redir expand p (Heredoc n _ d) = do s <- unshell $ expand d
                                    (r,w) <- catchIO newPipe
                                    catchIO $ wPutStrLn w s >> wSafeClose w
                                    return (p { p_in = RUseHandle r},return ())
redir expand p (n:<<s) = do fail $ "Unexpanded heredoc: "++show n++"<<"++s
redir expand p (n:<<-s) = do fail $ "Unexpanded heredoc: "++show n++"<<-"++s
redir _ _ r = fail $ show r ++ " not yet supported"

toFile :: Int -> Handle -> PipeState -> PipeState
toFile 1 h p = p { p_out = toWriteStream h }
toFile 2 h p = p { p_err = toWriteStream h }
toFile _ _ _ = undefined

fromFile :: Int -> Handle -> PipeState -> PipeState
fromFile 0 h p = p { p_in = toReadStream h }
fromFile _ _ _ = undefined

-- |Now this allows modifying the state, but locals and redirects
-- are /preserved/ back to the original values.
-- We could possibly be smart by using a flag in a variable to tell
-- whether to merge its value back...  i.e. "this var is local to
-- the current block, so fully defer to the previous block".  This
-- could hold equally well for functions and aliases, etc.
withEnvironment :: (Word -> Shell String) -> [Redir] -> [Assignment]
                -> Shell a -> Shell a
withEnvironment exp rs as (Shell sub) = Shell $ do
  s <- get
  pipest <- gets pipeState
  let redir' (xxx,yyy) zzz = do (xxx',yyy') <- redir exp xxx zzz
                                return (xxx', yyy' >> yyy)
  (p,closehs) <- foldM redir' (pipest, return ()) rs
  ls <- flip (foldM (assign exp)) as =<< gets locals
  (result,s') <- catchIO $ runStateT (runErrorT sub) $
                 s { pipeState = p, locals = ls }
  catchIO closehs
  put $ s' { locals = locals s, pipeState = pipeState s }
  case result of
    Right a  -> return a
    Left err -> throwError err
