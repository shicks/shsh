{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses,
             TypeSynonymInstances, CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- |This is where we do stuff.

module System.Console.ShSh.Shell ( Shell, ShellT,
                                   getEnv, setEnv, getAllEnv,
                                   tryEnv, alterEnv, unsetEnv,
                                   makeLocal, withLocalScope,
                                   setExport, getExports,
                                   getPositionals, modifyPositionals,
                                   setFunction, getFunction,
                                   setAlias, getAlias, getAliases,
                                   unsetAlias, unsetAllAliases,
                                   getExitCode, setExitCode,
                                   getFlag, setFlag, unsetFlag, getFlags,
                                   runShell_, runShell,
                                   pipeShells, runInShell, withPipes,
                                   startShell, subShell,
                                   withHandler, withHandler_,
                                   withExitHandler, failOnError,
                                   withMaybeHandler, withChangeCodeHandler,
                                   pipeState, closeOut, maybeCloseOut,
                                   maybeCloseIn, finally, withRedirects,
                                   withSubState, withErrorsPrefixed,
                                   withEnvironment, withPositionals,
                                   runShellProcess, withShellProcess,
                                   ShellProcess(..)
                                   -- * debugging only!
                                 , envVars
                                 )
    where

-- import Debug.Trace ( trace )
import Prelude hiding ( lookup )

import Control.Applicative ( (<|>) )
import Control.Arrow ( first, second, (***) )
import Control.Concurrent ( forkIO, MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Monad ( MonadPlus, when, foldM, join )
import Control.Monad.Error ( ErrorT, runErrorT,
                             MonadError, throwError, catchError )
import Control.Monad.State ( MonadState, get, put, runStateT,
                             StateT, evalStateT, gets, modify )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Char ( isDigit )
import Data.List ( union, unionBy, (\\) )
import Data.Maybe ( fromMaybe, fromJust, isJust )
import Data.Monoid ( Monoid, mempty, mappend )
import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          getHomeDirectory, doesFileExist )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnvironment )
import System.IO ( stdin, stdout, stderr, openFile, hClose, IOMode(..), Handle )

import System.Console.ShSh.IO ( MonadSIO, iHandle, oHandle, eHandle )
import System.Console.ShSh.Internal.IO ( rSafeClose, wSafeClose, newPipe,
                                         wIsOpen, rIsOpenNonBlocking,
                                         wPutStrLn )
import System.Console.ShSh.Internal.Process ( launch,
                                              toWriteStream, toReadStream,
                                              PipeState(..),
                                              ReadStream(..), WriteStream(..),
                                              fromReadStream, fromWriteStream
                                            )
import System.Console.ShSh.ShellError ( ShellError, catchS, announceError,
                                        exitCode, prefixError, exit, failWith )
import System.Console.ShSh.Compat ( on )
import System.Console.ShSh.Var ( Equiv, (===),
                                 Var(..), VarFlags(..),
                                 lookup, update, alter,
                                 Stringy, maybeToStringy, Maybeable, toMaybe,
                                 setVarHelper, setFlagHelper,
                                 setExportedFlag, exportedVar
                               )

import Language.Sh.Syntax ( Word, CompoundStatement,
                            Redir(..), Assignment(..) )

data ShellState e = ShellState {
      environment :: [(Var,(VarFlags,Maybe String))],
      locals      :: [(Var,(VarFlags,Maybe String))], -- undefined vars too
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
newtype ShellT e a = Shell { unShell :: ((InnerShell e) a) }
    deriving ( Functor, Monad, MonadSIO, MonadError ShellError, MonadPlus )

catchIO :: MonadIO m => IO a -> m a
catchIO j = do me <- liftIO (fmap Right j `catch` \e -> return $ Left e)
               case me of
                 Right x -> return x
                 Left e -> fail $ show e

instance MonadIO (ShellT e) where
    liftIO j = Shell $ catchIO j

-- |In this new instance, we make the pipes lazily.  We don't actually
-- gain anything by doing this, except convenience, because we'll
-- always close the handles, which will create and then immediately
-- close the pipe.
instance MonadSIO (InnerShell e) where
    iHandle = do ps <- gets pipeState
                 case p_in ps of
                   RCreatePipe mw ->
                       do (r,w) <- catchIO newPipe
                          modify $ \s -> s { pipeState = ps
                                             { p_in = RUseHandle r } }
                          catchIO $ putMVar mw w
                          return r
                   i -> return $ fromReadStream stdin i
    oHandle = do ps <- gets pipeState
                 case p_out ps of
                   WCreatePipe mr ->
                       do (r,w) <- catchIO newPipe
                          modify $ \s -> s { pipeState = ps
                                             { p_out = WUseHandle w } }
                          catchIO $ putMVar mr r
                          return w
                   o -> return $ fromWriteStream stdout o
    eHandle = do ps <- gets pipeState
                 case p_err ps of
                   WCreatePipe mr ->
                       do (r,w) <- catchIO newPipe
                          modify $ \s -> s { pipeState = ps
                                             { p_err = WUseHandle w } }
                          catchIO $ putMVar mr r
                          return w
                   e -> return $ fromWriteStream stderr e

instance MonadState e (ShellT e) where
    get = Shell $ gets extra
    put a = Shell $ modify $ \s -> s { extra = a }

type Shell = ShellT ()

-- |I've gone a bit overboard on the flexibility here.  If we just want
-- a string, we'll get a fail.  If we put it in a MonadPlus then we
-- guarantee no failure.
getEnv :: Stringy (InnerShell e) s => String -> ShellT e s
getEnv "#" = Shell $ do p <- gets positionals
                        maybeToStringy (Just $ show $ length p) "#"
getEnv "0" = Shell $ maybeToStringy Nothing "0"
getEnv v | all isDigit v = Shell $ do p <- gets positionals
                                      let n = read v - 1 -- starts at 1
                                      if n>length p
                                         then maybeToStringy Nothing v
                                         else maybeToStringy (Just $ p!!n) v
         | otherwise = Shell $ do e <- gets environment
                                  l <- gets locals
                                  let x = join $ snd `fmap` lookup v l
                                             <|> snd `fmap` lookup v e
                                  maybeToStringy x v

-- |This is a simpler version because we also give a default.
tryEnv :: String -> String -> ShellT e String
tryEnv d v = fromMaybe d `fmap` getEnv v

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

-- |Now this is more general - we can unset vars with it!  It also treats
-- locals correctly.  Unfortunately, because of the flags, we can't ever
-- remove anything from the list...  
setEnv :: (Maybeable String a, Show a) => String -> a -> ShellT e ()
setEnv v x = Shell $ do let mx = toMaybe x
                        l <- gets locals
                        case lookup v l of
                          Just _  -> do
                            l' <- setVarHelper v (Just mx) l
                            modify $ \st -> st { locals = l' }
                          Nothing -> do
                            e <- gets environment
                            e' <- case mx of -- can still unset...?  readonly?
                                    Nothing -> setVarHelper v Nothing e
                                    Just _  -> setVarHelper v (Just mx) e
                            modify $ \st -> st { environment = e' }

alterVarFlags :: String -> (VarFlags -> VarFlags) -> ShellT e ()
alterVarFlags v f = Shell $ do e <- gets environment
                               l <- gets locals
                               let e' = setFlagHelper v f e
                                   l' = setFlagHelper v f l
                               case lookup v l of
                                 Just _  -> modify $ \st -> st { locals = l' }
                                 Nothing -> modify $
                                               \st -> st { environment = e' }

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
makeLocal var = Shell $ do x <- unShell $ getEnv var
                           l <- setVarHelper var (Just x) =<< gets locals
                           modify $ \st -> st { locals = l }

setExport :: String -> Bool -> ShellT e ()
setExport var x = alterVarFlags var $ setExportedFlag x

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
alterEnv :: (Stringy (InnerShell e) a,Maybeable String b) =>
            String -> (a -> b) -> ShellT e () -- could have safe one too
alterEnv s f = do e <- getEnv s
                  setEnv s $ (toMaybe $ f e :: Maybe String)

-- joinStringSet :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
-- joinStringSet = unionBy ((==) `on` fst)

getAllEnv :: ShellT a [(String,String)]
getAllEnv = Shell $ do l <- gets locals
                       e <- gets environment
                       p <- zip (map show [1::Int ..]) `fmap` gets positionals
                       return $ unionBy ((===)`on`fst) p $ -- do we add these?
                                map (unVar *** fromJust) $
                                filter (isJust.snd) $
                                map (second snd) $
                                unionBy ((===)`on`fst) l e

getExports :: ShellT a [(String,String)]
getExports = Shell $ do l <- gets locals
                        e <- gets environment
                        return $ map (unVar *** (fromJust . snd)) $
                                 filter (isJust.snd.snd) $
                                 filter (exported.fst.snd) $
                                 unionBy ((===)`on`fst) l e


-- *Positional parameters
getPositionals :: ShellT e [String]
getPositionals = Shell $ gets positionals

modifyPositionals :: ([String] -> [String]) -> ShellT e ()
modifyPositionals f = Shell $ modify $
                      \s -> s { positionals = f $ positionals s }


-- *Alias commands
setAlias :: String -> String -> ShellT e ()
setAlias s x = Shell $ modify $ \st ->
               st { aliases = update s x (aliases st) }

unsetAlias :: String -> ShellT e ()
unsetAlias s = Shell $ modify $ \st ->
               st { aliases = filter ((/=s).fst) (aliases st) }

unsetAllAliases :: ShellT e ()
unsetAllAliases = Shell $ modify $ \st -> st { aliases = [] }

getAlias :: Stringy (InnerShell e) s => String -> ShellT e s
getAlias s = Shell $ do e <- gets aliases
                        maybeToStringy (lookup s e) s

getAliases :: ShellT e [(String,String)]
getAliases = Shell $ gets aliases

-- *Flag commands - moved from Options
setFlag :: Char -> ShellT a ()
setFlag c = alterEnv "-" (`union`[c])

unsetFlag :: Char -> ShellT a ()
unsetFlag c = alterEnv "-" (\\[c])

getFlag :: Char -> ShellT a Bool
getFlag c = elem c `fmap` getEnv "-"

getFlags :: ShellT a String
getFlags = getEnv "-"

parseFlags :: IO String
parseFlags = return "" -- start with no flags set, for now...
                       -- Later we'll get these with getopt

-- getShellState :: ShellT e (ShellState e)
-- getShellState = Shell get

startState :: Monoid e => IO (ShellState e)
startState = do e <- getEnvironment
                f <- parseFlags -- better way to integrate these
                cwd <- getCurrentDirectory
                pwdval <- case lookup "PWD" e of
                          Nothing -> return cwd
                          Just v -> do setCurrentDirectory v
                                       v' <- getCurrentDirectory
                                       if v' == cwd then return v
                                                    else return cwd
                                    `catch` \_ -> return cwd
                setCurrentDirectory cwd
                home <- getHomeDirectory
                let e' = map (first Var) $
                         update "-" (mempty,Just f) $
                         map (\(a,b)->(a,(exportedVar,Just b))) $
                         update "SHELL" "shsh" $ update "PWD" pwdval $
                         alter "HOME" (<|> Just home) $ winVars e
                return $ emptyState { environment = e' }

winVars :: [(String,String)] -> [(String,String)]
#ifndef WINDOWS
winVars = update "PATHSEP" ":" -- cooperate...
#else
winVars = (foldr copyVar `flip` [("UserName","USER")
                                ,("ComputerName","HOSTNAME")
                                ,("CD","PWD")]) .
           update "PATHSEP" ";" -- can make this ";:" if desired...
copyVar :: Equiv a' a => (a,a) -> [(a',b)] -> [(a',b)]
copyVar (a,b) e = case lookup a e of
                    Just u  -> update b u e
                    Nothing -> e
#endif

-- would be still nicer to actually dynamically *autolink* the variables


startShell :: Monoid e => ShellT e ExitCode -> IO ExitCode
startShell a = runShell a =<< startState

runShell :: ShellT e ExitCode -> ShellState e -> IO ExitCode
runShell (Shell s) e = do result <- evalStateT (runErrorT s) e
                          case result of
                            Right ec -> return ec
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
                  open <- liftIO $ rIsOpenNonBlocking h
                  when open $ liftIO $ rSafeClose h

-- |This function is the only way to change the PipeState.  In that sense,
-- it's more of a Reader type, but since we're already in a StateT, there's
-- not really any point in changing it.  It works by returning a computation
-- in which the pipes are locally changed.  There's gotta be a better way?
-- (the difficulty is that the result /must/ be in the Shell monad, since we
-- need to get at the /current/ pipe state.  But we don't want to allow it
-- to be used to change the pipe state, so the effects need to be bounded...?
withPipes :: PipeState -> ShellT e a -> ShellT e a
withPipes p (Shell s) -- -- | trace ("withPipes "++show p) True
             = Shell $ do ps <- gets pipeState
                          modify $ \st -> st { pipeState = ps `mappend` p }
                          ret <- s
                          modify $ \st -> st { pipeState = ps }
                          return ret

runInShell :: String -> [String] -> Shell ExitCode
runInShell c args = Shell $ do ps <- gets pipeState
                               exports <- unShell getExports
                               catchIO $ catchIO $ launch c args exports ps

-- pipes :: ShellT e PipeState
-- pipes = Shell $ gets pipeState

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

-- how about we pass a Maybe Shell in to the consumer instead of all this
-- mvar nonsense?
data ShellProcess = BuiltinProcess  (Shell ExitCode)
                  | ExternalProcess (Shell ExitCode)

forkTarget :: Shell ExitCode -> Shell (PipeState,MVar ExitCode)
forkTarget job = Shell $ do mvh <- catchIO $ newEmptyMVar
                            mve <- catchIO $ newEmptyMVar
                            s <- get
                            let p = mempty { p_in = RCreatePipe mvh }
                                job' = do ec <- withPipes p job
                                          liftIO $ putMVar mve ec
                            catchIO $ forkIO $ runShell_ job' s
                            h <- catchIO $ takeMVar mvh
                            return (mempty { p_out = WUseHandle h },mve)

#ifdef HAVE_CREATEPROCESS
forkSource :: Shell ExitCode -> Shell (PipeState,MVar ExitCode)
forkSource job = Shell $ do mvh <- catchIO $ newEmptyMVar
                            mve <- catchIO $ newEmptyMVar
                            s <- get
                            let p = mempty { p_out = WCreatePipe mvh }
                                job' = do ec <- withPipes p job
                                          liftIO $ putMVar mve ec
                            catchIO $ forkIO $ runShell_ job' s
                            h <- catchIO $ takeMVar mvh
                            return (mempty { p_in = RUseHandle h },mve)
#endif

setExitCode :: ExitCode -> ShellT e ExitCode
setExitCode ExitSuccess = setEnv "?" "0" >> return ExitSuccess
setExitCode (ExitFailure n) = setEnv "?" (show n) >> return (ExitFailure n)

-- openHandles :: Shell ()
-- openHandles = iHandle >> oHandle >> eHandle >> return ()

runShellProcess :: ShellProcess -> Shell ExitCode
runShellProcess (BuiltinProcess  s) = s -- openHandles >> s -- doesn't work?!
runShellProcess (ExternalProcess s) = s

-- |this optionally opens in/out handles
-- runShellProcess' :: ShellProcess -> Bool -> Bool -> Shell ExitCode
-- runShellProcess' (BuiltinProcess  s) openIn openOut
--     = do when openIn $ iHandle >> return ()
--          when openOut $ oHandle >> return ()
--          s -- openHandles >> s
-- runShellProcess' (ExternalProcess s) _ _ = s

finally :: Shell a -> Shell b -> Shell a
finally a b = do { a' <- a; b; return a' }

-- closeFDs :: Shell ()
-- closeFDs = maybeCloseIn >> maybeCloseOut

withShellProcess :: (Shell ExitCode -> Shell ExitCode)
                 -> ShellProcess -> ShellProcess
withShellProcess f (BuiltinProcess  s) = BuiltinProcess  $ f s
withShellProcess f (ExternalProcess s) = ExternalProcess $ f s

-- |This isolates a shell...  but we'll eventually need to do more...?
subShell :: ShellT e ExitCode -> ShellT e ExitCode
subShell job = Shell $ do s <- get
                          catchIO $ runShell job s

-- any optimization here will need to depend on the associativity
-- of how we're running the pipes.  Currently, it runs as
--     p1 | (p2 | (p3 | ...))
-- Thus, we should return the same type as source, since there's nothing
-- after dest...
-- The forked process is the one that's asked to create the pipe, so we
-- should try to fork the external one instead of the builtin one.
pipeShells :: ShellProcess -> ShellProcess -> ShellProcess
#ifdef HAVE_CREATEPROCESS
pipeShells (ExternalProcess src) dst = ExternalProcess $ do
  (p,e) <- forkSource $ src `finally` maybeCloseOut
  ec <- withPipes p $ runShellProcess dst `finally` maybeCloseIn
  liftIO (takeMVar e)
  setExitCode ec
#endif
pipeShells src dst = BuiltinProcess $ do
  (p,e) <- forkTarget $ runShellProcess dst `finally` maybeCloseIn
  withPipes p $ runShellProcess src `finally` maybeCloseOut
  liftIO (takeMVar e) >>= setExitCode -- this should work....?

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

-- |A strange sort of error handler
withMaybeHandler :: ShellT e a -> ShellT e (Maybe a)
withMaybeHandler job = catchError (Just `fmap` job) $ \_ -> return Nothing

withChangeCodeHandler :: Int -> ShellT e a -> ShellT e a
withChangeCodeHandler n job = catchError job $ \err -> failWith n $ show err

assign :: (Word -> Shell String) -> [(Var,(VarFlags,Maybe String))]
       -> Assignment -> InnerShell () [(Var,(VarFlags,Maybe String))]
assign expand e (n:=w) = do v <- unShell $ expand w
                            e' <- setVarHelper n (Just $ Just v) e
                            return $ setFlagHelper n (setExportedFlag True) e'

redir :: (Word -> Shell String) -> PipeState -> Redir
      -> InnerShell () (PipeState, IO ())
redir expand p (n:>w) = do f <- unShell $ expand w
                           exists <- catchIO $ doesFileExist f
                           noclobber <- unShell $ getFlag 'C'
                           when (exists && noclobber) $
                                fail $ f++": cannot overwrite existing file"
                           h <- catchIO $ openFile f WriteMode
                           return (toFile n h p, hClose h)
redir expand p (n:>|w) = do f <- unShell $ expand w
                            h <- catchIO $ openFile f WriteMode
                            return (toFile n h p, hClose h)
redir _      p (2:>&1) = do oHandle
                            return (p { p_err = p_out p }, return ())
redir _      p (1:>&2) = do eHandle -- this will mess stuff up sometimes?
                            return (p { p_out = p_err p }, return ())
redir expand p (n:>>w) = do f <- unShell $ expand w
                            h <- catchIO $ openFile f AppendMode
                            return (toFile n h p, hClose h)
redir expand p (n:<w) = do f <- unShell $ expand w
                           h <- catchIO $ openFile f ReadMode
                           return (fromFile n h p, hClose h)
redir expand p (n:<>w) = do f <- unShell $ expand w -- probably doesn't work...?
                            h <- catchIO $ openFile f ReadWriteMode
                            return (fromFile n h p, hClose h)
redir expand p (Heredoc 0 _ _ d) = do s <- unShell $ expand d
                                      (r,w) <- catchIO newPipe
                                      catchIO $ wPutStrLn w s >> wSafeClose w
                                      return (p { p_in = RUseHandle r},
                                              return ())
redir _ _ (n:<<s) = do fail $ "Unexpanded heredoc: "++show n++"<<"++s
redir _ _ (n:<<-s) = do fail $ "Unexpanded heredoc: "++show n++"<<-"++s
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
withEnvironment expand rs as (Shell sub) = Shell $ do
  s <- get
  pipest <- gets pipeState
  let redir' (xxx,yyy) zzz = do (xxx',yyy') <- redir expand xxx zzz
                                return (xxx', yyy' >> yyy)
  (p,closehs) <- foldM redir' (pipest, return ()) rs
  ls <- flip (foldM (assign expand)) as =<< gets locals
  (result,s') <- catchIO $ runStateT (runErrorT sub) $
                 s { pipeState = p, locals = ls }
  catchIO closehs
  put $ s' { locals = locals s, pipeState = pipeState s }
  case result of
    Right a  -> return a
    Left err -> throwError err

withRedirects :: (Word -> Shell String) -> [Redir] -> Shell a -> Shell a
withRedirects expand rs (Shell sub) = Shell $ do
  s <- get
  pipest <- gets pipeState
  let redir' (xxx,yyy) zzz = do (xxx',yyy') <- redir expand xxx zzz
                                return (xxx', yyy' >> yyy)
  (p,closehs) <- foldM redir' (pipest, return ()) rs
  (result,s') <- catchIO $ runStateT (runErrorT sub) $ s { pipeState = p }
  catchIO closehs
  put $ s' { pipeState = pipeState s }
  case result of
    Right a  -> return a
    Left err -> throwError err

withPositionals :: [String] -> Shell a -> Shell a
withPositionals ps (Shell sub) = Shell $ do
  s <- get
  (result,s') <- catchIO $ runStateT (runErrorT sub) $ s { positionals = ps }
  put $ s' { positionals = positionals s }
  case result of
    Right a  -> return a
    Left err -> throwError err

-- |This is a bit simpler: we just isolate a local scope...
-- Note that we could have chosen to /not/ update the state on
-- an error.  (we don't actually need to export this just now
-- because functions already live inside a 'withEnvironment'.)
withLocalScope :: Shell a -> Shell a
withLocalScope (Shell sub) = Shell $ do
  s <- get
  (result,s') <- catchIO $ runStateT (runErrorT sub) s
  put $ s' { locals = locals s }
  case result of
    Right a  -> return a
    Left err -> throwError err


-- * Debugging functions!

envVars :: ShellT e ([(Var,(VarFlags,Maybe String))],
                     [(Var,(VarFlags,Maybe String))],
                     [String])
envVars = Shell $ do e <- gets environment
                     l <- gets locals
                     p <- gets positionals
                     return (e,l,p)
