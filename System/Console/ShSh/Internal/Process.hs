-- |This is the other half of the new version of what I called PipeIO
-- before.  Again, this module does \emph{not} depend on Shell, and
-- therefore Shell can depend on it.

{-# LANGUAGE CPP #-}

-- |This module basically wraps 'System.Process' in a way that allows
-- us to use the 6.10 API even if we don't have the package.  We
-- hide all the dirty laundry in here.
module System.Console.ShSh.Internal.Process (
  Pipe, PipeState(..), launch,
  ReadStream(..), fromReadStream, WriteStream(..), fromWriteStream,
  toWriteStream, toReadStream
) where

import Control.Concurrent ( MVar, newEmptyMVar, takeMVar, putMVar, forkIO )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Monoid, mempty, mappend )
import System.IO ( Handle )
import System.Exit ( ExitCode )
#ifdef HAVE_CREATEPROCESS
import System.Process ( std_in, std_out, std_err, proc, StdStream(..),
                        createProcess, waitForProcess )
#else
import System.Process ( runInteractiveProcess, runProcess, waitForProcess )
import System.IO ( stdout, stderr, hGetContents, hPutStr, hClose )
import System.Console.ShSh.Internal.IO ( wPutStr, rGetContents )
#endif

import System.Console.ShSh.Internal.IO ( WriteHandle, ReadHandle,
                                         toWriteHandle, toReadHandle,
                                         fromWriteHandle, fromReadHandle,
                                         joinHandles, wClose )


import Debug.Trace ( trace )

tr a b = trace (a ++ show b) b

-- |This type is used to keep track of pipes, for the purpose of waiting
-- on them.  We might want to make it an 'MVar' 'ShellHandle' instead, so
-- that we can close the write end of it manually, if we want.
newtype Pipe = Pipe (MVar ())
instance Show Pipe where
    showsPrec p _ = showsPrec p "Pipe"

-- |This is basically copied directly out of the new 'System.Process' API,
-- except we need that 'UseHandle' can be a generalized 'ReadHandle' or
-- 'WriteHandle' instead, so we need to redefine the whole type.  This
-- effectively ends up being a @Maybe (Maybe (WriteHandle))@ or something,
-- I guess.
data WriteStream = WInherit | WUseHandle WriteHandle | WCreatePipe deriving ( Show )
data ReadStream  = RInherit | RUseHandle ReadHandle  | RCreatePipe deriving ( Show )

data PipeState = PipeState { p_in  :: ReadStream,
                             p_out :: WriteStream,
                             p_err :: WriteStream,
                             openPipes :: [Pipe]
                           } deriving ( Show )

-- Inherit is empty; otherwise just use the more recent one.
instance Monoid WriteStream where
    mempty             = WInherit
    mappend x WInherit = x
    mappend _ x        = x

instance Monoid ReadStream where
    mempty             = RInherit
    mappend x RInherit = x
    mappend _ x        = x

instance Monoid PipeState where -- trivial instantiation...
    mempty = PipeState mempty mempty mempty mempty
    mappend (PipeState i o e p) (PipeState i' o' e' p')
        = PipeState (mappend i i') (mappend o o')
                    (mappend e e') (mappend p p')

-- In this case, it seems like whatever I'm using the stream for might
-- be better served with a Maybe Handle, since CreatePipe might not be
-- meaningful?  Think of a use case where this is required instead of
-- just 'fromMaybe'...
fromWriteStream :: Handle -> WriteStream -> WriteHandle
fromWriteStream h WInherit = toWriteHandle h
fromWriteStream _ (WUseHandle h) = h
fromWriteStream _ (WCreatePipe) = undefined -- fail "Pipe not yet created!"

toWriteStream :: Handle -> WriteStream
toWriteStream = WUseHandle . toWriteHandle

fromReadStream :: Handle -> ReadStream -> ReadHandle
fromReadStream h RInherit = toReadHandle h
fromReadStream _ (RUseHandle h) = h
fromReadStream _ (RCreatePipe) = undefined -- fail "Pipe not yet created!"

toReadStream :: Handle -> ReadStream
toReadStream = RUseHandle . toReadHandle

-- This is a tricky function...  This PipeState needs to have CreatePipe.
-- But the Shell's stored one does not.
launch :: String -> [String] -> PipeState -> IO (Maybe WriteHandle,
                                                 Maybe ReadHandle,
                                                 Maybe ReadHandle,
                                                 IO ExitCode)
#ifdef HAVE_CREATEPROCESS
launch c args ps = do let is = rMkStream $ p_in  ps
                          os = wMkStream $ p_out ps
                          es = wMkStream $ p_err ps
                      (i,o,e,pid) <- createProcess $
                                     (proc c args) { std_in  = is,
                                                     std_out = os,
                                                     std_err = es }
                      (ih,ps1) <- rProcess i (p_in ps) ps
                      (oh,ps2) <- wProcess o (p_out ps) ps
                      (eh,ps3) <- wProcess e (p_err ps) ps
                      return (ih,oh,eh,
                              do mapM_ waitForPipe $ concat [ps1,ps2,ps3]
                                 waitForProcess pid)
    where rMkStream RInherit    = Inherit
          rMkStream RCreatePipe = CreatePipe
          rMkStream (RUseHandle h) = fromMaybe CreatePipe $
                                     UseHandle `fmap` fromReadHandle h
          wMkStream WInherit    = Inherit
          wMkStream WCreatePipe = CreatePipe
          wMkStream (WUseHandle h) = fromMaybe CreatePipe $
                                     UseHandle `fmap` fromWriteHandle h
          --wProcess :: Maybe Handle -> WriteStream -> PipeState
          --        -> IO (Maybe WriteHandle, PipeState)
          rProcess Nothing _ p = return (Nothing,[])
          rProcess (Just h) RCreatePipe p = return (Just (toWriteHandle h),[])
          rProcess (Just h) (RUseHandle c) p = do pipe <- inChanPipe c h
                                                  return (Nothing,[pipe])
          wProcess Nothing _ p = return (Nothing,[])
          wProcess (Just h) WCreatePipe p = return (Just (toReadHandle h),[])
          wProcess (Just h) (WUseHandle c) p = do pipe <- outChanPipe h c
                                                  return (Nothing,[pipe])
#else
launch c args ps =
    case p_in ps of
    RCreatePipe ->
        do (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
           case (p_out ps, p_err ps) of
             (WCreatePipe, WCreatePipe) ->
                 return (Just $ toWriteHandle i, Just $ toReadHandle o,
                         Just $ toReadHandle e, waitForProcess pid)
             (WCreatePipe, _) -> fail "launch bug 1"
             (_, WCreatePipe) -> fail "launch bug 2"
             _ -> do p1 <- openPipe (toReadHandle o) outs
                     p2 <- openPipe (toReadHandle e) errs
                     return (Just $ toWriteHandle i, Nothing, Nothing,
                             do mapM_ waitForPipe [p1,p2]
                                waitForProcess pid)
    RInherit -> case (p_out ps, p_err ps) of
                  (WCreatePipe, _) -> fail "launch bug 3"
                  (_, WCreatePipe) -> fail "launch bug 4"
                  _ -> case (fromWriteHandle outs, fromWriteHandle errs) of
                       (Just ho, Just he) ->
                           do let mho = if ho == stdout then Nothing else Just ho
                                  mhe = if he == stderr then Nothing else Just he
                              pid <- runProcess c args Nothing Nothing Nothing mho mhe
                              return (Nothing, Nothing, Nothing,
                                      waitForProcess pid)
                       _ -> do (ii,oo,ee,pid) <- runInteractiveProcess c args Nothing Nothing
                               hClose ii -- this isn't right...
                               p1 <- openPipe (toReadHandle oo) outs
                               p2 <- openPipe (toReadHandle ee) errs
                               return (Nothing, Nothing, Nothing,
                                       do mapM_ waitForPipe [p1,p2]
                                          waitForProcess pid)
    RUseHandle hin ->
        case (p_out ps, p_err ps) of
          (WCreatePipe, _) -> fail "launch bug 5"
          (_, WCreatePipe) -> fail "launch bug 6"
          _ -> case (fromReadHandle hin, fromWriteHandle outs, fromWriteHandle errs) of
               (Just i, Just o, Just e) ->
                   do pid <- runProcess c args Nothing Nothing (Just i) (Just o) (Just e)
                      return (Nothing, Nothing, Nothing,
                              waitForProcess pid)
               (Nothing, _, _) ->
                   do (ii, oo, ee, pid) <- runInteractiveProcess c args Nothing Nothing
                      p1 <- openPipe hin $ toWriteHandle ii
                      p2 <- openPipe (toReadHandle oo) outs
                      p3 <- openPipe (toReadHandle ee) errs
                      return (Nothing, Nothing, Nothing,
                              do mapM_ waitForPipe [p1,p2,p3]
                                 waitForProcess pid)
    where errs = case p_err ps of WInherit -> toWriteHandle stderr
                                  WUseHandle herr -> herr
                                  WCreatePipe -> error "bad errh"
          outs = case p_out ps of WInherit -> toWriteHandle stdout
                                  WUseHandle hout -> hout
                                  WCreatePipe -> error "bad outh"
#endif

outChanPipe :: Handle -> WriteHandle -> IO Pipe
outChanPipe h c = openPipe (toReadHandle h) c

inChanPipe :: ReadHandle -> Handle -> IO Pipe
inChanPipe c h = openPipe c (toWriteHandle h)

openPipe :: ReadHandle -> WriteHandle -> IO Pipe
openPipe r w = do -- read (output) handle, write (input) handle
  mv <- newEmptyMVar
  forkIO $ joinHandles r w $ putMVar mv () >> return ()
  return $ Pipe mv

waitForPipe :: Pipe -> IO ()
waitForPipe (Pipe mv) = takeMVar mv
