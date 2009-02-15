{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

-- |This is the other half of the new version of what I called PipeIO
-- before.  Again, this module does \emph{not} depend on Shell, and
-- therefore Shell can depend on it.

-- |This module basically wraps 'System.Process' in a way that allows
-- us to use the 6.10 API even if we don't have the package.  We
-- hide all the dirty laundry in here.
module System.Console.ShSh.Internal.Process (
  PipeState(..), launch,
  ReadStream(..), fromReadStream, WriteStream(..), fromWriteStream,
  toWriteStream, toReadStream
) where

import Control.Concurrent ( MVar, newEmptyMVar, takeMVar, putMVar, forkIO )
import Data.Monoid ( Monoid, mempty, mappend )
import System.IO ( Handle )
import System.Exit ( ExitCode )
#ifdef HAVE_REDIRECTS_CREATEPROCESS
import Data.Maybe ( fromMaybe )
import System.Process.Redirects ( std_in, std_out, std_err, env, proc, StdStream(..),
                                  createProcess, waitForProcess )
import System.IO ( hClose )
#elif HAVE_CREATEPROCESS
import Data.Maybe ( fromMaybe )
import System.Process ( std_in, std_out, std_err, env, proc, StdStream(..),
                        createProcess, waitForProcess )
import System.IO ( hClose )
#else
import System.Process ( runInteractiveProcess, runProcess, waitForProcess )
import System.IO ( stdout, stderr, hGetContents, hPutStr, hClose )
import System.Console.ShSh.Internal.IO ( wPutStr, rGetContents )
#endif

import System.Console.ShSh.Internal.IO ( WriteHandle, ReadHandle,
                                         toWriteHandle, toReadHandle,
                                         fromWriteHandle, fromReadHandle,
                                         joinHandles )

import Debug.Trace ( trace )

-- tr a b = trace (a ++ show b) b

-- |This is basically copied directly out of the new 'System.Process' API,
-- except we need that 'UseHandle' can be a generalized 'ReadHandle' or
-- 'WriteHandle' instead, so we need to redefine the whole type.  This
-- effectively ends up being a @Maybe (Maybe (WriteHandle))@ or something,
-- I guess.
data WriteStream = WInherit | WUseHandle WriteHandle
                 | WCreatePipe (MVar ReadHandle)
data ReadStream  = RInherit | RUseHandle ReadHandle
                 | RCreatePipe (MVar WriteHandle)

instance Show WriteStream where
    show WInherit = "WInherit"
    show (WUseHandle w) = "WUseHandle "++show w
    show (WCreatePipe _) = "WCreatePipe"
instance Show ReadStream where
    show RInherit = "RInherit"
    show (RUseHandle r) = "RUseHandle "++show r
    show (RCreatePipe _) = "RCreatePipe"

data PipeState = PipeState { p_in  :: ReadStream,
                             p_out :: WriteStream,
                             p_err :: WriteStream
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
    mempty = PipeState mempty mempty mempty
    mappend (PipeState i o e) (PipeState i' o' e')
        = PipeState (mappend i i') (mappend o o') (mappend e e')

-- In this case, it seems like whatever I'm using the stream for might
-- be better served with a Maybe Handle, since CreatePipe might not be
-- meaningful?  Think of a use case where this is required instead of
-- just 'fromMaybe'...
fromWriteStream :: Handle -> WriteStream -> WriteHandle
fromWriteStream h WInherit = toWriteHandle h
fromWriteStream _ (WUseHandle h) = h
fromWriteStream _ (WCreatePipe _) = undefined -- fail "Pipe not yet created!"

toWriteStream :: Handle -> WriteStream
toWriteStream = WUseHandle . toWriteHandle

fromReadStream :: Handle -> ReadStream -> ReadHandle
fromReadStream h RInherit = toReadHandle h
fromReadStream _ (RUseHandle h) = h
fromReadStream _ (RCreatePipe _) = undefined -- fail "Pipe not yet created!"

toReadStream :: Handle -> ReadStream
toReadStream = RUseHandle . toReadHandle

-- This is a tricky function...  This PipeState needs to have CreatePipe.
-- But the Shell's stored one does not.
launch :: String -> [String] -> [(String,String)] -> PipeState
       -> IO ExitCode
#if defined(HAVE_CREATEPROCESS) || defined(HAVE_REDIRECTS_CREATEPROCESS)
launch cmd args vars ps = do let is = rMkStream $ p_in  ps
                                 os = wMkStream $ p_out ps
                                 es = wMkStream $ p_err ps
                             (i,o,e,pid) <- createProcess $
                                            (proc cmd args) { env = Just vars,
                                                              std_in  = is,
                                                              std_out = os,
#ifdef HAVE_REDIRECTS_CREATEPROCESS
                                                              std_err = Just es
#else
                                                              std_err = es
#endif
                                                            }
                             ps1 <- rProcess i (p_in ps)
                             ps2 <- wProcess o (p_out ps)
                             ps3 <- wProcess e (p_err ps)
                             sequence_ $ concat [ps1,ps2,ps3]
                             waitForProcess pid
    where rMkStream RInherit    = Inherit
          rMkStream (RCreatePipe _) = trace "rMkStream RCreatePipe" CreatePipe
          rMkStream (RUseHandle h) = fromMaybe CreatePipe $
                                     UseHandle `fmap` fromReadHandle h
          wMkStream WInherit    = Inherit
          wMkStream (WCreatePipe _) = trace "wMkStream WCreatePipe" CreatePipe
          wMkStream (WUseHandle h) = fromMaybe CreatePipe $
                                     UseHandle `fmap` fromWriteHandle h
          --wProcess :: Maybe Handle -> WriteStream -> PipeState
          --        -> IO (Maybe WriteHandle, PipeState)
          rProcess :: Maybe Handle -> ReadStream -> IO [IO ()]
          rProcess Nothing _ = return []
          rProcess (Just w) (RCreatePipe mw) = do putMVar mw $ toWriteHandle w
                                                  return []
          rProcess (Just w) (RUseHandle c) = do pipe <- inChanPipe c w
                                                return [pipe]
          rProcess _ _ = error "impossible"
          wProcess Nothing _ = return []
          wProcess (Just r) (WCreatePipe mr) = do putMVar mr $ toReadHandle r
                                                  return []
          wProcess (Just r) (WUseHandle c) = do pipe <- outChanPipe r c
                                                return [pipe]
          wProcess _ _ = error "impossible"
#else
launch c args env ps =
    case p_in ps of
    RCreatePipe mvi ->
        do (i,o,e,pid) <- runInteractiveProcess c args Nothing (Just env)
           case (p_out ps, p_err ps) of
             (WCreatePipe mvo, WCreatePipe mve) ->
                 do putMVar mvi $ toWriteHandle i
                    putMVar mvo $ toReadHandle o
                    putMVar mve $ toReadHandle e
                    waitForProcess pid
             (WCreatePipe _, _) -> fail "launch bug 1"
             (_, WCreatePipe _) -> fail "launch bug 2"
             _ -> do p1 <- outChanPipe o outs
                     p2 <- outChanPipe e errs
                     putMVar mvi $ toWriteHandle i
                     sequence_ [p1,p2,hClose o, hClose e]
                     waitForProcess pid
    RInherit -> case (p_out ps, p_err ps) of
                  (WCreatePipe _, _) -> fail "launch bug 3"
                  (_, WCreatePipe _) -> fail "launch bug 4"
                  _ -> case (fromWriteHandle outs, fromWriteHandle errs) of
                       (Just ho, Just he) ->
                           do let mho = if ho == stdout then Nothing else Just ho
                                  mhe = if he == stderr then Nothing else Just he
                              pid <- runProcess c args Nothing (Just env) Nothing mho mhe
                              waitForProcess pid
                       _ -> do (ii,oo,ee,pid) <- runInteractiveProcess c args Nothing (Just env)
                               hClose ii -- this isn't right...
                               p1 <- outChanPipe oo outs
                               p2 <- outChanPipe ee errs
                               sequence_ [p1,p2]
                               waitForProcess pid
    RUseHandle hin ->
        case (p_out ps, p_err ps) of
          (WCreatePipe _, _) -> fail "launch bug 5"
          (_, WCreatePipe _) -> fail "launch bug 6"
          _ -> case (fromReadHandle hin, fromWriteHandle outs, fromWriteHandle errs) of
               (Just i, Just o, Just e) ->
                   do pid <- runProcess c args Nothing (Just env) (Just i) (Just o) (Just e)
                      waitForProcess pid
               (Nothing, _, _) ->
                   do putStrLn "hello world!"
                      (ii, oo, ee, pid) <- runInteractiveProcess c args Nothing (Just env)
                      p1 <- inChanPipe hin ii
                      p2 <- outChanPipe oo outs
                      p3 <- outChanPipe ee errs
                      sequence_ [p1,p2,p3]
                      waitForProcess pid
    where errs = case p_err ps of WInherit -> toWriteHandle stderr
                                  WUseHandle herr -> herr
                                  WCreatePipe _ -> error "bad errh"
          outs = case p_out ps of WInherit -> toWriteHandle stdout
                                  WUseHandle hout -> hout
                                  WCreatePipe _ -> error "bad outh"
#endif

outChanPipe :: Handle -> WriteHandle -> IO (IO ())
outChanPipe r w = do mv <- newEmptyMVar
                     forkIO $ joinHandles (toReadHandle r) w $ do putMVar mv ()
                                                                  hClose r
                     return $ takeMVar mv

inChanPipe :: ReadHandle -> Handle -> IO (IO ())
inChanPipe r w = do mv <- newEmptyMVar
                    forkIO $ joinHandles r (toWriteHandle w) $ do putMVar mv ()
                                                                  hClose w
                    return $ takeMVar mv
