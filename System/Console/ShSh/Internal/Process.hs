-- |This is the other half of the new version of what I called PipeIO
-- before.  Again, this module does \emph{not} depend on Shell, and
-- therefore Shell can depend on it.

{-# LANGUAGE CPP #-}

-- |This module basically wraps 'System.Process' in a way that allows
-- us to use the 6.10 API even if we don't have the package.  We
-- hide all the dirty laundry in here.
module System.Console.ShSh.Internal.Process (
  PipeState(..), launch,
  ReadStream(..), fromReadStream, WriteStream(..), fromWriteStream,
  toWriteStream, toReadStream
) where

import Control.Concurrent ( MVar, newEmptyMVar, takeMVar, putMVar, forkIO )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Monoid, mempty, mappend )
import System.IO ( Handle )
import System.Exit ( ExitCode )
#ifdef HAVE_CREATEPROCESS
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
                                         joinHandles, wClose )


import Debug.Trace ( trace )

tr a b = trace (a ++ show b) b

-- |This is basically copied directly out of the new 'System.Process' API,
-- except we need that 'UseHandle' can be a generalized 'ReadHandle' or
-- 'WriteHandle' instead, so we need to redefine the whole type.  This
-- effectively ends up being a @Maybe (Maybe (WriteHandle))@ or something,
-- I guess.
data WriteStream = WInherit | WUseHandle WriteHandle | WCreatePipe deriving ( Show )
data ReadStream  = RInherit | RUseHandle ReadHandle  | RCreatePipe deriving ( Show )

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
launch :: String -> [String] -> [(String,String)] -> PipeState
       -> IO (Maybe WriteHandle,
              Maybe ReadHandle,
              Maybe ReadHandle,
              IO ExitCode)
#ifdef HAVE_CREATEPROCESS
launch c args vars ps = do let is = rMkStream $ p_in  ps
                               os = wMkStream $ p_out ps
                               es = wMkStream $ p_err ps
                           (i,o,e,pid) <- createProcess $
                                          (proc c args) { env = Just vars,
                                                          std_in  = is,
                                                          std_out = os,
                                                          std_err = es }
                           (ih,ps1) <- rProcess i (p_in ps) ps
                           (oh,ps2) <- wProcess o (p_out ps) ps
                           (eh,ps3) <- wProcess e (p_err ps) ps
                           return (ih,oh,eh,
                                   do sequence_ $ concat [ps1,ps2,ps3]
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
launch c args env ps =
    case p_in ps of
    RCreatePipe ->
        do (i,o,e,pid) <- runInteractiveProcess c args Nothing (Just env)
           case (p_out ps, p_err ps) of
             (WCreatePipe, WCreatePipe) ->
                 return (Just $ toWriteHandle i, Just $ toReadHandle o,
                         Just $ toReadHandle e, waitForProcess pid)
             (WCreatePipe, _) -> fail "launch bug 1"
             (_, WCreatePipe) -> fail "launch bug 2"
             _ -> do p1 <- outChanPipe o outs
                     p2 <- outChanPipe e errs
                     return (Just $ toWriteHandle i, Nothing, Nothing,
                             do sequence_ [p1,p2]
                                waitForProcess pid)
    RInherit -> case (p_out ps, p_err ps) of
                  (WCreatePipe, _) -> fail "launch bug 3"
                  (_, WCreatePipe) -> fail "launch bug 4"
                  _ -> case (fromWriteHandle outs, fromWriteHandle errs) of
                       (Just ho, Just he) ->
                           do let mho = if ho == stdout then Nothing else Just ho
                                  mhe = if he == stderr then Nothing else Just he
                              pid <- runProcess c args Nothing (Just env) Nothing mho mhe
                              return (Nothing, Nothing, Nothing,
                                      waitForProcess pid)
                       _ -> do (ii,oo,ee,pid) <- runInteractiveProcess c args Nothing (Just env)
                               hClose ii -- this isn't right...
                               p1 <- outChanPipe oo outs
                               p2 <- outChanPipe ee errs
                               return (Nothing, Nothing, Nothing,
                                       do sequence_ [p1,p2]
                                          waitForProcess pid)
    RUseHandle hin ->
        case (p_out ps, p_err ps) of
          (WCreatePipe, _) -> fail "launch bug 5"
          (_, WCreatePipe) -> fail "launch bug 6"
          _ -> case (fromReadHandle hin, fromWriteHandle outs, fromWriteHandle errs) of
               (Just i, Just o, Just e) ->
                   do pid <- runProcess c args Nothing (Just env) (Just i) (Just o) (Just e)
                      return (Nothing, Nothing, Nothing,
                              waitForProcess pid)
               (Nothing, _, _) ->
                   do putStrLn "hello world!"
                      (ii, oo, ee, pid) <- runInteractiveProcess c args Nothing (Just env)
                      p1 <- inChanPipe hin ii
                      p2 <- outChanPipe oo outs
                      p3 <- outChanPipe ee errs
                      return (Nothing, Nothing, Nothing,
                              do sequence_ [p1,p2,p3]
                                 waitForProcess pid)
    where errs = case p_err ps of WInherit -> toWriteHandle stderr
                                  WUseHandle herr -> herr
                                  WCreatePipe -> error "bad errh"
          outs = case p_out ps of WInherit -> toWriteHandle stdout
                                  WUseHandle hout -> hout
                                  WCreatePipe -> error "bad outh"
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
