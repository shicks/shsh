\chapter{Internal.Process Module}

This is the other half of the new version of what I called PipeIO
before.  Again, this module does \emph{not} depend on Shell, and
therefore Shell can depend on it.

\begin{code}

-- |This module basically wraps 'System.Process' in a way that allows
-- us to use the 6.10 API even if we don't have the package.  We
-- hide all the dirty laundry in here.
module System.Console.ShSh.Internal.Process (
  CreateProcess(..), ProcessHandle, -- re-export this...
  Pipe, waitForPipe, PipeState(..), launch,
  ReadStream(..), fromReadStream, WriteStream(..), fromWriteStream
) where

import Control.Concurrent ( MVar, newEmptyMVar, takeMVar, putMVar, forkIO )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Monoid, mempty, mappend )
import System.IO ( Handle )
import System.Process ( CreateProcess(..), ProcessHandle, StdStream(..),
                        createProcess )

import System.Console.ShSh.Internal.IO ( WriteHandle, ReadHandle,
                                         toWriteHandle, toReadHandle,
                                         fromWriteHandle, fromReadHandle,
                                         joinHandles, wClose )


-- |This type is used to keep track of pipes, for the purpose of waiting
-- on them.  We might want to make it an 'MVar' 'ShellHandle' instead, so
-- that we can close the write end of it manually, if we want.
newtype Pipe = Pipe (MVar ())

-- |This is basically copied directly out of the new 'System.Process' API,
-- except we need that 'UseHandle' can be a generalized 'ReadHandle' or
-- 'WriteHandle' instead, so we need to redefine the whole type.  This
-- effectively ends up being a @Maybe (Maybe (WriteHandle))@ or something,
-- I guess.
data WriteStream = WInherit | WUseHandle WriteHandle | WCreatePipe
data ReadStream  = RInherit | RUseHandle ReadHandle  | RCreatePipe

data PipeState = PipeState { p_in  :: ReadStream,
                             p_out :: WriteStream,
                             p_err :: WriteStream,
                             openPipes :: [Pipe]
                           }

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

fromReadStream :: Handle -> ReadStream -> ReadHandle
fromReadStream h RInherit = toReadHandle h
fromReadStream _ (RUseHandle h) = h
fromReadStream _ (RCreatePipe) = undefined -- fail "Pipe not yet created!"

-- This is a tricky function...  This PipeState needs to have CreatePipe.
-- But the Shell's stored one does not.
launch :: CreateProcess -> PipeState -> IO (PipeState,
                                            Maybe WriteHandle,
                                            Maybe ReadHandle,
                                            Maybe ReadHandle,
                                            ProcessHandle)
launch pr ps = do let is = rMkStream $ p_in  ps
                      os = wMkStream $ p_out ps
                      es = wMkStream $ p_err ps
                  (i,o,e,pid) <- createProcess $ pr { std_in  = is,
                                                      std_out = os,
                                                      std_err = es }
                  (ih,ps') <- rProcess i (p_in ps) ps
                  (oh,ps'') <- wProcess o (p_out ps) ps'
                  (eh,ps''') <- wProcess e (p_err ps) ps''
                  return (ps''',ih,oh,eh,pid)
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
          rProcess Nothing _ p = return (Nothing,p)
          rProcess (Just h) RCreatePipe p = return (Just (toWriteHandle h),p)
          rProcess (Just h) (RUseHandle c) p = do
            pipe <- inChanPipe c h
            return (Nothing,p { openPipes = openPipes p ++ [pipe] })
          wProcess Nothing _ p = return (Nothing,p)
          wProcess (Just h) WCreatePipe p = return (Just (toReadHandle h),p)
          wProcess (Just h) (WUseHandle c) p = do
            pipe <- outChanPipe h c
            return (Nothing,p { openPipes = openPipes p ++ [pipe] })

outChanPipe :: Handle -> WriteHandle -> IO Pipe
outChanPipe h c = openPipe (toReadHandle h) c

inChanPipe :: ReadHandle -> Handle -> IO Pipe
inChanPipe c h = openPipe c (toWriteHandle h)

openPipe :: ReadHandle -> WriteHandle -> IO Pipe
openPipe r w = do -- read (output) handle, write (input) handle
  mv <- newEmptyMVar
  forkIO $ joinHandles r w $ wClose w >> putMVar mv () >> return ()
  return $ Pipe mv

waitForPipe :: Pipe -> IO ()
waitForPipe (Pipe mv) = takeMVar mv
\end{code}