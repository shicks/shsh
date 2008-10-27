\chapter{Internal.Process Module}

This is the other half of the new version of what I called PipeIO
before.  Again, this module does \emph{not} depend on Shell, and
therefore Shell can depend on it.

\begin{code}

-- |This module basically wraps 'System.Process' in a way that allows
-- us to use the 6.10 API even if we don't have the package.  We
-- hide all the dirty laundry in here.
module System.Console.ShSh.Internal.Process (
  Pipe, waitForPipe,
  PipeState(..), noPipes,
  ReadStream, rStreamToHandle,
  WriteStream, wStreamToHandle,
  launch
) where

-- |This type is used to keep track of pipes, for the purpose of waiting
-- on them.  We might want to make it an 'MVar' 'ShellHandle' instead, so
-- that we can close the write end of it manually, if we want.
newtype Pipe = Pipe (MVar ())

-- |This is basically copied directly out of the new 'System.Process' API,
-- except we need that 'UseHandle' can be a generalized 'ShellHandle'
-- instead, so we need to redefine the whole type.
data ShellStream = SInherit | SUseHandle ShellHandle | SCreatePipe

data PipeState = PipeState { p_in :: ShellStream,
                             p_out :: ShellStream,
                             p_err :: ShellStream,
                             openPipes :: [Pipe]
                           }

noPipes :: PipeState
noPipes = PipeState SInherit SInherit SInherit []

streamToHandle :: Handle -> ShellStream -> ShellHandle
streamToHandle h SInherit = SHandle h
streamToHandle _ (SUseHandle h) = h
streamToHandle _ (SCreatePipe) = undefined -- fail "Pipe not yet created!"
-- ^Should name this @fromStream@ (or @wFromStream@, etc, as in @fromMaybe@)

-- This is a tricky function...
launch :: CreateProcess -> PipeState -> IO (PipeState,
                                            Maybe ShellHandle,
                                            Maybe ShellHandle,
                                            Maybe ShellHandle,
                                            ProcessHandle)
launch pr ps = do let is = mkStdStream $ p_in  ps
                      os = mkStdStream $ p_out ps
                      es = mkStdStream $ p_err ps
                  (i,o,e,pid) <- createProcess $ pr { std_in = is,
                                                      std_out = os,
                                                      std_err = es }
                  (ih,ps') <- process i (p_in ps) ps False
                  (oh,ps'') <- process o (p_out ps) ps' True
                  (eh,ps''') <- process e (p_err ps) ps'' True
                  return (ps''',ih,oh,eh,pid)
    where mkStdStream SInherit = Inherit
          mkStdStream SCreatePipe = CreatePipe
          mkStdStream (SUseHandle (SHandle h)) = UseHandle h
          mkStdStream (SUseHandle (SChan c)) = CreatePipe
          process :: Maybe Handle -> ShellStream -> PipeState -> Bool
                  -> IO (Maybe ShellHandle, PipeState)
          process Nothing _ p _ = return (Nothing,p)
          process (Just h) SCreatePipe p _ = return (Just (SHandle h),p)
          process (Just h) (SUseHandle ch@(SChan c)) p dir = do
            pipe <- startPipe dir ch h
            return (Nothing,p { openPipes = openPipes p ++ [pipe] })
          startPipe True = outChanPipe
          startPipe False = inChanPipe

outChanPipe :: ShellHandle -> Handle -> IO Pipe
outChanPipe c h = openPipe True (SHandle h) c

inChanPipe :: ShellHandle -> Handle -> IO Pipe
inChanPipe c h = openPipe True c (SHandle h)

pipe :: Bool -> ShellHandle -> ShellHandle -> IO ()
pipe close r w = do -- read (output) handle, write (input) handle
  mv <- newEmptyMVar
  forkIO $ doPipe mv close r w
  takeMVar mv

openPipe :: Bool -> ShellHandle -> ShellHandle -> IO Pipe
openPipe close r w = do -- read (output) handle, write (input) handle
  mv <- newEmptyMVar
  forkIO $ doPipe mv close r w
  return $ Pipe mv

waitForPipe :: Pipe -> IO ()
waitForPipe (Pipe mv) = takeMVar mv

waitForPipesIO :: [Pipe] -> IO ()
waitForPipesIO = mapM_ waitForPipe

\end{code}