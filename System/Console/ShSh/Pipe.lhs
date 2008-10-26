\chapter{Pipe module}

This is the higher-level module that interfaces with Shell.

In the absence of a sane createProcess, we'll failover to
\begin{itemize}
\item use non-interactive input rather than inheriting
\item use stderr rather than redirecting
\end{itemize}
It might be worth warning about these things (unless a suitable
environment variable is set?)

How do we do this in the case of two builtin commands?!?
We can't just start a cat process portably on Windows.  Say,
\verb+set | grep+?  We clearly want to FORK into the internal
in this case...  But how do we fork the shell...?  What to do
about the state?!?

It turns out that anything in a subshell (i.e. parens) can't
change the state.  And variable assignments aren't pipeable.
Also, anything in a pipeline can't change outside state either!!!
(compare \verb+set -C -o | grep noclobber && set -o | grep noclobber+)

So our semantic should be that we peel off all the layers of state,
do a forkIO, and then go back INTO the state to run the subprocesses.   

We'll also probably have to reimplement most of System.IO to run
with a new type, Either Handle (Chan String).  And we'll also
have to redo all of the process stuff too!

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Pipe ( runInShell, pipeShells, waitForPipes ) where

import Control.Concurrent
import Control.Concurrent.Chan

import Control.Monad.Trans ( liftIO )

import System.Environment
import System.Exit
import System.IO

import System.Process

import System.Console.ShSh.ShellError ( exit )
import System.Console.ShSh.Shell ( Shell, getAllEnv, sh_out,
                                   getPState, putPState, getShellState,
                                   pipeState, runShell )
import System.Console.ShSh.PipeIO

-- This takes care of all the handles for us!
runInShell :: CreateProcess -> Shell (Maybe ShellHandle, Maybe ShellHandle,
                                      Maybe ShellHandle, ProcessHandle)
#ifdef HAVE_CREATEPROCESS
runInShell p = do ps <- getPState
                  (ps',a,b,c,d) <- liftIO $ launch p ps
                  putPState ps'
                  return (a,b,c,d)
#else

#endif

-- This one is a bit frightening!
-- We can optimize this if one or both is a process...
-- call them pipeSS, pipeSP, pipePS, pipePP
-- or just make it take Either (Shell ()) CreateProcess as args
pipeShells :: Shell a -> Shell b -> Shell ()
pipeShells source dest = do
  state <- getShellState
  ps <- getPState
  h <- liftIO createSPipe
  let s = ps { p_out = SUseHandle h }
  let d = ps { p_in = SUseHandle h }
  liftIO $ forkIO $ runShell (putPState s >> source) state >> return ()
  ret <- liftIO $ runShell (putPState d >> dest) state
  case ret of
    ExitSuccess   -> return ()
    ExitFailure n -> exit n -- already announced...?

-- We could use some sort of dup'd broadcast channel for STDERR, and
-- maybe even something funny for STDIN?

waitForPipes :: Shell ()
waitForPipes = do ps <- getPState
                  liftIO $ waitForPipesIO $ openPipes ps
                  putPState $ ps { openPipes = [] }

pipeOutput :: FilePath -> [String] -> Handle -> Shell (ExitCode,[Pipe])
pipeOutputInput :: FilePath -> [String] -> Handle ->
                   Shell (Handle,ProcessHandle,[Pipe])
#ifdef HAVE_CREATEPROCESS
pipeOutput cmd args h = do
  env <- getAllEnv
  (_, Just h', _, pid) <- liftIO $ createProcess $
                          (proc cmd args) { env = Just env,
                                            std_out = CreatePipe }
  liftIO $ pipe' False h' h
  ec <- liftIO $ waitForProcess pid
  return (ec,[])
      where pipe' = undefined
pipeOutputInput cmd args h = do
  env <- getAllEnv
  (Just hi, Just h', _, pid) <- liftIO $ createProcess $
                                (proc cmd args) { env = Just env,
                                                  std_in = CreatePipe,
                                                  std_out = CreatePipe }
  pipe <- liftIO $ openPipe' False h' h
  return (hi,pid,[pipe])
      where openPipe' = undefined
#else
-- These don't actually do what they claim to do...
noPipe cmd args = do
  env <- getAllEnv
  pid <- liftIO $ runProcess cmd args Nothing (Just env) Nothing Nothing Nothing
  liftIO $ waitForProcess pid -- this is what pipeOutput did originally, and is
                              -- still useful because it has interactive input.

pipeOutput cmd args h = do
  env <- getAllEnv
  (i,o,e,pid) <- liftIO $ runInteractiveProcess cmd args Nothing (Just env)
  --ip <- liftIO $ openPipe False stdin i
  ep <- liftIO $ openPipe False e stderr
  liftIO $ pipe False o h
  ec <- liftIO $ waitForProcess pid
  return (ec,[ep]) --,ip
  
pipeOutputInput cmd args h = do
  env <- getAllEnv
  (i,o,e,pid) <- liftIO $ runInteractiveProcess cmd args Nothing (Just env)
  op <- liftIO $ openPipe False o h
  ep <- liftIO $ openPipe False e stderr
  return (i,pid,[op,ep])
#endif

\end{code}