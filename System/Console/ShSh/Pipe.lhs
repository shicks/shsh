\chapter{Pipe module}

This will need a lot of work, both on interface and implementation.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Pipe ( Pipe, pipe, openPipe,
                                  waitForPipe, waitForPipes,
                                  pipeOutput, pipeOutputInput ) where

import Control.Concurrent
import Control.Monad.Trans ( liftIO )

import Data.Word ( Word8 )
import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.Environment
import System.Exit
import System.IO
import System.Process

import System.Console.ShSh.Shell ( Shell, getAllEnv )

newtype Pipe = Pipe (Ptr Word8, MVar ())

doPipe :: Ptr Word8 -> MVar () -> Bool -> Handle -> Handle -> IO ()
doPipe buf v close r w 
               = catch (do hWaitForInput r (-1)
                           len <- hGetBufNonBlocking r buf bufferSize
                           hPutBuf w buf len
                           hFlush w
                           doPipe buf v close r w
                       ) $
                 \e -> do eof <- hIsEOF r
                          if eof then maybeClose w >> putMVar v ()
                                 else do putStrLn $ "Caught error: "++show e
                                         doPipe buf v close r w
    where maybeClose w = if close then hClose w else return ()

bufferSize = 4096

pipe :: Bool -> Handle -> Handle -> IO ()
pipe close r w = do -- read (output) handle, write (input) handle
  buf <- mallocBytes bufferSize
  mv <- newEmptyMVar
  forkIO $ doPipe buf mv close r w
  takeMVar mv
  free buf

openPipe :: Bool -> Handle -> Handle -> IO Pipe
openPipe close r w = do -- read (output) handle, write (input) handle
  buf <- mallocBytes bufferSize
  mv <- newEmptyMVar
  forkIO $ doPipe buf mv close r w
  return $ Pipe (buf,mv)

waitForPipe :: Pipe -> IO ()
waitForPipe (Pipe (buf,mv)) = do
  takeMVar mv
  free buf

waitForPipes :: [Pipe] -> IO ()
waitForPipes = mapM_ waitForPipe

pipeOutput :: FilePath -> [String] -> Handle -> Shell (ExitCode,[Pipe])
pipeOutputInput :: FilePath -> [String] -> Handle ->
                   Shell (Handle,ProcessHandle,[Pipe])
#ifdef HAVE_CREATEPROCESS
pipeOutput cmd args h = do
  env <- getAllEnv
  (_, Just h', _, pid) <- liftIO $ createProcess $
                          (proc cmd args) { env = Just env,
                                            std_out = CreatePipe }
  liftIO $ pipe False h' h
  ec <- liftIO $ waitForProcess pid
  return (ec,[])
pipeOutputInput cmd args h = do
  env <- getAllEnv
  (Just hi, Just h', _, pid) <- liftIO $ createProcess $
                                (proc cmd args) { env = Just env,
                                                  std_in = CreatePipe,
                                                  std_out = CreatePipe }
  pipe <- liftIO $ openPipe False h' h
  return (hi,pid,[pipe])
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