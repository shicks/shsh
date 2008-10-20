\chapter{Pipe module}

This will need a lot of work, both on interface and implementation.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Pipe ( Pipe, pipe, openPipe, waitForPipe,
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

newtype Pipe = Pipe (Maybe (Ptr Word8, MVar ()))

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
  return $ Pipe $ Just (buf,mv)

waitForPipe :: Pipe -> IO ()
waitForPipe (Pipe (Just (buf,mv))) = do
  takeMVar mv
  free buf
waitForPipe (Pipe Nothing) = return ()

pipeOutput :: FilePath -> [String] -> Handle -> Shell ExitCode
pipeOutputInput :: FilePath -> [String] -> Handle -> Shell (Handle,ProcessHandle,Pipe)
#ifdef HAVE_CREATEPROCESS
pipeOutput cmd args h = do
  env <- getAllEnv
  (_, Just h', _, pid) <- liftIO $ createProcess $
                          (proc cmd args) { env = Just env,
                                            std_out = CreatePipe }
  liftIO $ pipe False h' h
  liftIO $ waitForProcess pid
pipeOutputInput cmd args h = do
  env <- getAllEnv
  (Just hi, Just h', _, pid) <- liftIO $ createProcess $
                                (proc cmd args) { env = Just env,
                                                  std_in = CreatePipe,
                                                  std_out = CreatePipe }
  pipe <- liftIO $ openPipe False h' h
  return (hi,pid,pipe)
#else
-- These don't actually do what they claim to do...
pipeOutput cmd args _ = do
  env <- getAllEnv
  pid <- liftIO $ runProcess cmd args Nothing (Just env)
                                      Nothing Nothing Nothing
  liftIO $ waitForProcess pid
pipeOutputInput cmd args _ = do
  env <- getAllEnv
  pid <- liftIO $ runProcess cmd args Nothing (Just env)
                                      Nothing Nothing Nothing
  return (stdin,pid,Pipe Nothing)
#endif

\end{code}