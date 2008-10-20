\chapter{Pipe module}

This will need a lot of work, both on interface and implementation.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Pipe ( pipe, openPipe,
                                  waitForPipe, pipeOutput ) where

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

openPipe :: Bool -> Handle -> Handle -> IO (Ptr Word8, MVar ())
openPipe close r w = do -- read (output) handle, write (input) handle
  buf <- mallocBytes bufferSize
  mv <- newEmptyMVar
  forkIO $ doPipe buf mv close r w
  return (buf,mv)

waitForPipe :: (Ptr Word8, MVar ()) -> IO ()
waitForPipe (buf,mv) = do
  takeMVar mv
  free buf

pipeOutput :: FilePath -> [String] -> Handle -> Shell ExitCode
#ifdef HAVE_CREATEPROCESS
pipeOutput cmd args h = do
  env <- getAllEnv
  (_, Just h', _, pid) <- liftIO $ createProcess $
                          (proc cmd args) { env = Just env,
                                            std_out = CreatePipe }
  liftIO $ pipe False h' h
  liftIO $ waitForProcess pid
#else
pipeOutput cmd args _ = do
  env <- getAllEnv
  pid <- liftIO $ runProcess cmd args Nothing (Just env)
                                      Nothing Nothing Nothing
  liftIO $ waitForProcess pid
#endif

\end{code}