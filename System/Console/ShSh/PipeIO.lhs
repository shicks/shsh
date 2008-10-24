\chapter{PipeIO module}

This will need a lot of work, both on interface and implementation.
Okay, at this point we have three separate modules in order to get
all the interface hiding that we want...

This is all the low-level pipe maintenance code.

\begin{code}
module System.Console.ShSh.PipeIO ( PipeState(..),
                                    ShellHandle,
                                    Pipe, pipe, openPipe,
                                    waitForPipe, waitForPipes,
                                    launch, createSPipe,
                                    shGetContents,
                                    shPutStr, shPutStrLn,
                                    shFlush, shClose,
                                    shIsClosed, shIsOpen, shIsEOF,
                                    shGetChar, shGetLine,
                                    shWaitForInput, shGetNonBlocking, shPut
--                                    pipeOutput, pipeOutputInput
                                  ) where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char ( ord, chr )
import Data.Maybe ( catMaybes, isJust )
import Data.Word ( Word8 )
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr

import System.Environment
import System.Exit
import System.IO
import System.Process

newtype Pipe = Pipe (MVar ())

data ShellStream = SInherit | SUseHandle ShellHandle | SCreatePipe

data PipeState = PipeState { p_in :: ShellStream,
                             p_out :: ShellStream,
                             p_err :: ShellStream,
                             openPipes :: [Pipe]
                           }
noPipes :: PipeState
noPipes = PipeState SInherit SInherit SInherit []

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
outChanPipe c h = openPipe False (SHandle h) c

inChanPipe :: ShellHandle -> Handle -> IO Pipe
inChanPipe c h = openPipe False c (SHandle h)

doPipe :: MVar () -> Bool
       -> ShellHandle -> ShellHandle -> IO ()
doPipe v close r w
               = catch (do shWaitForInput r
                           shGetNonBlocking r bufferSize >>= shPut w
                           shFlush w
                           doPipe v close r w
                       ) $
                 \e -> do eof <- shIsEOF r
                          if eof then maybeClose w >> putMVar v ()
                                 else do putStrLn $ "Caught error: "++show e
                                         doPipe v close r w
    where maybeClose w = if close then shClose w else return ()

bufferSize = 4096

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
waitForPipe (Pipe mv) = do
  takeMVar mv

waitForPipes :: [Pipe] -> IO ()
waitForPipes = mapM_ waitForPipe

data ShellHandle = SChan (Chan (Maybe B.ByteString)) | SHandle Handle
-- For now we'll use strict bytestrings...

-- Should we distinguish read/write ShellHandles?

createSPipe :: IO ShellHandle
createSPipe = do c <- newChan
                 return $ SChan c

-- We could do better error checking by testing isEmptyMVar first...
shGetContents :: ShellHandle -> IO String
shGetContents (SChan c) = do bs <- getChanContents c
                             return $ B.unpack $ B.concat $ catMaybes bs
shGetContents (SHandle h) = hGetContents h

shPutChar :: ShellHandle -> Char -> IO ()
shPutChar (SChan c) = writeChan c . Just . B.singleton
shPutChar (SHandle h) = hPutChar h

shPutStr :: ShellHandle -> String -> IO ()
shPutStr (SChan c) = writeChan c . Just . B.pack
shPutStr (SHandle h) = hPutStr h

shPutStrLn :: ShellHandle -> String -> IO ()
shPutStrLn ch s = shPutStr ch (s++"\n")

shFlush :: ShellHandle -> IO ()
shFlush (SHandle h) = hFlush h
shFlush _ = return ()

shClose :: ShellHandle -> IO ()
shClose (SChan c) = writeChan c Nothing
shClose (SHandle h) = hClose h

shIsOpen :: ShellHandle -> IO Bool
shIsOpen (SChan c) = do e <- isEmptyChan c
                        if e then return True
                             else do b <- readChan c
                                     unGetChan c b
                                     return $ isJust b
shIsOpen (SHandle h) = hIsOpen h

shIsClosed :: ShellHandle -> IO Bool
shIsClosed (SChan c) = do e <- isEmptyChan c
                          if e then return False
                               else do b <- readChan c
                                       unGetChan c b
                                       return $ not $ isJust b
shIsClosed (SHandle h) = hIsClosed h

shIsEOF :: ShellHandle -> IO Bool
shIsEOF (SHandle h) = hIsEOF h
shIsEOF ch = shIsClosed ch

shGetChar :: ShellHandle -> IO Char
shGetChar (SChan c) = do b <- readChan c
                         case b of
                           Nothing -> fail "read from closed channel"
                           Just b' -> if B.null b'
                                      then shGetChar $ SChan c
                                      else do let (x,rest) = B.splitAt 1 b'
                                              unGetChan c $ Just rest
                                              return $ head $ B.unpack x
shGetChar (SHandle h) = hGetChar h

shGetLine :: ShellHandle -> IO String
shGetLine (SHandle h) = hGetLine h
shGetLine (SChan c) = do
  b <- readChan c
  case b of
    Nothing -> fail "read from closed channel"
    Just b' -> if B.null b'
               then shGetLine $ SChan c
               else do let l    = head $ B.lines b'
                           len  = B.length l
                           rest = B.drop len b'
                           s    = B.unpack l
                           r'   = B.drop 1 rest
                       if B.null rest
                          then fmap (s++) $ shGetLine $ SChan c
                          else do when (not $ B.null r') $ unGetChan c $ Just r'
                                  return s

shWaitForInput :: ShellHandle -> IO () -- simulates hWaitForInput h -1
shWaitForInput (SHandle h) = hWaitForInput h (-1) >> return ()
shWaitForInput ch@(SChan c) = do empty <- isEmptyChan c
                                 if empty
                                    then yield >> shWaitForInput ch
                                    else return ()

shGetNonBlocking :: ShellHandle -> Int -> IO B.ByteString
shGetNonBlocking (SHandle h) s = B.hGetNonBlocking h s
shGetNonBlocking ch@(SChan c) s = do
  empty <- isEmptyChan c
  if empty || s<=0
     then return B.empty
     else do x <- readChan c
             case x of
               Nothing -> do unGetChan c Nothing
                             return B.empty
               Just bs -> let l = fromIntegral $ B.length bs
                          in case compare l s of
                               LT -> do rest <- shGetNonBlocking ch $ s - l
                                        return $ B.append bs rest
                               EQ -> return bs
                               GT -> do let (ret,rest)
                                              = B.splitAt (fromIntegral s) bs
                                        unGetChan c $ Just rest
                                        return ret

shPut :: ShellHandle -> B.ByteString -> IO ()
shPut (SChan c) = writeChan c . Just
shPut (SHandle h) = B.hPut h
\end{code}