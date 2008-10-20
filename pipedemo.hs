import Control.Concurrent

import Data.Word ( Word8 )
import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.Environment
import System.Exit
import System.IO
import System.Process

doPipe :: Ptr Word8 -> MVar () -> Handle -> Handle -> IO ()
doPipe buf v w r 
               = catch (do hWaitForInput w (-1)
                           len <- hGetBufNonBlocking w buf bufferSize
                           hPutBuf r buf len
                           hFlush r
                           doPipe buf v w r
                       ) $
                 \e -> do eof <- hIsEOF w
                          if eof then putMVar v ()
                                 else do putStrLn $ "Caught error: "++show e
                                         doPipe buf v w r
bufferSize = 4096

pipeHandles :: Handle -> Handle -> IO ()
pipeHandles w r = do
  buf <- mallocBytes bufferSize
  mv <- newEmptyMVar
  forkIO $ doPipe buf mv w r
  takeMVar mv
  free buf

writeProc :: String -> Handle -> IO ExitCode
writeProc s h = do
  (_, Just h', _, p) <- createProcess (shell s) { std_out = CreatePipe }
  hSetBuffering h' LineBuffering
  pipeHandles h' h
  waitForProcess p

main = do
  [o1,o2,i] <- getArgs
  (Just h, _, _, p) <- createProcess (shell i) { std_in = CreatePipe }
  hSetBuffering h LineBuffering -- should we also set binary?

  writeProc o1 h
  writeProc o2 h

  catch (hClose h) $ \e -> putStrLn $ "Error closing input handle: "++show e
  waitForProcess p -- bash pipes return status of *last* command in pipeline
                   -- (true | false) && echo 1  vs.  (false | true) && echo 1
                   -- note that even broken pipes don't do it:
                   --   echo 1 | true  vs.  (echo 1 | true) && echo 1
