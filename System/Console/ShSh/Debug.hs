{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Debug where

import Control.Concurrent ( MVar, newMVar, takeMVar, putMVar )
import qualified Control.Concurrent as C
import System.IO.Unsafe ( unsafePerformIO )
import System.IO ( stderr, hPutStrLn )

lastID :: MVar Int
lastID = unsafePerformIO (newMVar 0)

getID :: IO Int
getID = do i <- (+1) `fmap` takeMVar lastID
           putMVar lastID i
           return i

traceForkIO :: String -> IO () -> IO C.ThreadId
traceForkIO name job
    = do i <- getID
         C.forkIO $ do hPutStrLn stderr $ "Starting thread "++show i++": "++name
                       job
                       hPutStrLn stderr $ "Ending thread "++show i
