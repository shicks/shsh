\chapter{Internal.IO Module}

This is the new version of what I called PipeIO before.  This
module does \emph{not} depend on Shell, and therefore Shell can
depend on it.

\begin{code}

-- |This module defines a bunch of useful IO constructs, including
-- basically wrapping all the original IO 'Handle's into a new
-- 'ShellHandle' type, which is useful because
--   (1) it allows a pair of 'Chan's to be used for piping, and
--   (2) it makes a distinction between read and write handles.
-- The basic idea is that if we're reimplementing this stuff anyway,
-- we might as well do it right.
module System.Console.ShSh.Internal.IO (
  ReadHandle, WriteHandle, ShellHandle, newPipe
  rGetContents, rGetChar, rGetLine, rWaitForInput, rGetNonBlocking,
  rClose, rSafeClose, rIsOpen, rIsClosed, rIsEOF,
  wPut, wPutChar, wPutStr, wPutStrLn, wFlush,
  wClose, wSafeClose, wIsOpen, wIsClosed,
  joinHandles ) where

import Control.Concurrent ( MVar, newEmptyMVar, takeMVar, putMVar )
import Control.Concurrent.Chan ( Chan, newChan,
                                 isEmptyChan, getChanContents,
                                 readChan, writeChan, unGetChan )

import qualified Data.ByteString.Lazy as B

import System.IO ( Handle, hFlush, hClose,
                   hIsEOF, hIsOpen, hIsClosed,
                   hGetContents, hGetChar, hGetLine,
                   hPutChar, hPutStr, hPutStrLn
                 )

import System.Console.ShSh.Internal.Chan ( Chan, newChan,
                                           isEmptyChan, getChanContents,
                                           readChan, writeChan )

-- * Data Types

-- |These two types are the main read/write handle wrappers.  We use
-- a lazy 'ByteString' in the channel, and it seems to be pretty
-- efficient.
data ReadHandle = RChan ShellChan | RHandle Handle
data WriteHandle = WChan ShellChan | WHandle Handle

-- |I'm not sure how useful this is as an exported type, but it might be.
-- We might also make it into a class, and then generalize operations like
-- hFlush, hIsOpen, hClose, etc, in the class instances.
type ShellHandle = Either ReadHandle WriteHandle

-- |This is the only exposed API for constructing handles out of thin air.
newPipe :: IO (ReadHandle,WriteHandle)
newPipe = do c <- newShellChan
             return (RChan c,WChan c)

-- * Operations
-- ** 'ReadHandle' operations
rGetContents :: ReadHandle -> IO String
rGetContents (RChan c) = do bs <- getChanContents (ch c) -- what about EOF?
                            return $ B.unpack $ B.concat bs
                            -- forkIO $ seq (length bs) $ put c Nothing ????
rGetContents (RHandle h) = hGetContents h

-- |In case it's not obvious already that @getChar@ needs a read handle...
rGetChar :: ReadHandle -> IO Char
rGetChar (RChan c) = do eof <- isEOFChan c
                        when eof fail "read from closed channel"
                        b <- readChan c
                        Just b' -> if B.null b'
                                      then rGetChar $ RChan c
                                      else do let (x,rest) = B.splitAt 1 b'
                                              unGetChan c $ Just rest
                                              return $ head $ B.unpack x
rGetChar (RHandle h) = hGetChar h

rGetLine :: ReadHandle -> IO String
rGetLine (RHandle h) = hGetLine h
rGetLine (RChan c) = notEOFChan "rGetLine" c gl'
gl' c = do -- getLine helper...
  eof <- isEOFChan c
  empty <- isEmptyChan c
  if eof || empty
     then return ""
     else do b <- readChan c
             if B.null b'
                then shGetLine $ SChan c
                else let l    = head $ B.lines b'
                         len  = B.length l
                         rest = B.drop len b'
                         s    = B.unpack l
                         r'   = B.drop 1 rest
                     in if B.null rest
                        then fmap (s++) $ gl' c
                        else do when (not $ B.null r') $ unGetChan c r'
                                return s

rWaitForInput :: ReadHandle -> IO () -- simulates hWaitForInput h -1
rWaitForInput (RHandle h) = hWaitForInput h (-1) >> return ()
rWaitForInput (RChan c) = notEOFChan "rWaitForInput" $ do
                            empty <- isEmptyChan
                            if empty
                               then yield >> rWaitForInput $ RChan c
                               else return ()

rGetNonBlocking :: ReadHandle -> Int -> IO B.ByteString
rGetNonBlocking (RHandle h) s = B.hGetNonBlocking h s
rGetNonBlocking (RChan c) s = notEOF "rGetNonBlocking" (gnb' c s)
gnb' s c = do -- getNonBlocking helper...
  eof <- isEOFChan c
  empty <- isEmptyChan c
  if eof || empty || s<=0
     then return B.empty
     else do bs <- readChan c
          let l = fromIntegral $ B.length bs
          in case compare l s of
               LT -> do rest <- gnb' (s-l) c
                        return $ B.append bs rest
               EQ -> return bs
               GT -> do let (ret,rest) = B.splitAt (fromIntegral s) bs
                        unGetChan c $ Just rest
                        return ret

-- This is apparently allowed...
rClose :: ReadHandle -> IO ()
rClose (RChan c) = closeChan c -- this is rather violent...
rClose (RHandle h) = hClose h

-- |This is like @rClose@ except that it takes advantage of
-- 'Handle''s 'Eq' instance to make sure that we're not closing
-- 'stdin'.
rSafeClose :: ReadHandle -> IO ()
rSafeClose (RHandle h) | h == stdin = return ()
                       | otherwise  = hClose h
rSafeClose (RChan c) = closeChan c

-- |This function is actually implemented /very/ differently depending
-- on whether it's a read or write handle.  For write handles, it's
-- closed as soon as we write the 'Nothing' to the back end.  For reading,
-- it stays open until the 'Nothing' comes out the front.
rIsOpen :: ReadHandle -> IO Bool
rIsOpen = not `fmap` rIsClosed

rIsClosed :: ReadHandle -> IO Bool
rIsClosed (RChan c) = isEOFChan c
rIsClosed (RHandle h) = hIsClosed h

rIsEOF :: ReadHandle -> IO Bool
rIsEOF (RHandle h) = hIsEOF h
rIsEOF h = rIsClosed

-- ** 'WriteHandle' operations
-- |These all behave like one would expect.
wPut :: WriteHandle -> B.ByteString -> IO ()
wPut (WChan c) = writeChan c
wPut (WHandle h) = B.hPut h

wPutChar :: WriteHandle -> Char -> IO ()
wPutChar (WChan c) = writeChan c . B.singleton
wPutChar (WHandle h) = hPutChar h

wPutStr :: WriteHandle -> String -> IO ()
wPutStr (WChan c) = writeChan c . B.pack
wPutStr (WHandle h) = hPutStr h

wPutStrLn :: WriteHandle -> String -> IO ()
wPutStrLn w s = wPutStr w (s++"\n")

wFlush :: WriteHandle -> IO ()
wFlush (WHandle h) = hFlush h
wFlush (WChan c) = requireOpenChan c

wClose :: WriteHandle -> IO ()
wClose (WChan c) = closeChan c Nothing
wClose (WHandle h) = hClose h

-- |This is like @wClose@ except that it takes advantage of
-- 'Handle''s 'Eq' instance to make sure that we're not closing
-- 'stdout' or 'stderr' by accident.
wSafeClose :: WriteHandle -> IO ()
wSafeClose (WHandle h) | h `elem` [stdout,stderr] = return ()
                       | otherwise = hClose h
wSafeClose (WChan c) = closeChan c

-- |This function is actually implemented /very/ differently depending
-- on whether it's a read or write handle.  For write handles, it's
-- closed as soon as we write the 'Nothing' to the back end.  For reading,
-- it stays open until the 'Nothing' comes out the front.
wIsOpen :: WriteHandle -> IO Bool
wIsOpen (WChan c) = isOpenChan c
wIsOpen (WHandle h) = hIsOpen h

wIsClosed :: WriteHandle -> IO Bool
wIsClosed = not `fmap` wIsOpen

-- *Pipes

-- This is internal
bufferSize = 4096

-- |This is a /much/ cleaner version of the old @doPipe@ function.  Here,
-- we take a job to do after the pipe closes.  This will likely be something
-- like closing the write end of the pipe and/or setting an @MVar@.
joinHandles :: ReadHandle  -- ^Read handle
            -> WriteHandle -- ^Write handle
            -> IO a        -- ^Job to do afterwards
            -> IO a
joinHandles r w job = catch (do rWaitForInput r
                                rGetNonBlocking r bufferSize >>= wPut w
                                wFlush w
                                joinHandles r w job) $
                      \e -> do eof <- rIsEOF r
                               if eof
                                  then job
                                  else do putStrLn $ "Caught error: "++show e
                                  -- this is for debugging--^  impossible?
                                          joinHandles r w job
\end{code}