-- |This is the new version of what I called PipeIO before.  This
-- module does \emph{not} depend on Shell, and therefore Shell can
-- depend on it.

-- |This module defines a bunch of useful IO constructs, including
-- basically wrapping all the original IO 'Handle's into a new
-- 'ShellHandle' type, which is useful because
--   (1) it allows a pair of 'Chan's to be used for piping, and
--   (2) it makes a distinction between read and write handles.
-- The basic idea is that if we're reimplementing this stuff anyway,
-- we might as well do it right.
module System.Console.ShSh.Internal.IO (
  ReadHandle, WriteHandle, ShellHandle, newPipe,
  toWriteHandle, toReadHandle, fromWriteHandle, fromReadHandle,
  rGetContents, rGetContentsBS,
  rGetChar, rGetLine, rWaitForInput, rGetNonBlocking,
  rClose, rSafeClose, rIsOpen, rIsClosed, rIsEOF,
  wPut, wPutChar, wPutStr, wPutStrLn, wFlush,
  wClose, wSafeClose, wIsOpen, wIsClosed,
  joinHandles ) where

import Control.Concurrent ( MVar, newEmptyMVar, takeMVar, putMVar, yield )
import Control.Monad ( when, unless, ap )
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO ( Handle, hFlush, hClose,
                   hIsEOF, hIsOpen, hIsClosed,
                   hGetContents, hGetChar, hGetLine,
                   hPutChar, hPutStr, hPutStrLn,
                   hWaitForInput,
                   stdin, stdout, stderr
                 )

import System.Console.ShSh.Internal.Chan ( Chan, newChan, isEOFChan,
                                           closeChan, unGetChan, notEOFChan,
                                           isOpenChan, requireOpenChan,
                                           isEmptyChan, getChanContents,
                                           readChan, writeChan )
import System.Console.ShSh.Util ( whenM )

-- * Data Types

-- |These two types are the main read/write handle wrappers.  We use
-- a lazy 'ByteString' in the channel, and it seems to be pretty
-- efficient.
data ReadHandle = RChan Chan | RHandle Handle deriving ( Show )
data WriteHandle = WChan Chan | WHandle Handle deriving ( Show )

-- |We don't need these to be entirely opaque, but it would be nice to
-- not pattern match on them so much...
toReadHandle :: Handle -> ReadHandle
toReadHandle = RHandle

toWriteHandle :: Handle -> WriteHandle
toWriteHandle = WHandle

fromReadHandle :: ReadHandle -> Maybe Handle
fromReadHandle (RHandle h) = Just h
fromReadHandle _ = Nothing

fromWriteHandle :: WriteHandle -> Maybe Handle
fromWriteHandle (WHandle h) = Just h
fromWriteHandle _ = Nothing


-- |I'm not sure how useful this is as an exported type, but it might be.
-- We might also make it into a class, and then generalize operations like
-- hFlush, hIsOpen, hClose, etc, in the class instances.
type ShellHandle = Either ReadHandle WriteHandle

-- |This is the only exposed API for constructing handles out of thin air.
newPipe :: IO (ReadHandle,WriteHandle)
newPipe = do c <- newChan
             return (RChan c,WChan c)

-- * Operations
-- ** 'ReadHandle' operations
rGetContents :: ReadHandle -> IO String
rGetContents (RChan c) = B.unpack `fmap` getChanContents c -- what about EOF?
                            -- forkIO $ seq (length bs) $ put c Nothing ????
rGetContents (RHandle h) = hGetContents h

rGetContentsBS :: ReadHandle -> IO B.ByteString
rGetContentsBS (RChan c) = getChanContents c -- what about EOF?
                            -- forkIO $ seq (length bs) $ put c Nothing ????
rGetContentsBS (RHandle h) = B.hGetContents h

-- |In case it's not obvious already that @getChar@ needs a read handle...
rGetChar :: ReadHandle -> IO Char
rGetChar (RChan c) = do eof <- isEOFChan c
                        when eof $ fail "read from closed channel"
                        b <- readChan c
                        if B.null b
                           then rGetChar $ RChan c
                           else do let (x,rest) = B.splitAt 1 b
                                   unGetChan c rest
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
             if B.null b
                then rGetLine $ RChan c
                else let l    = head $ B.lines b
                         len  = B.length l
                         rest = B.drop len b
                         s    = B.unpack l
                         r'   = B.drop 1 rest
                     in if B.null rest
                        then fmap (s++) $ gl' c
                        else do unless (B.null r') $ unGetChan c r'
                                return s

rWaitForInput :: ReadHandle -> IO () -- simulates hWaitForInput h -1
rWaitForInput (RHandle h) = hWaitForInput h (-1) >> return ()
rWaitForInput (RChan c) = notEOFChan "rWaitForInput" c $ \c ->
                            do empty <- isEmptyChan c
                               when empty $ yield >> rWaitForInput (RChan c)

rGetNonBlocking :: ReadHandle -> Int -> IO B.ByteString
rGetNonBlocking (RHandle h) s = B.hGetNonBlocking h s
rGetNonBlocking (RChan c) s = notEOFChan "rGetNonBlocking" c $ gnb' s
gnb' s c = do -- getNonBlocking helper...
  eof <- isEOFChan c
  empty <- isEmptyChan c
  if eof || empty || s<=0
     then return B.empty
     else do bs <- readChan c
             let l = fromIntegral $ B.length bs
             case compare l s of
               LT -> do rest <- gnb' (s-l) c
                        return $ B.append bs rest
               EQ -> return bs
               GT -> do let (ret,rest) = B.splitAt (fromIntegral s) bs
                        unGetChan c rest
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
rIsOpen h = not `fmap` rIsClosed h

rIsClosed :: ReadHandle -> IO Bool
rIsClosed (RChan c) = isEOFChan c
rIsClosed (RHandle h) = hIsClosed h

rIsEOF :: ReadHandle -> IO Bool
rIsEOF (RHandle h) = hIsEOF h
rIsEOF c = rIsClosed c

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
wFlush (WHandle h) = whenM (hIsOpen h) (hFlush h)
wFlush (WChan c) = requireOpenChan "wFlush" c

wClose :: WriteHandle -> IO ()
wClose (WChan c) = closeChan c
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
wIsClosed h = not `fmap` wIsOpen h

-- *Pipes

-- This is internal
bufferSize = 4096

-- |This is a /much/ cleaner version of the old @doPipe@ function.  Here,
-- we take a job to do after the pipe closes.  This will likely be something
-- like closing the write end of the pipe and/or setting an 'MVar'.
-- In the future we may want to take two functions: one for in case the
-- read handle closes, the other for in case the write handle closes.
-- The former would likely involve passing a 'ThreadId' and killing
-- the source thread...?  We might not have the 'ThreadId' yet, in which
-- case we'd send it via an @MVar ThreadId@, or possibly even an
-- @MVar (IO ())@...
joinHandles :: ReadHandle  -- ^Read handle
            -> WriteHandle -- ^Write handle
            -> IO a        -- ^Job to do afterwards
            -> IO a
joinHandles r w job = catch (do rWaitForInput r
                                b <- rGetNonBlocking r bufferSize
                                wPut w b
                                wFlush w
                                joinHandles r w job) $
                      \e -> do eof <- rIsEOF r
                               if eof
                                  then job
                                  else do putStrLn $ "Caught error: "++show e
                                  -- this is for debugging--^  impossible?
                                          joinHandles r w job
