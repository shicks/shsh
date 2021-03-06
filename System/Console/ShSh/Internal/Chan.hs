{-# OPTIONS_GHC -Wall #-}

-- |This is even lower-level than @Internal.IO@.  We just define a custom
-- Chan type and then redefine a few operations on it.

module System.Console.ShSh.Internal.Chan ( Chan, newChan, closeChan,
                                           isEmptyChan, getChanContents,
                                           readChan, writeChan, unGetChan,
                                           isEOFChan, isEOFChanNonBlocking,
                                           isOpenChan, requireOpenChan,
                                           notEOFChan
                                         ) where

import Control.Concurrent ( MVar, newEmptyMVar, isEmptyMVar,
                            putMVar, yield )
import System.IO.Unsafe ( unsafeInterleaveIO )

import qualified Control.Concurrent.Chan as C
import qualified Data.ByteString.Lazy as B

import Control.Monad ( unless )

-- |We define a channel actually as a tuple of a 'Chan' and an 'MVar'.
-- This is mainly so that we can deal with closing write handles in a
-- consistent and sane way by setting the 'MVar', since there's no
-- way to "unWrite" a 'Chan'.
data Chan = A (C.Chan B.ByteString) (MVar ())

instance Show Chan where
    showsPrec p _ = showsPrec p "Chan"

-- Internals
ch :: Chan -> C.Chan B.ByteString
ch (A c _) = c

mv :: Chan -> MVar ()
mv (A _ v) = v

-- |Exposed constructor
newChan :: IO Chan
newChan = do c <- C.newChan
             v <- newEmptyMVar
             return $ A c v

-- |This writes a 'Nothing' and sets the 'MVar'.
closeChan :: Chan -> IO ()
closeChan (A c v) = do requireOpenChan "closeChan" $ A c v
                       C.writeChan c B.empty
                       putMVar v ()

-- |Should this do any checking...?  It's sort of a read operation, so
-- it won't matter if the MVar is set.  What should it do on EOF?
isEmptyChan :: Chan -> IO Bool
isEmptyChan = C.isEmptyChan . ch

-- |Straightforward... Question about closing at the end (i.e. by @seq@ing
-- length and then calling close): what happens if we hGetContents and then
-- read the same handle from elsewhere before the pipe is closed???
getChanContents :: Chan -> IO B.ByteString
getChanContents (A c v) = unsafeInterleaveIO $
                          do bs <- C.readChan c
                             if B.null bs then do C.unGetChan c bs
                                                  return bs
                                          else do rest <- again
                                                  return $ B.append bs rest
    where again = getChanContents $ A c v

-- |Note that we're okay /reading/ from a closed channel, as long as we're
-- not at empty.
readChan :: Chan -> IO B.ByteString
readChan (A c _) = do b <- C.readChan c
                      if B.null b then do C.unGetChan c b
                                          fail "readChan: file closed"
                                  else return b

-- |We're not allowed to write to the @Chan@ if it's closed, and the only
-- way to write a 'Nothing' is to close it.
writeChan :: Chan -> B.ByteString -> IO ()
writeChan (A c v) b = do open <- isEmptyMVar v
                         unless open $ fail "writeChan: file closed"
                         unless (B.null b) $ C.writeChan c b

unGetChan :: Chan -> B.ByteString -> IO ()
unGetChan (A c _) b = unless (B.null b) $ C.unGetChan c b

-- |Here we interface with the messy part of @unGet@ting 'Nothing's.
isEOFChan :: Chan -> IO Bool
isEOFChan (A c v) = do e <- C.isEmptyChan c
                       if e then yield >> isEOFChan (A c v)
                            else do b <- C.readChan c
                                    C.unGetChan c b
                                    return $ B.null b

-- |This used to be non-blocking, but then we switched to make it block,
-- which messed us up when we wanted to do rIsOpen, since we don't expect
-- that one to block...  We might also want a way to separate out EOF
-- from closed?  Maybe to put a single EOF in the Chan but not close it?
isEOFChanNonBlocking :: Chan -> IO Bool
isEOFChanNonBlocking (A c _) = do e <- C.isEmptyChan c
                                  if e then return False
                                       else do b <- C.readChan c
                                               C.unGetChan c b
                                               return $ B.null b

-- |This one is applicable only when writing... the read end stays
-- open longer.
isOpenChan :: Chan -> IO Bool
isOpenChan = isEmptyMVar . mv

-- |This also only applies to writing.  Maybe we should make this more
-- explicit by defining WChan and RChan types?
requireOpenChan :: String -> Chan -> IO ()
requireOpenChan msg c = do open <- isOpenChan c
                           unless open $ fail $ msg ++ ": chan not open"

-- |This is a helper function for dealing with @Chan@s.  It does a single
-- check for EOF at the start, but then runs something without any more
-- checks.  Thus, future EOF checks can simply return empty, rather than
-- giving an error.  Unfortunately this generally requires defining a
-- clunky helper function which typically needs to be named in order to
-- call it recursively as we need to.
notEOFChan :: String -> Chan -> (Chan -> IO a) -> IO a
notEOFChan msg c job = do eof <- isEOFChan c
                          if eof then fail $ msg ++ ": end of file"
                                 else job c
