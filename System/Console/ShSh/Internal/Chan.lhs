\chapter{Internal.Chan module}

This is even lower-level than Internal.IO.  We just define a custom
Chan type and then redefine a few operations on it.

\begin{code}

module System.Console.ShSh.Internal.Chan ( Chan, newChan, closeChan,
                                           isEmptyChan, getChanContents,
                                           readChan, writeChan, unGetChan,
                                           isEOFChan,
                                           isOpenChan, requireOpenChan,
                                           notEOFChan
                                         ) where

import Control.Concurrent ( MVar, newEmptyMVar, isEmptyMVar,
                            takeMVar, putMVar )
import qualified Control.Concurrent.Chan as C

import qualified Data.ByteString.Lazy as B

import Control.Monad ( when )
import Data.List ( takeWhile )

-- |We define a channel actually as a tuple of a 'Chan' and an 'MVar'.
-- This is mainly so that we can deal with closing write handles in a
-- consistent and sane way by setting the 'MVar', since there's no
-- way to "unWrite" a 'Chan'.
newtype Chan = A (C.Chan B.ByteString) (MVar ())

-- Soon maybe we'll change this to use empty bytestrings instead...?

-- Internals
ch :: Chan -> Chan B.ByteString
ch = fst

mv :: Chan -> MVar ()
mv = snd

-- |Exposed constructor
newChan :: IO Chan
newChan = do c <- newChan
             v <- newEmptyMVar
             return $ A c v

-- |This writes a 'Nothing' and sets the 'MVar'.
closeChan :: Chan -> IO ()
closeChan (A c v) = do requireOpenChan (A c v)
                       C.writeChan c Nothing
                       putMVar v ()

-- |Should this do any checking...?  It's sort of a read operation, so
-- it won't matter if the MVar is set.  What should it do on EOF?
isEmptyChan :: Chan -> IO Bool
isEmptyChan c = C.isEmptyChan . ch

-- |Straightforward... Question about closing at the end (i.e. by @seq@ing
-- length and then calling close): what happens if we hGetContents and then
-- read the same handle from elsewhere before the pipe is closed???
getChanContents :: Chan -> IO [B.ByteString]
getChanContents c =  (B.concat . takeWhile (not . null))
                     `fmap` C.getChanContents . ch

-- |Note that we're okay /reading/ from a closed channel, as long as we're
-- not at empty.
readChan :: Chan -> IO B.ByteString
readChan c = do b <- C.readChan . ch
                if null b then fail "readChan: file closed"
                          else return b

-- |We're not allowed to write to the @Chan@ if it's closed, and the only
-- way to write a 'Nothing' is to close it.
writeChan :: Chan -> B.ByteString -> IO ()
writeChan (A c v) b = do closed <- isEmptyMVar v
                         if closed then fail "writeChan: file closed"
                                   else when (not $ null b) $ writeChan c

unGetChan :: Chan -> B.ByteString -> IO ()
unGetChan c = C.unGetChan c

-- |Here we interface with the messy part of @unGet@ting 'Nothing's.
isEOFChan :: Chan -> IO Bool
isEOFChan c = do e <- C.isEmptyChan c
                 if e then return True
                      else do b <- C.readChan c
                              C.unGetChan c b
                              return $ not $ null b

isOpenChan :: Chan -> IO Bool
isOpenChan = isEmptyMVar . mv

requireOpenChan :: Chan -> IO ()
requireOpenChan c = do open <- isOpenChan c
                       when (not open) $ fail "Chan not open"

-- |This is a helper function for dealing with @Chan@s.  It does a single
-- check for EOF at the start, but then runs something without any more
-- checks.  Thus, future EOF checks can simply return empty, rather than
-- giving an error.  Unfortunately this generally requires defining a
-- clunky helper function which typically needs to be named in order to
-- call it recursively as we need to.
notEOFChan :: String -> Chan -> (Chan -> IO a) -> IO a
notEOFChan msg c job = do eof <- rIsEOF c
                          if eof then fail $ msg ++ ": end of file"
                                 else job c


\end{code}