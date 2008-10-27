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
import Data.Maybe ( isJust, fromJust )

-- |We define a channel actually as a tuple of a 'Chan' and an 'MVar'.
-- This is mainly so that we can deal with closing write handles in a
-- consistent and sane way by setting the 'MVar', since there's no
-- way to "unWrite" a 'Chan'.
newtype Chan = A (C.Chan (Maybe B.ByteString)) (MVar ())

-- Soon maybe we'll change this to use empty bytestrings instead...?

-- Internals
ch :: Chan -> Chan (Maybe B.ByteString)
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
getChanContents c = takeWhile isJust . C.getChanContents . ch

-- |Note that we're okay /reading/ from a closed channel, as long as we're
-- not at empty.
readChan :: Chan -> IO B.ByteString
readChan c = fromJust `fmap` C.readChan . ch

-- |We're not allowed to write to the @Chan@ if it's closed, and the only
-- way to write a 'Nothing' is to close it.
writeChan :: Chan -> B.ByteString -> IO ()
writeChan (A c v) = do closed <- isEmptyMVar v
                       if closed then fail "write to closed Chan"
                                 else writeChan c . Just

unGetChan :: Chan -> B.ByteString -> IO ()
unGetChan c = C.unGetChan c . Just

-- |Here we interface with the messy part of @unGet@ting 'Nothing's.
isEOFChan :: Chan -> IO Bool
isEOFChan c = do e <- C.isEmptyChan c
                 if e then return True
                      else do b <- C.readChan c
                              C.unGetChan c b
                              return $ isJust b

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