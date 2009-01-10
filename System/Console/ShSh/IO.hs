-- |These are the public bindings to the @Internal.IO@ module.

-- |We'll also deal with things like waiting for pipes here, probably.
-- The question is how much of the Shell API do we want to expose in
-- order to keep this module separate?  I.e. all this messy @PipeState@
-- business...?  @Get@?  @Put@?  Maybe define a class in @Internal.IO@ for
-- how to get at the handles?

module System.Console.ShSh.IO ( MonadSIO, iHandle, oHandle, eHandle,
                                iGetContents, iGetContentsBS, oPut, ePut,
                                iGetChar, iGetLine, iIsEOF,
                                oPutChar, ePutChar, oPutStr, ePutStr,
                                oPutStrLn, ePutStrLn, oFlush, eFlush,
                                iUnsafeClose, oUnsafeClose, eUnsafeClose,
                                iClose, oClose, eClose,
                                iIsOpen, oIsOpen, eIsOpen ) where

import Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy as B

import System.IO ( stdin, stdout, stderr )
import System.Console.ShSh.Internal.IO ( ReadHandle, WriteHandle,
                                         toReadHandle, toWriteHandle,
                                         rGetContents, rGetContentsBS,
                                         rGetChar, rGetLine, rIsEOF,
                                         wPutChar, wPutStr, wPutStrLn,
                                         wPut, rClose, wFlush, wClose,
                                         wSafeClose, rSafeClose,
                                         rIsOpen, wIsOpen )

-- |A MonadSIO is basically something that defines the three handles
-- we want.  This abstraction allows us to decouple from Shell and
-- therefore be included by /any/ module, regardless of where it is.
class MonadIO m => MonadSIO m where
    iHandle :: m ReadHandle
    oHandle :: m WriteHandle
    eHandle :: m WriteHandle

instance MonadSIO IO where
    iHandle = return $ toReadHandle stdin
    oHandle = return $ toWriteHandle stdout
    eHandle = return $ toWriteHandle stderr

infixr 7 >>>=
(>>>=) :: (Show a,MonadSIO m) => m a -> (a -> IO b) -> m b
f >>>= g = f >>= \a -> liftIO $ g a
-- f >>>= g = f >>= \a -> liftIO $ putStrLn ("IO: "++show a) >> g a

iGetContentsBS :: MonadSIO m => m B.ByteString
iGetContentsBS = iHandle >>>= rGetContentsBS

iGetContents :: MonadSIO m => m String
iGetContents = iHandle >>>= rGetContents

iGetChar :: MonadSIO m => m Char
iGetChar = iHandle >>>= rGetChar

iGetLine :: MonadSIO m => m String
iGetLine = iHandle >>>= rGetLine

iIsEOF :: MonadSIO m => m Bool
iIsEOF = iHandle >>>= rIsEOF

oPut :: MonadSIO m => B.ByteString -> m ()
oPut c = oHandle >>>= flip wPut c

ePut :: MonadSIO m => B.ByteString -> m ()
ePut c = eHandle >>>= flip wPut c

oPutChar :: MonadSIO m => Char -> m ()
oPutChar c = oHandle >>>= flip wPutChar c

ePutChar :: MonadSIO m => Char -> m ()
ePutChar c = eHandle >>>= flip wPutChar c

oPutStr :: MonadSIO m => String -> m ()
oPutStr s = oHandle >>>= flip wPutStr s

ePutStr :: MonadSIO m => String -> m ()
ePutStr s = eHandle >>>= flip wPutStr s

oPutStrLn :: MonadSIO m => String -> m ()
oPutStrLn s = oHandle >>>= flip wPutStrLn s

ePutStrLn :: MonadSIO m => String -> m ()
ePutStrLn s = eHandle >>>= flip wPutStrLn s

oFlush :: MonadSIO m => m ()
oFlush = oHandle >>>= wFlush

eFlush :: MonadSIO m => m ()
eFlush = eHandle >>>= wFlush

iClose :: MonadSIO m => m ()
iClose = iHandle >>>= rSafeClose

oClose :: MonadSIO m => m ()
oClose = oHandle >>>= wSafeClose

eClose :: MonadSIO m => m ()
eClose = eHandle >>>= wSafeClose

iUnsafeClose :: MonadSIO m => m ()
iUnsafeClose = iHandle >>>= rClose

oUnsafeClose :: MonadSIO m => m ()
oUnsafeClose = oHandle >>>= wClose

eUnsafeClose :: MonadSIO m => m ()
eUnsafeClose = eHandle >>>= wClose

iIsOpen :: MonadSIO m => m Bool
iIsOpen = iHandle >>>= rIsOpen

oIsOpen :: MonadSIO m => m Bool
oIsOpen = oHandle >>>= wIsOpen

eIsOpen :: MonadSIO m => m Bool
eIsOpen = eHandle >>>= wIsOpen
