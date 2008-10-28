\chapter{IO Module}

These are the public bindings to the Internal.IO module.

We'll also deal with things like waiting for pipes here, probably.
The question is how much of the Shell API do we want to expose in
order to keep this module separate?  I.e. all this messy PipeState
business...?  Get?  Put?  Maybe define a class in Internal.IO for
how to get at the handles?

\begin{code}

module System.Console.ShSh.IO ( iGetContents, iGetContentsBS, oPut, ePut,
                                iGetChar, iGetLine, iIsEOF,
                                oPutChar, ePutChar, oPutStr, ePutStr,
                                oPutStrLn, ePutStrLn, oFlush, eFlush,
                                iUnsafeClose, oUnsafeClose, eUnsafeClose,
                                iClose, oClose, eClose ) where

import Control.Monad.Trans ( liftIO )
import qualified Data.ByteString.Lazy as B

import System.Console.ShSh.Shell ( ShellT, iHandle, oHandle, eHandle, (.~) )
import System.Console.ShSh.Internal.IO ( rGetContents, rGetContentsBS,
                                         rGetChar, rGetLine, rIsEOF,
                                         wPutChar, wPutStr, wPutStrLn,
                                         wPut, rClose, wFlush, wClose,
                                         wSafeClose, rSafeClose )

infixr 7 >>>=
(>>>=) :: ShellT e a -> (a -> IO b) -> ShellT e b
f >>>= g = f >>= \a -> liftIO $ g a

iGetContentsBS :: ShellT e B.ByteString
iGetContentsBS = iHandle >>>= rGetContentsBS

iGetContents :: ShellT e String
iGetContents = iHandle >>>= rGetContents

iGetChar :: ShellT e Char
iGetChar = iHandle >>>= rGetChar

iGetLine :: ShellT e String
iGetLine = iHandle >>>= rGetLine

iIsEOF :: ShellT e Bool
iIsEOF = iHandle >>>= rIsEOF

oPut :: B.ByteString -> ShellT e ()
oPut c = oHandle >>>= wPut .~ c

ePut :: B.ByteString -> ShellT e ()
ePut c = eHandle >>>= wPut .~ c

oPutChar :: Char -> ShellT e ()
oPutChar c = oHandle >>>= wPutChar .~ c

ePutChar :: Char -> ShellT e ()
ePutChar c = eHandle >>>= wPutChar .~ c

oPutStr :: String -> ShellT e ()
oPutStr s = oHandle >>>= wPutStr .~ s

ePutStr :: String -> ShellT e ()
ePutStr s = eHandle >>>= wPutStr .~ s

oPutStrLn :: String -> ShellT e ()
oPutStrLn s = oHandle >>>= wPutStrLn .~ s

ePutStrLn :: String -> ShellT e ()
ePutStrLn s = eHandle >>>= wPutStrLn .~ s

oFlush :: ShellT e ()
oFlush = oHandle >>>= wFlush

eFlush :: ShellT e ()
eFlush = eHandle >>>= wFlush

iClose :: ShellT e ()
iClose = iHandle >>>= rSafeClose

oClose :: ShellT e ()
oClose = oHandle >>>= wSafeClose

eClose :: ShellT e ()
eClose = eHandle >>>= wSafeClose

iUnsafeClose :: ShellT e ()
iUnsafeClose = iHandle >>>= rClose

oUnsafeClose :: ShellT e ()
oUnsafeClose = oHandle >>>= wClose

eUnsafeClose :: ShellT e ()
eUnsafeClose = eHandle >>>= wClose

\end{code}