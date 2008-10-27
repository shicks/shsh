\chapter{IO Module}

These are the public bindings to the Internal.IO module.

We'll also deal with things like waiting for pipes here, probably.
The question is how much of the Shell API do we want to expose in
order to keep this module separate?  I.e. all this messy PipeState
business...?  Get?  Put?  Maybe define a class in Internal.IO for
how to get at the handles?

\begin{code}

module System.Console.ShSh.IO ( ) where

import System.Console.ShSh.Shell ( )
import System.Console.ShSh.Internal.IO ( )

iHandle :: Shell ReadHandle
oHandle :: Shell WriteHandle
eHandle :: Shell WriteHandle

iGetChar :: Shell Char
iGetLine :: Shell String
iIsEOF :: Shell Bool

oPutStr :: String -> Shell ()
ePutStr :: String -> Shell ()

oPutStrLn :: String -> Shell ()
ePutStrLn :: String -> Shell ()

oFlush :: Shell ()
eFlush :: Shell ()

iClose :: Shell ()
oClose :: Shell ()
eClose :: Shell ()

iUnsafeClose :: Shell ()
oUnsafeClose :: Shell ()
eUnsafeClose :: Shell ()

\end{code}