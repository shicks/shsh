\documentclass{memoir}
\usepackage{shsh}

\title{shsh: The Simple Haskell Shell}
\begin{document}

\maketitle

This is the Simple Haskell Shell.  It aims to provide a portable,
native, \texttt{sh} replacement, independent of tools such as cygwin.
This should be useful for, e.g. test scripts, among other things.

A specification of \texttt{sh} can be found at
\texttt{http://www.opengroup.org/onlinepubs/000095399/utilities/sh.html}.
The plan currently is to base the implementation off of the main points
of that specification.

\input Shell.lhs
\input EventLoop.lhs
\input Prompt.lhs

\chapter{Main}

Here's where we define main.  It's very simple.

\begin{code}

{-# OPTIONS_GHC -cpp #-}

import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs )
import System.Console.ShSh.Shell ( startShell )
import System.Console.ShSh.EventLoop ( eventLoop, sourceProfile, source )
import System.IO ( stdin, hIsTerminalDevice )

#ifdef HAVE_SIGNALS
import System.Posix.Signals ( Handler(..), installHandler, sigPIPE )
#endif

main = do args <- getArgs
          term <- hIsTerminalDevice stdin
          let h = if term then Nothing else Just stdin -- extend later w/ getopt
#ifdef HAVE_SIGNALS
          installHandler sigPIPE Ignore Nothing
#endif
          case args of
            [] -> startShell (sourceProfile >> eventLoop "" h) >>= exitWith
            [f] -> startShell (do sourceProfile
                                  source f
                                  return ExitSuccess) >>= exitWith
            fs -> fail $ unwords $ "Weird arguments: ":fs
\end{code}

\end{document}
