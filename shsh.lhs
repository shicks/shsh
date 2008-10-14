\documentclass{memoir}
\usepackage{shsh}

\title{shsh: The Simple Haskell Shell}
\begin{document}

\maketitle

\input Shell.lhs
\input EventLoop.lhs
\input Prompt.lhs

\chapter{Main}

Here's where we define main.  It's very simple.

\begin{code}

import System ( exitWith )
import System.Console.ShSh.Shell ( runShell )
import System.Console.ShSh.EventLoop ( eventLoop )

main = runShell eventLoop >>= exitWith

\end{code}

\end{document}
