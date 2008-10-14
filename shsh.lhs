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

import Shell ( runShell )
import EventLoop ( eventLoop )

main = runShell eventLoop

\end{code}

\end{document}