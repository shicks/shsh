\chapter{Redirection module}

\begin{code}

module System.Console.ShSh.Redirection ( Redir(..) ) where

v v v v v v v
data Redir = Int :> String
           | Int :>> String
           | Int :>| String
           | Int :< String
           | Int :<> String
           | Int :>& Int
           | Int :<& Int
           | Int :<< String
           | Int :<<- String
    deriving ( Show )
*************
data Redir = OutTo String | AppendTo String | InFrom String
             deriving ( Show )
^ ^ ^ ^ ^ ^ ^

\end{code}
