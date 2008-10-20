\chapter{Expansions module}

Here is where we do some expansions.  Particularly, we just substitute
environment variables and \verb|~|.

\begin{code}

module System.Console.ShSh.Expansions ( shellExpansions ) where

import Data.Char ( isAlphaNum )
import Data.List ( takeWhile, dropWhile )

import System.Console.ShSh.Shell ( Shell, getEnv, tryEnv )

shellExpansions :: String -> Shell String
shellExpansions s = se "" s

se s "" = return s
se s ('~':cs) = do mh <- getEnv "HOME"
                   case mh of
                     Just h  -> se (s++h) cs
                     Nothing -> se (s++"~") cs
se s ('$':cs) = do let var = takeWhile isAlphaNum cs
                       rest = dropWhile isAlphaNum cs
                       (var',rest') = if null var
                                      then (take 1 rest,drop 1 rest)
                                      else (var,rest)
                   v <- tryEnv var
                   se (s++v) rest'
se s (c:cs) = se (s++[c]) cs

\end{code}