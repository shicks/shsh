\chapter{Util module}

This is a simple module where we define generally useful functions.

\begin{code}

module System.Console.ShSh.Util ( equating, update, updateWith ) where

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating p x y = p x == p y

-- *Simple routines to update associative list elements.
update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update x y [] = [(x,y)]
update x y ((x',y'):xs) | x'==x     = (x',y):xs
                        | otherwise = (x',y'):update x y xs

updateWith :: Eq a => a -> (Maybe b -> b) -> [(a,b)] -> [(a,b)]
updateWith x f [] = [(x,f Nothing)]
updateWith x f ((x',y'):xs) | x'==x     = (x',f (Just y')):xs
                            | otherwise = (x',y'):updateWith x f xs

\end{code}