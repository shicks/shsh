-- |This is a simple module where we define generally useful functions.

module System.Console.ShSh.Util ( equating, split,
                                  update, updateWith ) where

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating p x y = p x == p y

-- |I don't know why this isn't in the standard library.
split :: Eq a => a -> [a] -> [[a]]
split c [] = [[]]
split c (c':cs) | c==c'     = []:split c cs
                | otherwise = case split c cs of
                                [] -> [[c]]
                                (s:ss) -> (c:s):ss

-- *Simple routines to update associative list elements.
update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update x y [] = [(x,y)]
update x y ((x',y'):xs) | x'==x     = (x',y):xs
                        | otherwise = (x',y'):update x y xs
-- ^We could use unionBy (equating fst) [(x,y)] here...

updateWith :: Eq a => a -> (Maybe b -> b) -> [(a,b)] -> [(a,b)]
updateWith x f [] = [(x,f Nothing)]
updateWith x f ((x',y'):xs) | x'==x     = (x',f (Just y')):xs
                            | otherwise = (x',y'):updateWith x f xs
