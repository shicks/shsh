{-# OPTIONS_GHC -Wall #-}
-- |Exports a few routines for dealing with directories.

module System.Console.ShSh.Directory ( parentDir, joinDirs ) where

import System.FilePath ( splitDirectories, joinPath )

-- |Removes the last directory from the path.  We could put this in
-- the IO monad and then deal with what happens when we're at the top?
parentDir :: FilePath -> FilePath
parentDir = joinPath . reverse . drop 1 . reverse . splitDirectories

-- |Joins two directories, removing all the '.'s and '..'s.
joinDirs :: FilePath -> FilePath -> FilePath
joinDirs a' b' = join' (splitDirectories a') (splitDirectories b')
    where join' a (".":b) = join' a b
          join' a ("..":b) = join' (take (length a - 1) a) b
          join' a (x:b) = join' (a++[x]) b
          join' a [] = joinPath a
