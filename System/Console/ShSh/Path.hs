{-# LANGUAGE PatternGuards, CPP #-}
{-# OPTIONS_GHC -Wall #-}

module System.Console.ShSh.Path ( findExecutable, fixExports ) where

-- |Here is where we put all the stuff related to path lookups, including
-- the messy platform-independence issues.

import Control.Applicative ( (<|>) )
import Control.Monad ( msum, mplus, guard, (=<<) )
import Control.Monad.State ( get, modify )
import Control.Monad.Trans ( liftIO )
import Data.Maybe ( fromMaybe )
import System.Directory ( doesFileExist, getPermissions, Permissions(..) )
import System.FilePath ( (</>) )
#ifdef WINDOWS
import System.FilePath ( isDrive, (<.>) )
#endif

import System.Console.ShSh.Shell ( ShellT, getEnv, withSubState )

-- |Find an executable in the path.  We'll also check here whether the
-- input is a full-qualified path, and if so, we'll short circuit to simply
-- returning it if it exists and is executable.
-- (I implement this using withSubState because it's convenient, not pretty)
findExecutable :: String -> ShellT e String
findExecutable e = withSubState `flip` (Nothing::Maybe String) $
                   if hasDirs e
                   then find'' [] e -- just check one.
                   else find' =<< splitPath =<<
                            fromMaybe "" `fmap` getEnv "PATH"
    where find' :: [String] -> ShellT (Maybe String) String
          find' [] = fail . maybe (e++ ": No such file or directory")
                                  ( ++ ": Permission denied") =<< get
          find' (p:ps) = find'' ps $ p </> e
          find'' :: [String] -> String -> ShellT (Maybe String) String
          find'' ps f = do poss <- possibilities f
                           msum (map try poss) `mplus` find' ps
          try :: String -> ShellT (Maybe String) String
          try f = do guard =<< liftIO (doesFileExist f)
                     modify $ (<|> Just f) -- save first filename exists
                     guard =<< executable `fmap` liftIO (getPermissions f)
                     return f
#ifndef WINDOWS
          possibilities f = return [f]
          hasDirs f = '/' `elem` f
#else
          possibilities f = do pathext <- splitPath .
                                            fromMaybe ".com;.exe;.bat;.cmd"
                                              =<< getEnv "PATHEXT"
                               return $ f:map (f++) pathext
          hasDirs f = '/' `elem` f || '\\' `elem` f
#endif

splitBy' :: (a -> Bool) -> [a] -> ([a],[[a]]) -> [[a]]
splitBy' _ [] (s,ss) = ss++[s]
splitBy' f (c:cs) (s,ss)
#ifdef WINDOWS
       | isDrive $ s++[c] = splitBy' f cs (s++[c],ss) -- special dispensation
#endif
       | f c, null s      = splitBy' f cs ([],ss) -- clean up :: (dangerous)
       | f c, otherwise   = splitBy' f cs ([],ss++[s])
       | otherwise        = splitBy' f cs (s++[c],ss)

splitPath :: String -> ShellT e [String]
splitPath s = do pathsep <- fromMaybe ":" `fmap` getEnv "PATHSEP"
                 return $ splitBy' (`elem` pathsep) s ([],[])

-- |This function does something with the exports so that it's compatible
-- with what windows programs expect...?  Maybe it'll go somewhere else?
fixExports :: [String] -> ShellT e [String]
fixExports = return
