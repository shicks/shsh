{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Cd ( chDir ) where

import Control.Monad.Trans ( liftIO )
import Data.Maybe ( fromMaybe )
import System.Console.ShSh.Directory ( joinDirs )
import System.Console.ShSh.Shell ( Shell, getEnv, setEnv )

import System.Directory ( getCurrentDirectory, doesDirectoryExist,
                          setCurrentDirectory )
import System.Exit ( ExitCode(..) )

{-# NOINLINE chDir #-}
chDir :: [String] -> Shell ExitCode
chDir = cd' False False

usage :: String -> Shell ExitCode
usage opt = fail $ opt++": invalid option\n"++
            "Usage: cd [-L|-P] [dir]"

cd' :: Bool -> Bool -> [String] -> Shell ExitCode
cd' _ False ("-L":a) = cd' False False a  -- change to logical
cd' _ False ("-P":a) = cd' True  False a  -- change to physical
cd' a _ ("-":_)      = do dir <- getEnv "OLDPWD"
                          chDir' a dir
cd' a _ ("--":a')    = cd' a True a'      -- allow silly dirs
cd' _ False (opt@('-':_):_) = usage opt -- unknown option
cd' a _ []           = do home <- getEnv "HOME"
                          chDir' a home
cd' a _ (dir:_)      = do chDir' a dir

chDir' :: Bool -> String -> Shell ExitCode
chDir' a dir = do exists <- liftIO $ doesDirectoryExist dir
                  if exists
                     then go a
                     else fail $ dir++": No such file or directory"
    where go True = do oldpwd <- getEnv "PWD"
                       olddir <- liftIO $ getCurrentDirectory
                       liftIO $ setCurrentDirectory dir
                       newdir <- liftIO $ getCurrentDirectory
                       setEnv "PWD" newdir
                       setEnv "OLDPWD" $ fromMaybe olddir oldpwd
                       return ExitSuccess
          go False = do molddir <- getEnv "PWD"
                        olddir <- case molddir of
                                    Just olddir -> return olddir
                                    Nothing -> liftIO $ getCurrentDirectory
                        let newdir = joinDirs olddir dir
                        liftIO $ setCurrentDirectory newdir
                        setEnv "PWD" newdir
                        setEnv "OLDPWD" olddir
                        return ExitSuccess
