\chapter{Builtins.Cd module}

\begin{code}

module System.Console.ShSh.Builtins.Cd ( chDir ) where

import Control.Monad.Trans ( liftIO )
import System.Console.ShSh.Shell ( Shell, tryEnv, setEnv )
import System.Directory ( getCurrentDirectory, doesDirectoryExist,
                          setCurrentDirectory )

chDir :: [String] -> Shell ()
chDir ("-":_) = do dir <- tryEnv "OLDPWD"
                   chDir' dir
chDir (dir:_) = do chDir' dir
chDir []      = do home <- tryEnv "HOME"
                   chDir' home

chDir' :: String -> Shell ()
chDir' dir = do exists <- liftIO $ doesDirectoryExist dir
                if exists
                   then go
                   else fail $ dir++": No such file or directory"
    where go = do olddir <- liftIO $ getCurrentDirectory
                  liftIO $ setCurrentDirectory dir
                  newdir <- liftIO $ getCurrentDirectory
                  setEnv "PWD" newdir
                  setEnv "OLDPWD" olddir

\end{code}