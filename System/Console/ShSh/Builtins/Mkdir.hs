{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Mkdir ( mkDir ) where

import System.Console.ShSh.Builtins.Args ( withArgs, optSet, opt,
                                           flagOn, flag )

import System.Console.ShSh.Directory ( parentDir )
import System.Console.ShSh.IO ( ePutStrLn )
import System.Console.ShSh.Shell ( Shell )

import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import System.Directory ( doesDirectoryExist, createDirectory )
import System.Exit ( ExitCode(..) )

#ifdef UNIX
import System.Posix.Files ( setFileMode )
-- import System.Console.ShSh.Unix ( setFileMode, fileMode )
#endif

-- We don't strictly need a substate here, but it doesn't really hurt much
{-# NOINLINE mkDir #-}
mkDir :: [String] -> Shell ExitCode
mkDir xs = withArgs "mkdir" header args (mapM_ mk) xs >> return ExitSuccess
    where mk d = do let die e = fail $ "cannot create directory `"++d++"': "++e
                    m <- opt 'm' "755"
                    p <- flag 'p'
                    v <- flag 'v'
                    dex <- liftIO $ doesDirectoryExist d
                    if dex
                        then if p
                             then return () -- okay w/ -p
                             else die "File exists"
                        else
                          do case parentDir d of
                               "" -> return () -- infinite loop on relative dirs
                               "/" -> return () -- no point trying to mkdir /
                               parent -> do pex <- liftIO $
                                                   doesDirectoryExist parent
                                            when (not pex) $ if p
                                                             then mk parent
                                                             else die nsfd
                             liftIO $ createDirectory d -- should catch here ...
#ifdef UNIX 
                             -- check for windows here and warn/ignore?
                             liftIO $ setFileMode d (read $ '0':'o':m)
#else
                             return m -- do nothing, but don't complain w/ -Wall
#endif
                             when v $ ePutStrLn $ created d
          nsfd = "No such file or directory"
          created d = "mkdir: created directory `"++d++"'"
          args = [optSet "m" ["mode"] 'm' "MODE"
                     "set file mode (as in chmod), not a=rwx - umask"
                 ,flagOn "p" ["parents"] 'p'
                     "no error if existing, make parent directories as needed"
                 ,flagOn "v" ["verbose"] 'v'
                     "print a message for each created directory"]
          header = "Usage: mkdir [OPTION] DIRECTORY...\n"++
                   "Create the DIRECTORY(ies), if they do not already exist."
