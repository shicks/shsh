{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Cp ( cp ) where

import System.Console.ShSh.Builtins.Args ( withArgs, flagOn, flag )
import System.Console.ShSh.IO ( ePutStrLn )
import System.Console.ShSh.Shell ( Shell )

import Data.List ( delete )
import System.FilePath ( (</>), takeFileName )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import System.Directory ( copyFile, doesDirectoryExist, createDirectory,
                          getDirectoryContents, getPermissions, setPermissions )
import System.Exit ( ExitCode(..) )

{-# NOINLINE cp #-}
cp :: [String] -> Shell ExitCode
cp xs = withArgs "cp" header args run xs >> return ExitSuccess
    where run []  = fail "missing operand"
          run [s] = fail $ "missing destination operand after `"++s++"'"
          run fs  = do let dest = last fs
                       isdir <- liftIO $ doesDirectoryExist dest
                       case fs of
                         [s,_] | not isdir && last dest /= '/' -> cp1 s dest
                         _ -> do when (not isdir) $ fail $
                                   "target `"++dest++"' is not a directory."
                                 mapM_ (\s -> cp1 s (dest</>takeFileName s)) $
                                       init fs
          cp1 s d = do amVerbose <- flag 'v'
                       when amVerbose $ ePutStrLn $ unwords ["cp",s,d]
                       amR <- flag 'r'
                       isdir <- if amR then liftIO $ doesDirectoryExist s
                                       else return False
                       if isdir
                          then do liftIO $ do createDirectory d
                                              setPermissions d =<<
                                                      getPermissions s
                                  subfs <- liftIO $ getDirectoryContents s
                                  mapM_ (\ss -> cp1 (s</>ss) (d</>ss)) $
                                        delete "." $ delete ".." subfs
                          else liftIO $ copyFile s d
          args = [flagOn "rR" ["recursive"] 'r' "copy directories recursively",
                  flagOn "v" ["verbose"] 'v'
                             "print a message for each file copied"]
          header = "Usage: cp [OPTION]... SOURCE DEST\n\
                   \  or:  cp [OPTION]... SOURCE... DIRECTORY\n\
                   \Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY."