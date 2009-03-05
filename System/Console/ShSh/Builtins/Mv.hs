{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Mv ( mv ) where

import System.Console.ShSh.Builtins.Args ( withArgs, flagOn, flag )
import System.Console.ShSh.IO ( ePutStrLn )
import System.Console.ShSh.Shell ( Shell )

import System.FilePath ( (</>), takeFileName )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import System.Directory ( renameFile, renameDirectory, doesDirectoryExist )
import System.Exit ( ExitCode(..) )

{-# NOINLINE mv #-}
mv :: [String] -> Shell ExitCode
mv = withArgs "mv" header opts run
    where run []  = fail "missing operand"
          run [s] = fail $ "missing destination operand after `"++s++"'"
          run fs  = do let dest = last fs
                       isdir <- liftIO $ doesDirectoryExist dest
                       case fs of
                         [s,_] | not isdir && last dest /= '/' -> mv1 s dest
                         _ -> do when (not isdir) $
                                      fail $ "target `"++dest++
                                             "' is not a directory."
                                 mapM_ (\s -> mv1 s (dest</>takeFileName s)) $
                                       init fs
                       return ExitSuccess
          mv1 s d = do amV <- flag 'v'
                       when amV $ ePutStrLn $ unwords ["mv",s,d]
                       isd <- liftIO $ doesDirectoryExist s
                       if isd then liftIO $ renameDirectory s d
                              else liftIO $ renameFile s d
          opts = [flagOn "v" ["verbose"] 'v'
                             "print a message for each file moved"]
          header = "Usage: mv [OPTION]... SOURCE DEST\n\
                   \  or:  mv [OPTION]... SOURCE... DIRECTORY\n\
                   \Rename SOURCE to DEST, or move SOURCE(s) to DIRECTORY."
