{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Ls ( ls ) where

import System.Console.ShSh.IO ( oPutStr, oPutStrLn )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flagOn )

import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(..) )
import System.Console.GetOpt
import Data.List ( sort )
import System.Directory ( doesFileExist, getDirectoryContents )

ls :: [String] -> Shell ExitCode
ls = withArgs "ls" header args RequireOrder ls'
    where ls' [] = do fs <- liftIO (getDirectoryContents ".")
                      oPutStr $ unlines $ sort $
                              filter unboring fs
                      return ExitSuccess
          ls' [f] = do isf <- liftIO $ doesFileExist f
                       if isf
                        then oPutStrLn f
                        else do cs <- liftIO $ getDirectoryContents f
                                oPutStr $ unlines $ drop 2 $ sort cs
                       return ExitSuccess
          ls' fs =
              do x <- mapM (liftIO . lsf) fs
                 oPutStr $ unlines x
                 return ExitSuccess
          header = "Usage: ls [file]..."
          args = [flagOn "l" [] 'l' "this is ignored"]
          unboring ('.':_) = False
          unboring _ = True
          lsf f = do isf <- doesFileExist f
                     if isf
                        then return f
                        else do cs <- sort `fmap` getDirectoryContents f
                                return $ unlines ((f++":") : map ("  "++) (drop 2 cs))
