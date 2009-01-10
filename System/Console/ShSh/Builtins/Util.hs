{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Util ( orDash,
                                           readFileOrStdin,
                                           readFileOrStdinWithFilename,
                                           readFilesOrStdin,
                                           readFilesOrStdinWithFilename
                                         ) where

import System.Console.ShSh.Shell ( ShellT )
import System.Console.ShSh.IO ( iIsOpen, iGetContents )

import Control.Monad ( unless )
import Control.Monad.Trans ( liftIO )
import System.Directory ( doesFileExist, doesDirectoryExist )

orDash :: [String] -> [String]
orDash [] = ["-"]
orDash fs = fs

readFileOrStdin :: String -> ShellT e String
readFileOrStdin "-" = do open <- iIsOpen
                         if open then iGetContents else return ""
readFileOrStdin f = do exists <- liftIO $ doesFileExist f
                       unless exists $ noExist
                       liftIO $ readFile f
    where noExist = do isdir <- liftIO $ doesDirectoryExist f
                       fail $ if isdir then die "Is a directory"
                                       else die "No such file or directory"
          die msg = fail $ f ++ ": " ++ msg

readFileOrStdinWithFilename :: String -> ShellT e (String,String)
readFileOrStdinWithFilename f = (,) (fname f) `fmap` readFileOrStdin f
    where fname "-" = "(standard input)"
          fname fn = fn

readFilesOrStdin :: [String] -> ShellT e [String]
readFilesOrStdin = mapM readFileOrStdin . orDash

readFilesOrStdinWithFilename :: [String] -> ShellT e [(String,String)]
readFilesOrStdinWithFilename = mapM readFileOrStdinWithFilename . orDash
