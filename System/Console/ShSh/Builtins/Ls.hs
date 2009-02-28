{-# OPTIONS_GHC -Wall -cpp #-}
module System.Console.ShSh.Builtins.Ls ( ls ) where

import System.Console.ShSh.IO ( oPutStr, oPutStrLn )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flagOn, flag )

import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(..) )
import Data.List ( sort )
import System.Directory ( doesFileExist, getDirectoryContents )
#ifdef UNIX
import System.Posix.Files ( getSymbolicLinkStatus, isDirectory, linkCount,
                            fileSize,
                            ownerReadMode, ownerWriteMode, ownerExecuteMode,
                            groupReadMode, groupWriteMode, groupExecuteMode,
                            otherReadMode, otherWriteMode, otherExecuteMode,
                            fileMode, nullFileMode, intersectFileModes )
#endif

{-# NOINLINE ls #-}
ls :: [String] -> Shell ExitCode
ls = withArgs "ls" header args ls'
    where ls' [] = do fs <- liftIO (getDirectoryContents ".")
                      amlong <- flag 'l'
                      let lsone = if amlong then lsfilelong else oPutStrLn
                      mapM_ lsone $ sort $
                              filter unboring fs
                      return ExitSuccess
          ls' [f] = do isf <- liftIO $ doesFileExist f
                       amlong <- flag 'l'
                       let lsone = if amlong then lsfilelong else oPutStrLn
                       if isf
                        then lsone f
                        else do cs <- liftIO $ getDirectoryContents f
                                mapM_ lsone $ drop 2 $ sort cs
                       return ExitSuccess
          ls' fs =
              do x <- mapM (liftIO . lsf) fs
                 amlong <- flag 'l'
                 let lsone = if amlong then lsfilelong else oPutStrLn
                 mapM_ lsone x
                 return ExitSuccess
          header = "Usage: ls [option].. [file]..."
          args = [flagOn "l" [] 'l' "use a long listing format"]
          unboring ('.':_) = False
          unboring _ = True
          lsf f = do isf <- doesFileExist f
                     if isf
                        then return f
                        else do cs <- sort `fmap` getDirectoryContents f
                                return $ unlines ((f++":") :
                                                    map ("  "++) (drop 2 cs))
#ifdef UNIX
          lsfilelong f = do stat <- liftIO $ getSymbolicLinkStatus f
                            oPutStr $ if isDirectory stat then "d" else "-"
                            oPutStr (showMode $ fileMode stat)
                            oPutStr $ show (linkCount stat)++" "
                            oPutStr $ pad 8 (show $ fileSize stat)++" "
                            oPutStrLn f
                            return ()
          showMode m = showOne 'r' ownerReadMode m:
                       showOne 'w' ownerWriteMode m:
                       showOne 'x' ownerExecuteMode m:
                       showOne 'r' groupReadMode m:
                       showOne 'w' groupWriteMode m:
                       showOne 'x' groupExecuteMode m:
                       showOne 'r' otherReadMode m:
                       showOne 'w' otherWriteMode m:
                       showOne 'x' otherExecuteMode m:
                                   " "
          showOne c x m = if intersectFileModes x m == nullFileMode
                          then '-'
                          else c
          pad n s = if length s >= n then s else (take (n-length s) $ repeat ' ')++s
#else
          lsfilelong f = oPutStrLn f
#endif
