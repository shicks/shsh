{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Rm ( rm ) where

import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flag, flagOn )

import System.Directory ( removeFile, removeDirectory,
                          doesFileExist, doesDirectoryExist,
                          getDirectoryContents )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import System.Console.GetOpt
import System.Exit ( ExitCode(..) )

rm :: [String] -> Shell ExitCode
rm = withArgs "rm" header args RequireOrder rm'
    where rm' [] = fail "rm requires an argument!"
          rm' fs = do amr <- flag 'r'
                      amf <- flag 'f'
                      let rm'' = case (amr,amf) of
                                 (True, True) -> rm_rf
                                 (True, False) -> rm_r
                                 (False, False) -> liftIO . removeFile
                                 (False, True) -> rm_f
                      mapM_ rm'' fs
                      return ExitSuccess
          header = "Usage: rm [OPTION] FILE...\n"++
                   "Remove (unlink) the FILE(s)."
          args = [flagOn "f" ["force"] 'f' "ignore nonexistent files, never prompt",
                  flagOn "Rr" ["recursive"] 'r'
                             "remove directories and their contents recursively"]
          rm_rf d =
              do liftIO $ catch (removeFile d) $ \_ -> return ()
                 isd <- liftIO $ doesDirectoryExist d
                 when isd $ do fs <- liftIO $
                                     fmap (filter (not . (`elem` [".",".."]))) $
                                     getDirectoryContents d
                               mapM_ (rm_rf . ((d++"/")++)) fs
                               liftIO $ catch (removeDirectory d) $ \_ -> return ()
          rm_r d =
              do isf <- liftIO $ doesFileExist d
                 if isf
                   then liftIO $ removeFile d
                   else do liftIO $ catch (removeFile d) $ \_ -> return ()
                           isd <- liftIO $ doesDirectoryExist d
                           when isd $ do fs <- liftIO $
                                           fmap (filter (not . (`elem` [".",".."]))) $
                                           getDirectoryContents d
                                         mapM_ (rm_r . ((d++"/")++)) fs
                                         liftIO $ removeDirectory d
          rm_f f = liftIO $ catch (removeFile f) $ \_ -> return ()
