{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Touch ( touch ) where

import System.Console.ShSh.Shell ( Shell )

import System.IO ( openFile, IOMode(..), hClose, hFileSize, hSetFileSize )
import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(..) )

{-# NOINLINE touch #-}
touch :: [String] -> Shell ExitCode
touch [] = fail "missing file operand"
touch fs = do mapM_ touch'' fs
              return ExitSuccess
    where touch'' x = liftIO $ do h <- openFile x AppendMode
                                  s <- hFileSize h
                                  hSetFileSize h (s+1)
                                  hClose h
                                  h2 <- openFile x AppendMode
                                  hSetFileSize h2 s
                                  hClose h2


