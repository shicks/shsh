{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Touch ( touch ) where

import System.Console.ShSh.Shell ( Shell )

import System.IO ( openFile, IOMode(..), hClose )
import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(..) )

{-# NOINLINE touch #-}
touch :: [String] -> Shell ExitCode
touch [] = fail "missing file operand"
touch fs = do mapM_ touch'' fs
              return ExitSuccess
    where touch'' x = liftIO (openFile x AppendMode >>= hClose)


