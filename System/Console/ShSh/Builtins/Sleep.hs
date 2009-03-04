{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Sleep ( sleep ) where

import System.Console.ShSh.Shell ( Shell )

import Control.Monad.Trans ( liftIO )
import Control.Concurrent ( threadDelay )
import System.Exit ( ExitCode(..) )

{-# NOINLINE sleep #-}
sleep :: [String] -> Shell ExitCode
sleep args = do liftIO $ threadDelay $ 1000000*sum (map read args)
                return ExitSuccess
