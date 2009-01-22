{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Exit ( exit ) where

import Control.Monad.Trans ( liftIO )
import System.Console.ShSh.Shell ( Shell )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hPutStrLn, stderr )

{-# NOINLINE exit #-}
exit :: [String] -> Shell a
exit [] = liftIO $ exitWith ExitSuccess
exit ["0"] = liftIO $ exitWith ExitSuccess
exit [n] = case reads n of
           [(e,"")] -> liftIO $ exitWith $ ExitFailure e
           _ -> liftIO $ do hPutStrLn stderr $ "exit: "++n++": numeric argument required"
                            exitWith $ ExitFailure 1
exit _ = liftIO $ do hPutStrLn stderr $ "exit: too many arguments"
                     exitWith $ ExitFailure 1
