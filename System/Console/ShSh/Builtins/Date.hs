{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Date ( date ) where

import System.Console.ShSh.IO ( oPutStrLn )
import System.Console.ShSh.Shell ( Shell )

import System.Time ( getClockTime, toCalendarTime, calendarTimeToString )
import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(..) )

{-# NOINLINE date #-}
date :: [String] -> Shell ExitCode
date _ = do clockt <- liftIO getClockTime
            ct <- liftIO $ toCalendarTime clockt
            oPutStrLn $ calendarTimeToString ct
            return ExitSuccess
