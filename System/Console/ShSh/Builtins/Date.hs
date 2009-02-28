{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Date ( date ) where

import System.Console.ShSh.IO ( oPutStrLn )
import System.Console.ShSh.Shell ( Shell )

import System.Time ( getClockTime, toCalendarTime, CalendarTime,
                     calendarTimeToString,
                     ctYear, ctMonth, ctDay, ctHour, ctMin, ctSec, ctTZ )
import Control.Monad.Trans ( liftIO )
import System.Exit ( ExitCode(..) )

{-# NOINLINE date #-}
date :: [String] -> Shell ExitCode
date args = do clockt <- liftIO getClockTime
               ct <- liftIO $ toCalendarTime clockt
               oPutStrLn $ case args of
                             ['+':x] -> ct2str ct x
                             _ -> calendarTimeToString ct
               return ExitSuccess

-- The following isn't comprehensive, but is a start...
ct2str :: CalendarTime -> String -> String
ct2str ct ('%':'Y':r) = show (ctYear ct)++ ct2str ct r
ct2str ct ('%':'m':r) = padMonth ++ ct2str ct r
    where padMonth = if monthnum < 10 then '0':show monthnum else show monthnum
          monthnum = 1 + fromEnum (ctMonth ct)
ct2str ct ('%':'d':r) = show (ctDay ct)++ ct2str ct r
ct2str ct ('%':'k':r) = show (ctHour ct)++ ct2str ct r
ct2str ct ('%':'M':r) = show (ctMin ct)++ ct2str ct r
ct2str ct ('%':'s':r) = show (ctSec ct)++ ct2str ct r
ct2str ct ('%':'z':r) = show (ctTZ ct)++ ct2str ct r
ct2str ct (c:cs) = c : ct2str ct cs
ct2str _ "" = ""
