\chapter{Prompt module}

\begin{code}

module System.Console.ShSh.Prompt ( prompt )
    where

import Control.Monad.Trans ( liftIO )
import Data.Char ( chr )
import Data.Maybe ( fromMaybe )
import System.Environment ( getProgName )

import System.Console.ShSh.Constants ( version )
import System.Console.ShSh.Shell ( Shell, getEnv )

expand :: Char -> Shell String
expand 'e' = return [chr 27]
expand 'h' = fromMaybe "" `fmap` getEnv "HOSTNAME"
expand 'H' = (takeWhile (/='.') . fromMaybe "") `fmap` getEnv "HOSTNAME"
expand 's' = liftIO getProgName
expand 'u' = fromMaybe "" `fmap` getEnv "USER"
expand 'v' = return version
expand 'w' = fromMaybe "" `fmap` getEnv "PWD" -- need to sub ~
expand 'W' = fromMaybe "" `fmap` getEnv "PWD" -- basename only (and ~)
expand '$' = do uid <- fromMaybe "1" `fmap` getEnv "UID"
                return $ case uid of
                           "0" -> "#"
                           _   -> "$"
expand '[' = return ""
expand ']' = return ""
expand '\\' = return "\\"

expandAll :: String -> Shell String -- this is maybe bad recursion, but oh well
expandAll ('\\':c:rest) = expand c >>= \x -> (x++) `fmap` expandAll rest
expandAll (c:rest) = (c:) `fmap` expandAll rest
expandAll "" = return ""

prompt :: Shell String
prompt = do pwd <- fromMaybe "\\s-\\v\\$ " `fmap` getEnv "PS1"
            expandAll pwd
            --return $ pwd ++ " $ "
            --Just user <- getEnv "USER"
            --Just hostname <- getEnv "HOSTNAME"
            --return $ user ++ "@" ++ hostname ++ " " ++ pwd ++ " $ "

\end{code}
