\chapter{Prompt module}

\begin{code}

module System.Console.ShSh.Prompt ( prompt )
    where

import Control.Monad.Trans ( liftIO )
import Data.Char ( chr )
import Data.Maybe ( fromMaybe )
import Data.List ( isPrefixOf )
import System.Environment ( getProgName )

import System.Console.ShSh.Constants ( version )
import System.Console.ShSh.Shell ( Shell, getEnv )

tildeSub :: String -> Shell String
tildeSub s = do home <- getEnv "HOME"
                case home of
                  Just h -> if h `isPrefixOf` s
                               then return $ '~':drop (length h) s
                               else return s
                  Nothing -> return s

expand :: Char -> Shell String
expand 'e' = return [chr 27]
expand 'h' = fromMaybe "" `fmap` getEnv "HOSTNAME"
expand 'H' = (takeWhile (/='.') . fromMaybe "") `fmap` getEnv "HOSTNAME"
expand 's' = liftIO getProgName
expand 'u' = fromMaybe "" `fmap` getEnv "USER"
expand 'v' = return version
expand 'w' = tildeSub =<< fromMaybe "" `fmap` getEnv "PWD"
expand 'W' = tildeSub =<< maybe "" (reverse . takeWhile notSep . reverse)
                            `fmap` getEnv "PWD"
    where notSep c = c /= '/' && c /= '\\'
expand '$' = do uid <- fromMaybe "1" `fmap` getEnv "UID"
                return $ case uid of
                           "0" -> "#"
                           _   -> "$"
expand '[' = return ""
expand ']' = return ""
expand '\\' = return "\\"
expand x = return ['\\',x]

expandAll :: String -> Shell String -- this is maybe bad recursion, but oh well
expandAll ('\\':c:rest) = expand c >>= \x -> (x++) `fmap` expandAll rest
expandAll (c:rest) = (c:) `fmap` expandAll rest
expandAll "" = return ""

prompt :: String -> Shell String
prompt "" = do -- pwd <- fromMaybe "\\s-\\v\\$ " `fmap` getEnv "PS1"
              pwd <- fromMaybe "\\s-\\v: \\w \\$ " `fmap` getEnv "PS1"
              expandAll pwd
              --return $ pwd ++ " $ "
              --Just user <- getEnv "USER"
              --Just hostname <- getEnv "HOSTNAME"
              --return $ user ++ "@" ++ hostname ++ " " ++ pwd ++ " $ "
promp _ = do pwd <- fromMaybe "> " `fmap` getEnv "PS2"
             expandAll pwd

\end{code}
