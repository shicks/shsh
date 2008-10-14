\chapter{Prompt module}

\begin{code}

module System.Console.ShSh.Prompt ( prompt )
    where

import System.Console.ShSh.Shell

prompt :: Shell String
prompt = do Just pwd <- getEnv "PWD"
            return $ pwd ++ " $ "
            --Just user <- getEnv "USER"
            --Just hostname <- getEnv "HOSTNAME"
            --return $ user ++ "@" ++ hostname ++ " " ++ pwd ++ " $ "

\end{code}
