\chapter{Prompt module}

\begin{code}

module Prompt ( prompt )
    where

import Shell

prompt :: Shell String
prompt = do Just pwd <- getEnv "PWD"
            return $ pwd ++ " $ "
            --Just user <- getEnv "USER"
            --Just hostname <- getEnv "HOSTNAME"
            --return $ user ++ "@" ++ hostname ++ " " ++ pwd ++ " $ "

\end{code}