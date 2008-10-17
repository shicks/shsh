\chapter{Options module}

Here we deal with the various command-line options, which can
also be set with the 'set' builtin.

\begin{code}
module System.Console.ShSh.Options ( setFlag, unsetFlag, getFlag, getFlags,
                                     setOpts ) where

import Control.Monad.Trans ( liftIO )
import Data.List ( union, (\\) )
import System.Console.ShSh.Shell ( Shell, withEnv, tryEnv )


setFlag :: Char -> Shell ()
setFlag c = withEnv "-" (`union`[c])

unsetFlag :: Char -> Shell ()
unsetFlag c = withEnv "-" (\\[c])

getFlag :: Char -> Shell Bool
getFlag c = elem c `fmap` tryEnv "-"

getFlags :: Shell String
getFlags = tryEnv "-"


isOpt :: String -> Bool
isOpt ('-':_) = True
isOpt ('+':_) = True
isOpt _ = False

setOpts :: [String] -> Shell ()
setOpts [] = return ()
-- the "o" options are pesky...
setOpts ("-o":opt:ss) | isOpt opt = setOptLong opt >> setOpts ss
                      | otherwise = showOptsHuman >> setOpts (opt:ss)
setOpts ("+o":opt:ss) | isOpt opt = unsetOptLong opt >> setOpts ss
                      | otherwise = showOpts >> setOpts (opt:ss)
setOpts ["-o"] = showOptsHuman
setOpts ["+o"] = showOpts
setOpts (s:ss) = case s of
                   '-':c:cs -> setFlag c   >> setOpts (('-':cs):ss)
                   '+':c:cs -> unsetFlag c >> setOpts (('-':cs):ss)
                   _ -> setOpts ss -- no error checking...?

setOptLong :: String -> Shell ()
setOptLong s = return ()

unsetOptLong :: String -> Shell ()
unsetOptLong s = return ()

-- These need to be rewritten
showOptsHuman :: Shell ()
showOptsHuman = do f <- getFlags
                   liftIO $ putStrLn $ "Opts: "++f

showOpts :: Shell ()
showOpts = showOptsHuman

\end{code}