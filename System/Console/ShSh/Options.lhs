\chapter{Options module}

Here we deal with the various command-line options, which can
also be set with the 'set' builtin.

I've moved this all out of the Shell monad so that we can put
all the details of all the options into this module, and then
Shell can import it for, e.g. parsing command-line options.

Alternately, it might be better to just split this into two
separate modules, one with just the data, and the other with
the monadic operations.

The difficulty is that we'd like to be able to use something like
|setOpts| defined here to parse the command-line flags.  So maybe
this should end up somewhere else...?

\begin{code}
module System.Console.ShSh.Options ( setFlag, unsetFlag, getFlag, getFlags,
                                     setOpts ) where

import Control.Monad.Trans ( liftIO )
import Data.List ( union, (\\) )
import System.Exit ( ExitCode(..) )
import System.IO ( Handle, hPutStrLn )

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

setOpts :: Handle -> [String] -> Shell ExitCode
setOpts _ [] = return ExitSuccess
-- the "o" options are pesky...
setOpts h ("-o":opt:ss) | isOpt opt = setOptLong opt >> setOpts h ss
                        | otherwise = showOptsHuman h >> setOpts h (opt:ss)
setOpts h ("+o":opt:ss) | isOpt opt = unsetOptLong opt >> setOpts h ss
                        | otherwise = showOpts h >> setOpts h (opt:ss)
setOpts h ["-o"] = showOptsHuman h >> return ExitSuccess
setOpts h ["+o"] = showOpts h >> return ExitSuccess
setOpts h (s:ss) = case s of
                     '-':c:cs -> setFlag c   >> setOpts h (('-':cs):ss)
                     '+':c:cs -> unsetFlag c >> setOpts h (('-':cs):ss)
                     _ -> setOpts h ss -- no error checking...?

setOptLong :: String -> Shell ()
setOptLong s = return ()

unsetOptLong :: String -> Shell ()
unsetOptLong s = return ()

-- These need to be rewritten
showOptsHuman :: Handle -> Shell ()
showOptsHuman h = do f <- getFlags
                     liftIO $ hPutStrLn h $ "Opts: "++f

showOpts :: Handle -> Shell ()
showOpts = showOptsHuman

\end{code}