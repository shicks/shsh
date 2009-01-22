{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

import System.Exit ( exitWith )
import System.Environment ( getArgs )
import System.Console.ShSh.Shell ( startShell )
import System.Console.ShSh.EventLoop ( eventLoop, sourceProfile )
import System.Console.ShSh.Command ( source, eval )
import System.IO ( stdin, hIsTerminalDevice )

#ifdef HAVE_SIGNALS
import System.Posix.Signals ( Handler(..), installHandler, sigPIPE )
#endif

main :: IO ()
main = do args <- getArgs
          term <- hIsTerminalDevice stdin
          let h = if term then Nothing else Just stdin -- extend later w/ getopt
#ifdef HAVE_SIGNALS
          installHandler sigPIPE Ignore Nothing
#endif
          case args of
            [] -> startShell (sourceProfile >> eventLoop "" h) >>= exitWith
            [f] -> startShell (do sourceProfile
                                  source f) >>= exitWith
            ["-c",code] -> startShell (sourceProfile >> eval code) >>= exitWith
            fs -> fail $ unwords $ "Weird arguments: ":fs
