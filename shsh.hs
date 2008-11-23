{-# OPTIONS_GHC -cpp #-}

import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs )
import System.Console.ShSh.Shell ( startShell )
import System.Console.ShSh.EventLoop ( eventLoop, sourceProfile, source )
import System.IO ( stdin, hIsTerminalDevice )

#ifdef HAVE_SIGNALS
import System.Posix.Signals ( Handler(..), installHandler, sigPIPE )
#endif

main = do args <- getArgs
          term <- hIsTerminalDevice stdin
          let h = if term then Nothing else Just stdin -- extend later w/ getopt
#ifdef HAVE_SIGNALS
          installHandler sigPIPE Ignore Nothing
#endif
          case args of
            [] -> startShell (sourceProfile >> eventLoop "" h) >>= exitWith
            [f] -> startShell (do sourceProfile
                                  source f
                                  return ExitSuccess) >>= exitWith
            fs -> fail $ unwords $ "Weird arguments: ":fs
