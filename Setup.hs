#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do ghcFlags ["-threaded","-O2"]
               checkHaskeline
               buildable
               return ()

buildable = executable "shsh" "shsh.lhs" []

main = build [] configure buildable

checkHaskeline =
    unlessC (haveExtraData "NO_HASKELINE") $
    do requireModuleExporting "System.Console.Haskeline"
           "runInputT, defaultSettings, getInputLine, handleDyn"
           "runInputT defaultSettings (getInputLine \"prompt: \") :: IO (Maybe String)"
       define "HAVE_HASKELINE"
     `catchC` \_ -> putS "Haskeline not found."
