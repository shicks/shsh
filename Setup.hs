#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do ghcFlags ["-threaded","-O2"]
               requireModuleExporting "Data.List" "intercalate" "intercalate"
               withModule "System.Posix.Signals" $ define "HAVE_SIGNALS"
               withModule "System.Console.Haskeline" $ define "HAVE_HASKELINE"
               withModuleExporting "System.Process" "createProcess, shell"
                   ("createProcess (shell \"echo 1\") >> return ()") $
                   define "HAVE_CREATEPROCESS"

buildable = executable "shsh" "shsh.lhs" []

main = build [] configure buildable
