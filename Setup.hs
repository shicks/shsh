#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do ghcFlags ["-threaded","-O2"]
               withModule "System.Console.Haskeline" $ define "HAVE_HASKELINE"

buildable = executable "shsh" "shsh.lhs" []

main = build [] configure buildable
