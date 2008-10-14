#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do ghcFlags ["-threaded","-O2"]
               buildable
               return ()

buildable = executable "shsh" "shsh.lhs" []

main = build [] configure buildable

