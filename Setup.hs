#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do ghcFlags ["-threaded","-O2"]
               requireModule "System.Console.Haskeline"
                 `catchC` \_ -> putS "Packages haskeline not found."

buildable = executable "shsh" "shsh.lhs" []

main = build [] configure buildable
