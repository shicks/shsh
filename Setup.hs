#!/usr/bin/runhaskell
import Distribution.Franchise

true_version = "0.0.1"

configure = do version true_version
               defineAs "PACKAGE_VERSION" true_version
               ghcFlags ["-threaded","-O2"]
               requireModuleExporting "Data.List" "intercalate" "intercalate"
               withModule "System.Posix.Signals" $ define "HAVE_SIGNALS"
               withModule "System.Console.Haskeline" $ define "HAVE_HASKELINE"
               withModuleExporting "System.Process" "createProcess, shell"
                   "createProcess (shell \"echo 1\") >> return ()" $
                   define "HAVE_CREATEPROCESS"
               replace "@VERSION@" true_version
               createFile "System/Console/ShSh/Constants.lhs"

buildable = do executable "shsh" "shsh.lhs" []
               executable "testlex" "testlex.hs" []

main = build [] configure buildable
