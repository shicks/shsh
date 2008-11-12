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
               tryHeader "pwd.h" (define "HAVE_PWD") -- test function?!
                         "tilde expansion will not work fully."
               replace "@VERSION@" true_version
               createFile "System/Console/ShSh/Constants.lhs"

buildable = do have_pwd <- isDefined "HAVE_PWD"
               let cfiles = if have_pwd then ["hspwd.c"]
                                        else []
                   cfiles' = map ("System/Console/ShSh/Foreign/"++) cfiles
               executable "testlex" "testlex.hs" cfiles'
               executable "shsh" "shsh.lhs" cfiles'

main = build [] configure buildable

tryHeader h job warn = (checkHeader h >> putS ("found header "++h) >> job)
                       `catchC` \_ -> putS ("failed to find working "++h++": "
                                            ++warn)
