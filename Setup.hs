#!/usr/bin/runhaskell
import Distribution.Franchise
import Data.List ( isSuffixOf )

true_version = "0.0.2"

configure = do version true_version
               defineAs "PACKAGE_VERSION" true_version
               ghcFlags ["-threaded","-O2"]
               withModule "System.Posix.Signals" $ define "HAVE_SIGNALS"
               withModule "System.Console.Haskeline" $ define "HAVE_HASKELINE"
               withModuleExporting "System.Process" "createProcess, shell"
                   "createProcess (shell \"echo 1\") >> return ()" $
                   define "HAVE_CREATEPROCESS"
               withModule "Text.Parsec" $ define "HAVE_PARSEC3"
               tryHeader "pwd.h" (define "HAVE_PWD") -- test function?!
                         "tilde expansion will not work fully."
               replace "@VERSION@" true_version
               ghcFlags ["-I."]
               createFile "System/Console/ShSh/Constants.lhs"

buildable = do have_pwd <- isDefined "HAVE_PWD"
               let cfiles = if have_pwd then ["hspwd.c"]
                                        else []
                   cfiles' = map ("System/Console/ShSh/Foreign/"++) cfiles
               buildDoc
               executable "testlex" "testlex.hs" cfiles'
               executable "shsh" "shsh.lhs" cfiles'

main = build [] configure buildable

tryHeader h job warn = (checkHeader h >> putS ("found header "++h) >> job)
                       `catchC` \_ -> putS ("failed to find working "++h++": "
                                            ++warn)

buildDoc =
    do addExtraData "haddock-directory" "doc/haddock"
       rm_rf "tmp"
       alltests <- mapDirectory buildOneTest "test"
       beginTestWith (pwd >>= addToPath)
       withDirectory "doc" $
           do htmls <- concat `fmap` mapM (\i -> markdownToHtml "../doc.css" i "")
                       (concatMap fst alltests)
              addTarget $ ["*manual*","*html*"] :<
                            ("manual/index.html":htmls) |<- defaultRule
       test $ concatMap snd alltests
    where buildOneTest f | ".splits" `isSuffixOf` f = return ([],[])
          buildOneTest f =
              do xxx <- splitMarkdown f ("../doc/"++f++".txt")
                 case xxx of
                   txtf:tests ->
                       do ts <- mapM (\ (d, t) -> withDirectory d $
                                                  testOne t "shsh" t >> return t)
                                $ map splitPath tests
                          return ([txtf],ts)
                   [] -> return ([],[])
