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
               tryHeader "pwd.h" (define "HAVE_PWD") -- test function?!
                         "tilde expansion will not work fully."
               replace "@VERSION@" true_version
               ghcFlags ["-I."]
               createFile "System/Console/ShSh/Constants.hs"

buildable = do have_pwd <- isDefined "HAVE_PWD"
               let cfiles = if have_pwd then ["hspwd.c"]
                                        else []
                   cfiles' = map ("System/Console/ShSh/Foreign/"++) cfiles
               buildDoc
               executable "testlex" "testlex.hs" cfiles'
               executable "shsh" "shsh.hs" cfiles'

main = build [] configure buildable

tryHeader h job warn = (checkHeader h >> putS ("found header "++h) >> job)
                       `catchC` \_ -> putS ("failed to find working "++h++": "
                                            ++warn)

buildDoc =
    do addExtraData "haddock-directory" "doc/haddock"
       rm_rf "test/check-output"
       rm_rf "test/known-output"
       txtfiles <- concatMap fst `fmap` mapDirectory buildOneTest "test"
       beginTestWith $ do pwd >>= addToPath
                          pwd >>= setEnv "HOME"
       withDirectory "doc" $
           do htmls <- concat `fmap` mapM (\i -> markdownToHtml "../doc.css" i "")
                       txtfiles
              addTarget $ ["*manual*","*html*"] :<
                            ("manual/index.html":htmls) |<- defaultRule
       outputTests <- flip mapDirectory "test/check-output" $ \t ->
                      do expected <- cat $ "../known-output/"++t
                         testOutput t expected $
                                    fmap (filter (/='\r'))
                                    (systemOut "shsh" [t] `catchC` return)
                         return t
       test outputTests
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
