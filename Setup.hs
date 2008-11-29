#!/usr/bin/runhaskell
import Distribution.Franchise
import Data.List ( isSuffixOf )

main = build [] $
            do ghcFlags ["-threaded","-O2"]
               withModule "System.Posix.Signals" $ define "HAVE_SIGNALS"
               withModule "System.Console.Haskeline" $ define "HAVE_HASKELINE"
               withModule "System.FilePath.Glob" $ define "HAVE_GLOB"
               withModuleExporting "Text.ParserCombinators.Parsec.Expr"
                   "Operator(..)" "Postfix" $
                   define "HAVE_PARSEC_POSTFIX"
               withModuleExporting "System.Process" "createProcess, shell"
                   "createProcess (shell \"echo 1\") >> return ()" $
                   define "HAVE_CREATEPROCESS"
               tryHeader "pwd.h" (define "HAVE_PWD") -- test function?!
                         "tilde expansion will not work fully."
               autoVersion NumberedPreRc >>= replace "@VERSION@"
               ghcFlags ["-I."]
               createFile "System/Console/ShSh/Constants.hs"
               cfiles <- whenC (isDefined "HAVE_PWD") $ return ["hspwd.c"]
               let cfiles' = map ("System/Console/ShSh/Foreign/"++) cfiles
               buildDoc
               executable "testlex" "testlex.hs" cfiles'
               executable "shsh" "shsh.hs" cfiles'

tryHeader h job warn = requireWithFeedback ("for header "++h)
                       ((checkHeader h >> job >> return "yes")
                       `catchC` \_ -> return ("no\n"++warn))

buildDoc =
    do addExtraData "haddock-directory" "doc/haddock"
       rm_rf "test/check-output"
       rm_rf "test/known-output"
       txtfiles <- concat `fmap` mapDirectory buildOneTest "test"
       beginTestWith $ do pwd >>= addToPath
                          pwd >>= setEnv "HOME"
       withDirectory "doc" $
           do htmls <- concat `fmap` mapM (\i -> markdownToHtml "../doc.css" i "")
                       txtfiles
              addTarget $ ["*manual*","*html*"] :<
                            ("manual/index.html":htmls) |<- defaultRule
       outputTests <- flip mapDirectory "test/check-output" $ \t ->
                      if ".sh" `isSuffixOf` t
                      then do expected <- cat $ "../known-output/"++t
                              testOutput t expected $
                                   fmap (filter (/='\r')) $ systemOut "shsh" [t]
                              basht <- withProgram "bash" [] $ \_ ->
                                       do prefix <- whenC ((elem "fails-in-bash" . words)
                                                           `fmap` cat t) $
                                                    return "failing-"
                                          testOutput (prefix++"bash-"++t) expected $
                                                     fmap (filter (/='\r')) $ systemOut "bash" [t]
                                          return [prefix++"bash-"++t]
                              dasht <- withProgram "dash" ["sash"] $ \dash ->
                                       do prefix <- whenC ((elem "fails-in-dash" . words)
                                                           `fmap` cat t) $
                                                    return "failing-"
                                          testOutput (prefix++dash++"-"++t) expected $
                                                     fmap (filter (/='\r')) $ systemOut dash [t]
                                          return [prefix++dash++"-"++t]
                              return (t:basht++dasht)
                      else return []
       test $ concat outputTests
    where buildOneTest f | ".splits" `isSuffixOf` f = return []
          buildOneTest f = take 1 `fmap` splitMarkdown f ("../doc/"++f++".txt")
