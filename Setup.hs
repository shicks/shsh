#!/usr/bin/runhaskell
import Distribution.Franchise
import Data.List ( isSuffixOf )

main = build [] $
            do copyright "Copyright 2008 Stephen Hicks"
               license "BSD3"
               addExtraData "category" "Language"
               addExtraData "maintainer" "Stephen Hicks <sdh33@cornell.edu>"
               addExtraData "synopsis"
                   "A package for parsing shell scripts"
               addExtraData "description" $ unlines
                   ["",
                    "        Language.Sh is a collection of modules for parsing and",
                    "        manipulating expressions in shell grammar.",
                    "",
                    "        Note: the API is still rather unstable, at the moment."]
               ghcFlags ["-threaded","-O2"]
               withModule "System.Posix.Signals" $ define "HAVE_SIGNALS"
               withModule "System.Console.Haskeline" $ define "HAVE_HASKELINE"
               withModuleExporting "System.FilePath.Glob"
                   "compPosix, commonPrefix" "(compPosix,commonPrefix)" $
                   define "HAVE_GLOB" -- cannot be satisfied w/ released glob!
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
               package "language-sh" ["Language.Sh.Arithmetic",
                                      "Language.Sh.Expansion",
                                      "Language.Sh.Glob",
                                      "Language.Sh.Map",
                                      "Language.Sh.Parser",
                                      "Language.Sh.Pretty",
                                      "Language.Sh.Syntax"] []
               privateExecutable "testlex" "testlex.hs" cfiles'
               privateExecutable "pretty-sh" "pretty-sh.hs" cfiles'
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
                              shshprefix <- whenC ((elem "fails-in-shsh" . words)
                                                   `fmap` cat t) $ return "failing-"
                              testOutput (shshprefix++t) expected $
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
                              return ((shshprefix++t):basht++dasht)
                      else return []
       test $ concat outputTests
    where buildOneTest f | ".splits" `isSuffixOf` f = return []
          buildOneTest f = take 1 `fmap` splitMarkdown f ("../doc/"++f++".txt")
