import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hIsEOF, stdin, hFlush, stdout )
import Language.Sh.Parser ( parse, hereDocsComplete )
-- import System.Console.ShSh.Expansions ( expansions )

loop pre = do let prompt = if null pre then "$ " else "> "
              putStr prompt >> hFlush stdout
              eof <- hIsEOF stdin
              if eof then putStrLn "" >> exitWith ExitSuccess
                     else do x <- getLine
                             let l = parse [("foo","echo "),("bar","baz"),
                                            ("baz","foo bar"),
                                            ("foobar","sort && echo foo"),
                                            ("blah","echo #")] $
                                     pre++x
                             case l of
                               Right ts -> do process ts >> hFlush stdout
                                              loop $ if hereDocsComplete ts
                                                     then ""
                                                     else pre++x++"\n"
                               Left (err,False) -> do putStrLn err
                                                      loop $ pre++x++"\n"
                               Left (err,True) -> do putStrLn err
                                                     loop $ ""

process ts = do putStrLn $ "After Parsing: "++show ts

--                startShell $ do ts' <- expansions ts
--                                oPutStrLn $ "After Expand: "++show ts'

main = loop ""
