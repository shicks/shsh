import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hIsEOF, stdin, hFlush, stdout )
import System.Console.ShSh.Parse ( parse )
-- import System.Console.ShSh.Expansions ( expansions )

import System.Console.ShSh.Shell ( Shell, startShell )
import System.Console.ShSh.IO ( oPutStrLn )

loop pre = do let prompt = if null pre then "$ " else "> "
              putStr prompt >> hFlush stdout
              eof <- hIsEOF stdin
              if eof then putStrLn "" >> exitWith ExitSuccess
                     else do x <- getLine
                             let l = parse [("foo","echo "),("bar","baz"),
                                            ("baz","foo bar"),
                                            ("foobar","sort && echo foo")] $
                                     pre++x
                             case l of
                               Right ts -> do process ts
                                              hFlush stdout
                                              loop ""
                               Left err -> do putStrLn $ show err
                                              loop $ pre++x++"\n"

process ts = do putStrLn $ "After Parsing: "++show ts

--                startShell $ do ts' <- expansions ts
--                                oPutStrLn $ "After Expand: "++show ts'

main = loop ""
