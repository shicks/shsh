import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hIsEOF, stdin, hFlush, stdout )
import System.Console.ShSh.Lexer ( runLexer )
import System.Console.ShSh.Parser ( parse )
import System.Console.ShSh.Expansions ( expansions )

import System.Console.ShSh.Shell ( Shell, startShell )
import System.Console.ShSh.IO ( oPutStrLn )

loop pre = do let prompt = if null pre then "$ " else "> "
              putStr prompt >> hFlush stdout
              eof <- hIsEOF stdin
              if eof then putStrLn "" >> exitWith ExitSuccess
                     else do x <- getLine
                             let l = runLexer [] $ pre++x
                             case l of
                               Right ts -> process ts >> hFlush stdout >> loop ""
                               Left _ -> loop $ pre++x++"\n"

process ts = do putStrLn $ "After Lexing: "++show ts
                case parse ts of
                  Left e -> putStrLn $ "No Parse: " ++ e
                  Right e -> putStrLn $ "After Parsing: " ++ show e

--                startShell $ do ts' <- expansions ts
--                                oPutStrLn $ "After Expand: "++show ts'

main = loop ""
