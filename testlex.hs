import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hIsEOF, stdin, hFlush, stdout )
import System.Console.ShSh.Lexer ( runLexer )

loop pre = do let prompt = if null pre then "$ " else "> "
              putStr prompt >> hFlush stdout
              eof <- hIsEOF stdin
              if eof then putStrLn "" >> exitWith ExitSuccess
                     else do x <- getLine
                             let l = runLexer (pre++x)
                             case l of
                               Just ts -> print ts >> hFlush stdout >> loop ""
                               Nothing -> loop (pre++x++"\n")

main = loop ""
