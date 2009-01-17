import Control.Monad ( when )
import Data.List ( isPrefixOf )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hIsEOF, stdin, hFlush, stdout, stderr, hPutStrLn )
import Language.Sh.Parser ( parse, hereDocsComplete )
import Language.Sh.Pretty ( pretty )

loop pre = do mx <- getLine'
              case mx of
                Nothing -> if null pre
                           then exitWith ExitSuccess
                           else do hPutStrLn stderr $ "Unparsed: "++pre
                                   exitWith $ ExitFailure 1
                Just x -> case parse [] $ pre++x of
                            Right ts -> if hereDocsComplete ts
                                        then do process ts
                                                loop $ ""
                                        else loop $ pre++x++"\n"
                            Left (err,False) -> loop $ pre++x++"\n"
                            Left (err,True) -> do hPutStrLn stderr err
                                                  loop $ ""

getLine' = do eof <- hIsEOF stdin
              if eof
                 then return Nothing
                 else do x <- getLine -- deal with shebang/comments
                         if "#" `isPrefixOf` dropWhile (==' ') x
                            then putStrLn x >> getLine'
                            else return $ Just x

process ts = do putStrLn $ pretty ts

main = loop ""
