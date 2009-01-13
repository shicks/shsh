module System.Console.ShSh.Pattern ( matchPattern ) where

import Control.Monad.State ( put, runState )
import Data.List ( partition )
import Text.Regex.Posix ( (=~) )

import Language.Sh.Syntax ( Lexeme(..), Word )

matchPattern :: Word -> String -> Bool
matchPattern w s = case mkRegex w of
                     Just r  -> s =~ r
                     Nothing -> fromLit w == s

-- copied YET AGAIN from Glob via Language.Sh.Glob...

mkRegex :: Word -> Maybe String
mkRegex w = case runState (mkG w) False of
             (s,True) -> Just $ "^"++s++"$"
             _  -> Nothing
    where mkG [] = return []
          mkG (Literal '[':xs) = case mkClass xs of
                                   Just (g,xs') -> fmap (g++) $ mkG xs'
                                   Nothing -> fmap ((mkLit '[')++) $ mkG xs
          mkG (Literal '*':Literal '*':xs) = mkG $ Literal '*':xs
          mkG (Literal '*':xs) = put True >> fmap (".*"++) (mkG xs)
          mkG (Literal '?':xs) = put True >> fmap ('.':) (mkG xs)
          mkG (Literal c:xs) = fmap (mkLit c++) $ mkG xs
          mkG (Quoted (Literal c):xs) = fmap (mkLit c++) $ mkG xs
          mkG (Quoted q:xs) = mkG $ q:xs
          mkG (Quote _:xs) = mkG xs
          mkLit '[' = "\\["
          mkLit ']' = "\\]"
          mkLit '(' = "\\("
          mkLit ')' = "\\)"
          mkLit '{' = "\\{"
          mkLit '}' = "\\}"
          mkLit '|' = "\\|"
          mkLit '^' = "\\^"
          mkLit '$' = "\\$"
          mkLit '.' = "\\."
          mkLit '*' = "\\*"
          mkLit '+' = "\\+"
          mkLit '?' = "\\?"
          mkLit '\\' = "\\\\"
          mkLit  c  = [  c]

mkClass :: Word -> Maybe (String,Word)
mkClass xs = let (range, rest) = break (isLit ']') xs
             in if null rest then Nothing
                else if null range
                     then let (range', rest') = break (isLit ']') (tail rest)
                          in if null rest' then Nothing
                             else do x <- cr' range'
                                     return (x,tail rest')
                     else do x <- cr' range
                             return (x,tail rest)
    where cr' s = Just $ "["++movedash (filter (not . isQuot) s)++"]"
          isLit c x = case x of { Literal c' -> c==c'; _ -> False }
          isQuot x = case x of { Quote _ -> True; _ -> False }
          quoted c x = case x of Quoted (Quoted x) -> quoted c $ Quoted x
                                 Quoted (Literal c') -> c==c'
                                 _ -> False
          movedash s = let (d,nd) = partition (quoted '-') s
                           bad = null d || (isLit '-' $ head $ reverse s)
                       in map fromLexeme $ if bad then nd else nd++d
          fromLexeme x = case x of { Literal c -> c; Quoted q -> fromLexeme q }

fromLit :: Word -> String
fromLit = concatMap $ \l -> case l of Literal c -> [c]
                                      Quoted q  -> fromLit [q]
                                      _         -> []
