{-# LANGUAGE PatternGuards #-}

module Language.Sh.Parser.Internal where

import Language.Sh.Parser.Parsec
import Language.Sh.Syntax

import Data.Char ( isDigit )
import Text.ParserCombinators.Parsec ( choice )

impossible = const undefined

redirOperator :: P String
redirOperator = choice [do char '>'
                           choice [char '&' >> return ">&"
                                  ,char '>' >> return ">>"
                                  ,char '|' >> return ">|"
                                  ,return ">"]
                       ,do char '<'
                           choice [char '&' >> return "<&"
                                  ,char '<' >> choice [char '-' >> return "<<-"
                                                      ,return "<<"]
                                  ,return "<"]]

mkRedir :: String -> Maybe Int -> Word -> P Redir -- in monad for fail
mkRedir _ (Just d) _ | d > 255 = fail $ "file descriptor too large: "++show d
mkRedir op@('<':_) Nothing t   = mkRedir op (Just 0) t
mkRedir op@('>':_) Nothing t   = mkRedir op (Just 1) t -- defaults
mkRedir "<"   (Just s) t = return $ s :< t
mkRedir "<&"  (Just s) t | Just t' <- wordToInt t = return $ s :<& t'
                         | otherwise = fail "bad file descriptor"
mkRedir "<>"  (Just s) t = return $ s :<> t
mkRedir "<<"  (Just s) t = return $ s :<< t
mkRedir "<<-" (Just s) t = return $ s :<<- t
mkRedir ">"   (Just s) t = return $ s :> t
mkRedir ">&"  (Just s) t | Just t' <- wordToInt t = return $ s :>& t'
                         | otherwise = fail "bad file descriptor"
mkRedir ">>"  (Just s) t = return $ s :>> t
mkRedir ">|"  (Just s) t = return $ s :>| t

wordToInt :: Word -> Maybe Int
wordToInt w = case fromLiteral w of
                Just ds | null $ filter (not . isDigit) ds -> Just $ read ds
                _ ->  Nothing

addAssignment :: Assignment -> Statement -> Statement
addAssignment a (Statement ws rs as) = Statement ws rs (a:as)
addAssignment _ (Subshell _ _) = impossible "cannot add assignment to subshell"

addWord :: Word -> Statement -> Statement
addWord w (Statement ws rs as) = Statement (w:ws) rs as
addWord _ (Subshell _ _) = impossible "cannot add word to subshell"

addRedirection :: Redir -> Statement -> Statement
addRedirection r (Statement ws rs as) = Statement ws (r:rs) as
addRedirection r (Subshell cs rs) = Subshell cs (r:rs)

fromLiteral :: Word -> Maybe String
fromLiteral [] = Just []
fromLiteral (Literal c:cs) = fmap (c:) $ fromLiteral cs
fromLiteral _ = Nothing

ql :: Char -> Lexeme
ql = Quoted . Literal
