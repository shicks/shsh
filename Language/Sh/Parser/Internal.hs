{-# LANGUAGE PatternGuards #-}

module Language.Sh.Parser.Internal where

import Language.Sh.Parser.Parsec
import Language.Sh.Syntax

import Data.Char ( isDigit )
import Text.ParserCombinators.Parsec ( choice )

impossible = const undefined

redirOperator :: P String
redirOperator = token $ choice [do char '>'
                                   choice [char '&' >> return ">&"
                                          ,char '>' >> return ">>"
                                          ,char '|' >> return ">|"
                                          ,return ">"]
                               ,do char '<'
                                   choice [char '&' >> return "<&"
                                          ,do char '<'
                                              choice [char '-' >> return "<<-"
                                                     ,return "<<"]
                                          ,char '>' >> return "<>"
                                          ,return "<"]]

-- |Takes an operator, maybe an int, and a word target.
mkRedir :: String -> Maybe Int -> Word -> P Redir -- need P for fail
mkRedir _ (Just d) _ | d > 255 = fail $ "file descriptor too large: "++show d
mkRedir op@('<':_) Nothing t   = mkRedir op (Just 0) t
mkRedir op@('>':_) Nothing t   = mkRedir op (Just 1) t -- defaults
mkRedir "<"   (Just s) t = return $ s :< t
mkRedir "<&"  (Just s) t | Just t' <- wordToInt t = return $ s :<& t'
                         | otherwise = fail "bad file descriptor"
mkRedir "<>"  (Just s) t = return $ s :<> t
mkRedir ">"   (Just s) t = return $ s :> t
mkRedir ">&"  (Just s) t | Just t' <- wordToInt t = return $ s :>& t'
                         | otherwise = fail "bad file descriptor"
mkRedir ">>"  (Just s) t = return $ s :>> t
mkRedir ">|"  (Just s) t = return $ s :>| t


mkHereDoc :: String -> Maybe Int -> String -> P Redir -- queues...
mkHereDoc op Nothing t     = mkHereDoc op (Just 0) t
mkHereDoc "<<"  (Just s) t = do addHereDoc t
                                return $ s :<< t
mkHereDoc "<<-" (Just s) t = do addHereDoc t
                                return $ s :<<- t

wordToInt :: Word -> Maybe Int
wordToInt w = case fromLiteral w of
                Just ds | null $ filter (not . isDigit) ds -> Just $ read ds
                _ ->  Nothing

addAssignment :: Assignment -> Statement -> Statement
addAssignment a (Statement ws rs as) = Statement ws rs (a:as)
addAssignment a (OrderedStatement ts) = OrderedStatement (TAssignment a:ts)
addAssignment _ (Compound _ _) = impossible "cannot add assignment to Compound"

addWord :: Word -> Statement -> Statement
addWord w (Statement ws rs as) = Statement (w:ws) rs as
addWord w (OrderedStatement ts) = OrderedStatement (TWord w:ts)
addWord _ (Compound _ _) = impossible "cannot add word to Compound"

addRedirection :: Redir -> Statement -> Statement
addRedirection r (Statement ws rs as) = Statement ws (r:rs) as
addRedirection r (OrderedStatement ts) = OrderedStatement (TRedir r:ts)
addRedirection r (Compound c rs) = Compound c (r:rs)

fromLiteral :: Word -> Maybe String
fromLiteral [] = Just []
fromLiteral (Literal c:cs) = fmap (c:) $ fromLiteral cs
fromLiteral _ = Nothing

ql :: Char -> Lexeme
ql = Quoted . Literal
