{-# LANGUAGE PatternGuards #-}

module Language.Sh.Parser.Internal where

import Language.Sh.Parser.Parsec
import Language.Sh.Syntax

import Data.Char ( isDigit )
import Text.ParserCombinators.Parsec ( choice )

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
wordToInt (LitWord ds) | null $ filter (not . isDigit) ds = Just $ read ds
wordToInt _ = Nothing
