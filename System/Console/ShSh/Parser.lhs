\chapter{Parser module}

This is the second stage of parsing.  We take the tokens and lexemes
from the first stage and turn them into an AST.

\begin{code}

module System.Console.ShSh.Parser ( Expression(..),
                                    parse, reservedWords ) where

import Debug.Trace ( trace )

import System.Console.ShSh.Lexer ( Lexeme(..), Token(..) )
import System.Console.ShSh.Operator ( Operator(..) )

import Text.ParserCombinators.Parsec ( SourcePos, GenParser, runParser,
                                       choice, try, (<|>), (<?>),
                                       lookAhead,
                                       many1, many, eof, skipMany )
import Text.Parsec.Pos ( initialPos )
import qualified Text.ParserCombinators.Parsec as PC

import qualified Text.Parsec.Expr as E

type P = GenParser (SourcePos,Token) ()

parseToken :: (Token -> Maybe a) -> P a
parseToken test = PC.token (show . snd) fst (test . snd)

token :: Token -> P Token
token l = parseToken $ \s -> if s==l then Just l else Nothing

oneOf :: [Token] -> P Token
oneOf ls = parseToken $ \s -> if s`elem`ls then Just s else Nothing

noneOf :: [Token] -> P Token
noneOf ls = parseToken $ \s -> if s`elem`ls then Nothing else Just s

oneOpOf :: [Operator] -> P Token
oneOpOf os = oneOf $ map Oper os

oper :: Operator -> P Token
oper o = oneOpOf [o]

redirectionOperator :: P Token
redirectionOperator = oneOpOf [Less,Great,DLess,DGreat,
                               LessAnd,GreatAnd,LessGreat,
                               DLessDash,Clobber]
                          <?> "redirection operator"

ioNum :: P Token
ioNum = parseToken $ \s -> case s of
                             IONum _ -> Just s
                             _       -> Nothing

newline :: P Token
newline = parseToken $ \s -> if s==Newline then Just s else Nothing

word :: P Token
word = parseToken $ \s -> case s of
                            Word _ -> Just s
                            _      -> Nothing

anyToken :: P Token
anyToken = parseToken $ \s -> Just s

data Expression = Simple [Token]
                | SubShell Expression
                | Expression :|: Expression
                | Expression :&&: Expression
                | Expression :||: Expression
                | Expression :>>: Expression -- semicolon
                | RunAsync Expression
                deriving ( Show )

infixl 4 :>>:
infixl 5 :||:,:&&:
infixr 6 :|:

simple :: P Expression
simple = fmap Simple $ many1 $ word <|> redirectionOperator <|> ioNum

parens :: P Expression -> P Expression
parens p = do oper LParen
              e <- p
              op <- oper RParen
              return $ SubShell e

compound :: P Expression
compound = parens expr <|> simple <?> "simple command or subshell"

-- The postfixing isn't working right... we should remove them from
-- the table and then look for either EOF, eof, ;, newline, or &
-- and then remove all the extra newlines and try again, joining with
-- :>>:.
table = [[inf (oper Pipe) (:|:) E.AssocRight]
        ,[inf (oper AndIf) (:&&:) E.AssocLeft
         ,inf (oper OrIf) (:||:) E.AssocLeft]]
    where inf s f assoc = E.Infix (s >> return f) assoc

terminator :: P (Expression -> Expression)
terminator = choice [oper Async >> return RunAsync
                    ,oper Semi >> return id
                    ,lookAhead (oper RParen) >> return id
                    ,eof >> return id
                    ,oneOf [Newline, EOF] >> return id]
                <?> "expression terminator"

newlines :: P ()
newlines = skipMany $ oneOf [Newline, EOF]

ex' :: Maybe Expression -> P Expression
ex' ex = do next <- E.buildExpressionParser table compound
                      <?> "expression"
            async <- terminator
            newlines
            case ex of
              Nothing -> return $ async next
              Just x  -> return $ x :>>: async next

ex'' :: Expression -> P Expression
ex'' x = choice [lookAhead (oper RParen) >> return x,
                 eof >> return x,ex' (Just x) >>= ex'']

expr :: P Expression
expr = ex' Nothing >>= ex''

parse :: [Token] -> Either String Expression
parse ts = case runParser expr () "" (map (\x->(initialPos "",x)) ts) of
             Left e  -> Left $ show e
             Right x -> Right x

reservedWords :: [String]
reservedWords = ["!", "{", "}", "case", "do", "done", "elif", "else",
                 "esac", "fi", "for", "fi", "in", "then", "until", "while"]

\end{code}