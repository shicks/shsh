\chapter{Operator module}

We define the operator data type here.  Maybe also some functions to
deal with them...

\begin{code}
module System.Console.ShSh.Operator( Operator(..), toOperator ) where

import Text.ParserCombinators.Parsec ( Parser, parse, choice, eof, char )

data Operator = Async | Pipe | Semi | Less | Great | LParen | RParen
              | AndIf | OrIf | DSemi | DLess | DGreat | LessAnd
              | GreatAnd | LessGreat | DLessDash | Clobber
              deriving ( Eq )

-- This seems like needless efficiency.  We could probably just make a
-- list [(Operator,String)] and use lookup and (lookup _ . (map swap))

-- We should build the IO Numbers into the operators...?
-- [n]>, [n]<, [n]>>, [n]<<, [n]<<-, [n]>|, [n]>&[n'], [n]<&[n'], [n]<>

instance Show Operator where
    showsPrec _ Async s     = '&':s
    showsPrec _ Pipe s      = '|':s
    showsPrec _ Semi s      = ';':s
    showsPrec _ Less s      = '<':s
    showsPrec _ Great s     = '>':s
    showsPrec _ LParen s    = '(':s
    showsPrec _ RParen s    = ')':s
    showsPrec _ AndIf s     = '&':'&':s
    showsPrec _ OrIf s      = '|':'|':s
    showsPrec _ DSemi s     = ';':';':s
    showsPrec _ DLess s     = '<':'<':s
    showsPrec _ DGreat s    = '>':'>':s
    showsPrec _ LessAnd s   = '<':'&':s
    showsPrec _ GreatAnd s  = '>':'&':s
    showsPrec _ LessGreat s = '<':'>':s
    showsPrec _ DLessDash s = '<':'<':'-':s
    showsPrec _ Clobber s   = '>':'|':s

data ParseTree a = Parser () :-> a
                 | Parser () :-< [ParseTree a]
(-<) :: Parser c -> [ParseTree a] -> ParseTree a
a -< b = (a >> return ()) :-< b
(-->) :: Parser c -> a -> ParseTree a
a --> b = (a >> return ()) :-> b
infixl 0 -->,-< -- these need to be slower than >>

lchar :: Char ->  Parser ()
lchar c = char c >> eof

ops :: [ParseTree Operator]
ops = [char '&' -< [eof --> Async
                   ,lchar '&' --> AndIf]
      ,char '|' -< [eof --> Pipe
                   ,lchar '|' --> OrIf]
      ,char ';' -< [eof --> Semi
                   ,lchar ';' --> DSemi]
      ,char '<' -< [eof --> Less
                   ,char '<' -< [eof --> DLess
                                ,lchar '-' --> DLessDash]
                   ,lchar '>' --> LessGreat
                   ,lchar '&' --> LessAnd]
      ,char '>' -< [eof --> Great
                   ,lchar '>' --> DGreat
                   ,lchar '&' --> GreatAnd
                   ,lchar '|' --> Clobber]
      ,lchar '(' --> LParen
      ,lchar ')' --> RParen]

parseTree :: [ParseTree a] -> Parser a
parseTree = choice . map pt
    where pt (f :-< t) = f >> parseTree t
          pt (f :-> v) = f >> return v

toOperator :: String -> Maybe Operator
toOperator s = case parse (parseTree ops) "" s of
                 Left _  -> Nothing
                 Right x -> Just x

\end{code}