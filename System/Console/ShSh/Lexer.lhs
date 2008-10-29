\chapter{Lexer module}

This is the module that breaks up the input into tokens.  It comes before
parsing, and is where we stop if we need to ask for another line of input.

\begin{code}

module System.Console.ShSh.Lexer ( Token(..), tokenize ) where

import Text.ParserCombinator.Parsec ( parse, char, eof, (<|>) )

data Token = Word [EString] | Oper Operator | EOF

data EString = Literal String | ParamExp String
             | CommandSub [Token] | ArithExp String

data Operator = Async | Pipe | Semi | Less | Great | LParen | RParen
              | AndIf | OrIf | DSemi | DLess | DGreat | LessAnd
              | GreatAnd | LessGreat | DLessDash | Clobber

-- This seems like needless efficiency.  We could probably just make a
-- list [(Operator,String)] and use lookup and (lookup _ . (map swap))

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

readOper :: Parser Operator
readOper = (char '&' >> ((eof >> return Async) <|>
                         (char '&' >> eof >> return AndIf))) <|>
           (char '|' >> ((eof >> return Pipe) <|>
                         (char '|' >> eof >> return OrIf))) <|>
           (char ';' >> ((eof >> return Semi) <|>
                         (char ';' >> eof >> return DSemi))) <|>
           (char '<' >> ((eof >> return Less) <|>
                         (char '<' >> ((eof >> return DLess) <|>
                                       (char '<' >> char '-'
                                        >> eof >> return DLessDash))) <|>
                         (char '&' >> eof >> return LessAnd) <|>
                         (char '>' >> eof >> return LessGreat))) <|>
           (char '>' >> ((eof >> return Great) <|>
                         (char '>' >> eof >> return DGreat) <|>
                         (char '&' >> eof >> return GreatAnd) <|>
                         (char '|' >> eof >> return Clobber))) <|>
           (char '(' >> eof >> return LParen) <|>
           (char ')' >> eof >> return RParen)
                                              
toOperator :: String -> Maybe Operator
toOperator s = case parse readOper "" s of
                 Left _  -> Nothing
                 Right x -> Just x

-- We could use Either in a similar way - fmap and fail both work..
tokenize :: String -> Maybe [Token] -- Nothing means more input...
tokenize s = evalWriter $ evalStateT tok s

-- |This lets us treat this sort of like a writer monad...
-- The underscore version is for end of input...
tell_ :: Token -> Maybe [Token] -> Maybe [Token]
tell_ t = fmap (++[t])

tell :: Token -> String -> Maybe [Token] -> Maybe [Token]
tell t s ts = tok s Nothing $ fmap (++t) ts

-- |If the next char can be added onto the operator then do it, else
-- tokenize it and try again.
extendOperator :: Operator -> String -> Maybe [Token] -> Maybe [Token]
extendOperator o (c:cs) = case toOperator (show o++[c]) of
                            Just o' -> tok cs (Oper o')
                            Nothing -> tell o (c:cs)

-- |Append a @String@ to a @Maybe Token@ only if it's @Nothing@ or a @Word@.
append :: Maybe Token -> EString -> Maybe Token
append Nothing x = Just $ Word [x]
append (Just (Word xs)) (Literal s) = Just $ Word $ appLit xs s
append (Just (Word xs)) x = Just $ Word $ x:xs
append _ _ = undefined -- can't append to operator or EOF

appLit :: [EString] -> String -> [EString] -- note: BACKWARDS!
appLit (Literal s:xs) s' = Literal (s++s'):xs
appLit xs s = Literal s:xs

concat :: Maybe Token -> [EString] -> Maybe Token
concat Nothing [] = Nothing
concat Nothing xs = Just $ Word xs
concat (Just (Word xs)) xs' = Just $ Word $ xs'++xs
concat _ _ = undefined

data TokMode = SimpleParam | ParamExp | ComSubst | BackTick | ArithSubst

-- |This is the main function.
tok :: [TokMode]     -- ^stack of mode things
    -> String        -- ^the remaining input stream
    -> Maybe Token   -- ^the current token
    -> Maybe [Token] -- ^the token list so far
    -> Maybe [Token]
-- What do we do with extra unparsed data for submodes?
-- -> it's a pain to return a Maybe ([Token],String)...
-- Maybe we do want to be inside a monad...?

tok (SimpleParam:ms) "" t = ??? -- push the token, pop out of SimpleParam
tok (ParamExp:ms) "" t = Left 
tok [] "" Nothing  = tell_ EOF     -- what does this do?
tok [] "" (Just t) = tell_ t       -- tokenize last token, no EOF token.
tok [] _ (Just EOF) = undefined    -- this should never happen
tok [] s (Just (Oper o)) = extendOperator o s    -- try to add onto an operator
tok [] ('\\':[]) _ = const Nothing -- line continuation...
tok [] ('\\':'\n':cs) t = tok cs t --   "    "    "
tok [] ('\\':c:cs) t = tok cs $ append t $ Literal [c] -- add literal character
tok [] ('"':cs) t = tok rest $ concat t quot
    where (quot,rest) = findDQuote cs
tok [] ('\'':cs) t = tok rest $ append t $ Literal quot
    where (quot,rest) = findSQuote cs
tok [] ('$':cs) t = 


-- New version: make it into a State monad, including which mode we're
-- in and current token.
-- Might make a WriterT [Token], and maybe an ErrorT () so that fail
-- can signify unexpected end of input...?  This still doesn't cover
-- extra input on closeparens.... so maybe we need the input to be
-- part of the state, though it would be better to just pass it around...?
-- We could use fail to pass extra input if it's stacked on the right
-- side of the WriterT...

data LexMode = N  -- ^Normal - this should never go in the stack...?
             | P  -- ^Paren
             | DQ -- ^Double quote
             | SQ -- ^Single quote
             | BQ -- ^Backquote
             | SP -- ^Simple parameter
             | BP -- ^Brace parameter
             | CS -- ^Command substitution
             | AE -- ^Arithmetic expansion
             | HD Bool String String -- ^Heredoc <strip tabs> 
                                     --          <delimiter> <rest of line>
data LexState = LexState { lexMode :: [LexMode],
                           curTok :: Maybe Token,
                           allToks :: Maybe [Token] }

lex :: String -> Maybe ([Token],String)


\end{code}