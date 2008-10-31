\chapter{Lexer module}

This is the module that breaks up the input into tokens.  It comes before
parsing, and is where we stop if we need to ask for another line of input.

\begin{code}

module System.Console.ShSh.Lexer ( Token(..), Operator(..), EString(..),
                                   runLexer ) where

import Control.Monad ( when )
import Data.Maybe ( isJust, fromJust )
import Data.Monoid ( Monoid, mappend, mconcat )

import Prelude hiding ( lex )
import Text.ParserCombinators.Parsec (
                     Parser, CharParser, GenParser, parse, runParser,
                     getState, setState,
                     choice, try, (<|>),
                     many1, many, eof, lookAhead,
                     char, letter, alphaNum, digit,
                     oneOf, noneOf, anyChar )

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

data Token = Word [EString] | Oper Operator | Newline | EOF
           deriving ( Show )

-- |This is a type for anything that can make up a single "word", i.e.
-- anything within double quotes.  We build up a list of all these,
-- expand them each before concatenating them.  Note that we need to
-- keep track of the double quotes because, e.g.
--   A=\*
--   echo $A # prints list of files
--   echo "$A" # prints '*'
-- So while expanding variables (and literals) we identify glob tokens
-- (and other things), and inside Quoted, we just make the globs literal...
data EString = Literal String | Quoted [EString] | ParamExp String
             | CommandSub [Token] | ArithExp String
           deriving ( Show )

-- |State: current tok, output so far
type Lexer = CharParser (Maybe Token,[Token])

-- |This only operates on words...  Note that words are stored backwards,
-- and thus need to be reversed at the end...
append :: [EString] -> EString -> [EString]
append (Literal s:xs) (Literal s') = Literal (s++s'):xs
append xs x = x:xs

delimit_ :: Lexer ()
delimit_ = do (t,ts) <- getState
              when (isJust t) $ setState (Nothing,fixWord (fromJust t):ts)

delimit :: Token -> Lexer ()
delimit t' = getState >>= \(t,ts) -> setState (t,fixWord t':ts)

fixWord :: Token -> Token
fixWord (Word x) = Word $ reverse x
fixWord x = x

done :: Lexer [Token]
done = getState >>= \(_,ts) -> return $ reverse ts

curTok :: Lexer (Maybe Token)
curTok = getState >>= \(t,_) -> return t

setTok :: Token -> Lexer ()
setTok t = getState >>= \(_,ts) -> setState (Just t,ts)

getCurWord :: Lexer [EString]
getCurWord = do (t,ts) <- getState
                case t of
                  Just (Word w) -> setState (Nothing,ts) >> return w
                  _ -> delimit_ >> return []

appendWord :: EString -> Lexer ()
appendWord x = do t <- curTok
                  case t of
                    Just (Word w) -> setTok $ Word $ append w x
                    Just t        -> delimit_ >> setTok (Word [x])
                    Nothing       -> setTok $ Word [x]

appendChar :: Char -> Lexer ()
appendChar c = appendWord $ Literal [c]

infixl 3 +++
(+++) :: Monoid w => GenParser i s w -> GenParser i s w -> GenParser i s w
a +++ b = do w <- a
             w' <- b
             return $ w `mappend` w'

name :: CharParser st String
name = many1 (letter <|> char '_') +++ many (alphaNum <|> char '_')

special :: CharParser st String
special = do c <- oneOf "*@#?-$!" <|> digit -- positional parameters too...
             return [c]

-- |@lex@ takes a delimiter that it's looking for to stop...
lex :: Maybe Char -> Lexer [Token]
lex d = do tok <- curTok
           choice [case d of    -- delimit but don't consume, return if wanted
                     Just delim -> lookAhead (char delim) >> done
                     Nothing    -> fail ""
                  ,do eof
                      when (isJust d) $ fail $ "expected "++[fromJust d]
                      case tok of
                        Nothing -> delimit EOF >> done
                        Just t  -> delimit_ >> done
                  ,try $ case tok of
                           Just (Oper op) ->
                               do c <- anyChar
                                  case toOperator (show op++[c]) of 
                                    Just op' -> setTok (Oper op')
                                    Nothing  -> delimit_ >> fail "" -- refail
                                  lex d             -- (or we could put back)
                           _ -> fail ""
                  ,do char '\\'    -- The Quoted here is important for e.g.
                      c <- anyChar -- alias expansion, globbing, etc.
                      when (c/='\n') $ appendWord $ Quoted $ [Literal [c]]
                      lex d
                  ,lexQuoted >> lex d
                  ,do char '\''
                      s <- many $ noneOf "'"
                      char '\''
                      appendWord $ Literal s
                      lex d
                  ,lexDollar >> lex d
                  ,lexBacktick >> lex d
                  ,do o <- oneOf "<>|&;)"
                      delimit_
                      let op = fromJust $ toOperator [o]
                      setTok $ Oper op
                      lex d
                  ,do char '('
                      delimit_
                      delimit (Oper LParen)
                      lex $ Just ')' -- ignore output (still saved in state)
                      char ')' -- make sure it's actually there...
                      delimit (Oper RParen)
                      lex d
                  ,char '\n' >> delimit_ >> delimit Newline >> lex d
                  ,char ' ' >> delimit_ >> lex d
                  ,case tok of
                     Just (Word w) -> do c <- anyChar
                                         appendChar c
                                         lex d
                     _ -> fail ""
                  ,(char '#' >> many (noneOf "\n") >> lex d)
                  ,do c <- anyChar -- catch-all...
                      appendChar c
                      lex d]

subLexer :: Lexer a -> Lexer a
subLexer lexer = do t <- curTok
                    case t of
                      Just (Word _) -> return ()
                      _ -> delimit_
                    save <- getState
                    setState (Nothing,[])
                    ret <- lexer
                    setState save
                    return ret

lexDollar :: Lexer ()
lexDollar = do char '$'
               choice [do char '{'
                          s <- many (noneOf "}") 
                          char '}' -- this works b/c predictive
                          appendWord $ ParamExp s
                      ,do char '('
                          choice [char '(' >> lexArithmetic,
                                  do t <- subLexer $ lex $ Just ')'
                                     char ')'
                                     appendWord $ CommandSub t]
                      ,(name <|> special) >>= (appendWord . ParamExp)]

lexBacktick :: Lexer ()
lexBacktick = do char '`'
                 s <- many $ (char '\\' >> anyChar) <|> noneOf "`"
                 char '`'
                 case runParser (lex Nothing) (Nothing,[]) "" s of
                   Left e  -> fail (show e) -- is this all we can do?
                   Right x -> appendWord $ CommandSub x

lexQuoted :: Lexer ()
lexQuoted = do char '"'
               t <- subLexer $ do many $ choice [lexDollar, lexBacktick
                                                ,char '\\' >>
                                                 ((oneOf "$`\\\"" >>= appendChar)
                                                   <|> appendChar '\\')
                                                ,noneOf "\"" >>= appendChar]
                                  (es,_) <- getState
                                  case es of
                                    Just (Word e) -> return e
                                    Nothing -> return []
                                    _ -> fail "Got something other than a word"
               char '"'
               appendWord $ Quoted $ reverse t

one :: Functor f => f a -> f [a]
one = fmap (take 1 . repeat)
cat :: (Monoid a,Functor f) => f [a] -> f a
cat = fmap mconcat

parens :: Lexer String
parens = one (char '(') +++ cat (many parens) +++ one (char ')') 
         <|> many1 (noneOf "()")

lexArithmetic :: Lexer ()
lexArithmetic = do s <- cat (many parens) +++ closeArith
                   appendWord $ ArithExp s
    where closeArith = do char ')' -- first unmatched rparen...
                          ((char ')' >> return "") <|> -- suppress "))" output
                           return ")" +++ one anyChar +++ closeArith)

runLexer :: String -> Maybe [Token]
runLexer s = case runParser (lex Nothing) (Nothing,[]) "" s of
               Left _  -> Nothing
               Right x -> Just x
\end{code}