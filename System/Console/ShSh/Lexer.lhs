\chapter{Lexer module}

This is the module that breaks up the input into tokens.  It comes before
parsing, and is where we stop if we need to ask for another line of input.

\begin{code}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module System.Console.ShSh.Lexer where

import Control.Monad ( when, unless )
import Data.Maybe ( isJust, fromJust, listToMaybe )
import Data.Monoid ( Monoid, mappend, mconcat )

import Debug.Trace ( trace )

import Prelude hiding ( lex, last )
import Text.ParserCombinators.Parsec (
                     Parser, CharParser, GenParser, parse, runParser,
                     getState, setState,
                     choice, try, (<|>), (<?>),
                     skipMany, many1, many, eof, lookAhead,
                     char, letter, alphaNum, digit,
                     oneOf, noneOf, anyChar )

import System.Console.ShSh.Operator ( Operator(..), toOperator )

data Lexeme = Quote Char        -- ^we need to retain the quote chars
            | Literal Char      -- ^a letter
            | Quoted Lexeme     -- ^a quoted letter (or other thing)
            | ParamExp [Lexeme] -- ^parameter expansion $... and ${...}
            | Command [Token]   -- ^command substitution `...` and $(...)
            | ArithExp String   -- ^arithmetic expansion
            deriving ( Eq, Show )

data Token = Word [Lexeme]        -- ^several lexemes make up a word
           | IONum Int            -- ^digits before @<@ or @>@
           | Oper Operator        -- ^operator (split into redir/cont?)
           | Newline              -- ^simple enough...
           | EOF                  -- ^what does this do?
           -- That's all we do in this module...
--           | ReservedWord String  -- ^eventually we'll turn them into this
--           | Separator Operator   -- ^@;@ or @&@
--           | SubShell [Token]     -- ^pre-grouping...?
--           | SimpleCommand [Token]
--           | Pipeline [Token]     -- ^token list for each command
--           | AndOrList [Token]    -- ^build up hierarchical structure...
           deriving ( Eq, Show )
-- We might need to add others here...?

-- when parsing `...`, we first slurp everything up into a String,
-- quoting \\, \`, and \$ into \, `, $, and leavinf all other \ alone...
-- THEN, re-lex the inner part...

-- First pass: classify characters as quoted or not, spaces as
-- delimiters or not (grouped), and identify candidates for expansion.


-- NEW:
-------
-- We need a lexer with _state_ and with _lookahead delimiting_...
-- Alias expansion occurs within the lexer...
-- While we're at it, we should keep track of the SourcePos too...



-- We need to wait until we know we need to run the branch to even
-- try to parse the lexemes into tokens, since false && ${B$} doesn't
-- give an error, and echo ${X:=1} ${X:=2} prints "1 1"... so we can't
-- interpret the expansions till we actually need to do them!  But then
-- we never need to bother storing the tokens in the AST...


-- rudimentary structure for redirections...?
data InOut = Input | Output | ReadWrite
data Target = Fd Int | File FilePath | Close
data Redir = Redir InOut Target Target

-- Third pass: expansions, classifications, etc... - only when
-- needed...

-- |State: current tok, output so far
type Lexer = CharParser ([Lexeme],[Token])

store :: Lexeme -> Lexer ()
store l = getState >>= \(ls,ts) -> setState (l:ls,ts)

-- |Tell a quote
storeQ :: Char -> Lexer ()
storeQ = store . Quote

-- |Tell a literal
storeL :: Char -> Lexer ()
storeL = store . Literal

-- |Tell a quoted literal
storeQL :: Char -> Lexer ()
storeQL = store . Quoted . Literal

-- |Aliases for return True/False
loop,done :: Lexer Bool
loop = return True
done = return False

-- |repeat a job until it returns false
whileM_ :: Monad m => m Bool -> m ()
whileM_ job = do res <- job
                 when (res) $ whileM_ job

-- |Lex a newline
newline :: Lexer ()
newline = do (char '\n' >> try (char '\r'))
               <|> (char '\r' >> try (char '\n'))
             return ()

-- |Lex a blank (space or tab)
blank :: Lexer ()
blank = oneOf " \t" >> return ()

-- |Add a delimiter if there's not already one there...
-- We should hold onto some state - maybe a bool - for whether the
-- next token is subject to alias expansion.  Basically, this is at
-- the beginning, as well as after any control operator (;, \n, &, &&, ||, |).
delimit :: Lexer ()
delimit = do (ls,ts) <- getState
             unless (null ls) $ setState ([],Word (reverse ls):ts)

delimitIO :: Lexer ()
delimitIO = do (ls,ts) <- getState
               unless (null ls) $
                    setState $ if digits ls
                               then ([],IONum (read $ reverse $
                                               map fromLiteral ls):ts)
                               else ([],Word (reverse ls):ts)
    where digits [] = True
          digits (Literal c:xs) = c `elem` "0123456789"
          fromLiteral (Literal c) = c
          fromLiteral _ = error "Impossible"

-- |Return the accumulated tokens (in the correct order)
output :: Lexer [Token]
output = fmap (reverse.snd) getState

last :: Lexer (Maybe Lexeme)
last = fmap (listToMaybe.fst) getState

tell :: Token -> Lexer ()
tell t = do (ls,ts) <- getState
            let ts' = t:(if null ls then id else (Word (reverse ls):)) ts
            setState ([],ts')



infixl 3 +++
(+++) :: Monoid w => GenParser i s w -> GenParser i s w -> GenParser i s w
a +++ b = do w <- a
             w' <- b
             return $ w `mappend` w'

-- name :: Stringy s => CharParser st s
class Stringy s where
    fromString :: String -> s
    fromChar :: Char -> s
    fromChar c = fromString [c]
instance Stringy String where fromString = id
instance Stringy [Lexeme] where fromString l = map Literal l

name :: Stringy s => Lexer s
name = fmap fromString $ many1 (letter <|> char '_')
                         +++ many (alphaNum <|> char '_')

special :: Stringy s => Lexer s
special = fmap fromChar $ oneOf "*@#?-$!" <|> digit -- $1, etc, also

-- Here's another attempt...
lex :: [Lexer Bool] -> Lexer [Token]
lex m = whileM_ (choice m) >> delimit >> output -- this is extra unless no EOF

-- |@lex@ takes a delimiter that it's looking for to stop...
normalLex :: Maybe Char -> [Lexer Bool]
normalLex d | isJust d  = endOn (fromJust d):rest
            | otherwise = rest
            where rest = [lexEOF d, lexBackslash anyChar, lexDoubleQuotes,
                          lexSingleQuotes, lexDollar, lexBacktick,
                          lexOperator "", lexParen,
                          newline >> delimit >> tell Newline >> loop,
                          blank >> delimit >> loop,
                          lexComment,
                          anyChar >>= storeL >> loop]

lexOperator :: String -> Lexer Bool
lexOperator x = do o <- oneOf "<>|&;)"
                   if o `elem` "<>" then delimitIO else delimit
                   case toOperator $ x++[o] of
                     Just op -> (try $ lexOperator $ x++[o]) <|>
                                do tell $ Oper op
                                   loop
                     Nothing -> fail "" -- unread last char...

endOn :: Char -> Lexer Bool
endOn delim = lookAhead (char delim) >> done

lexEOF :: Maybe Char -> Lexer Bool
lexEOF d = do tok <- last
              eof
              when (isJust d) $ fail $ "expected "++[fromJust d]
              if (isJust tok)
                 then delimit
                 else tell EOF
              done

-- |This takes a lexer, either anyChar or oneOf "..."
lexBackslash :: Lexer Char -> Lexer Bool
lexBackslash f = do char '\\'
                    choice [newline -- just gobble the \ and the \n
                           ,do c <- f -- fails on line continuation
                               storeQ '\\'
                               storeQL c
                           ,eof >> fail "" -- @@@TEST THIS!
                           ,storeL '\\']
                    loop

lexSingleQuotes :: Lexer Bool
lexSingleQuotes = do char '\''
                     s <- many $ noneOf "'"
                     char '\''
                     storeQ '\'' >> mapM_ storeQL s >> storeQ '\''
                     loop

-- Instead, just make lex be "many $ choice modules" and have modules
-- return a Bool for done...?  endOn then gets a fail instead of a done.
-- We may need to modify many to instead run as many as possible and
-- return the bool of the last successful one...?  hmm...

subLex :: Lexer a -> Lexer a
subLex job = do s <- getState
                setState ([],[])
                res <- job
                setState s
                return res

lexDollar :: Lexer Bool
lexDollar = do char '$'
               choice [do char '{'
                          s <- subLex $ lex [endOn '}',
                                             lexBackslash $
                                               oneOf "}\\\"'$`",
                                             lexDoubleQuotes,
                                             lexSingleQuotes,
                                             lexDollar, lexBacktick,
                                             anyChar >>= storeL >> loop]
                          char '}'
                          store $ ParamExp $ fromTok s
                      ,do char '('
                          choice [do char '(' -- arithmetic expansion
                                     s <- subLex lexArithmetic
                                     store $ ArithExp s
                                 ,do t <- subLex $ lex $ normalLex $ Just ')'
                                     char ')'
                                     store $ Command t]
                      ,(name <|> special) >>= (store . ParamExp)]
               loop

lexBacktick :: Lexer Bool
lexBacktick = do char '`'
                 s <- cat $ choice [char '`' >> fail "" -- done
                                   ,char '\\' >> (one (oneOf "\\`$]") <|>
                                                  fmap ('\\':) (one anyChar))
                                   ,one anyChar]
                 case runParser (lex $ normalLex Nothing) ([],[]) "" s of
                   Left e  -> fail (show e) -- is this all we can do?
                   Right x -> store $ Command x
                 loop

fromTok :: [Token] -> [Lexeme]
fromTok [] = []
fromTok [Word ls] = ls
fromTok x = error $ "Impossible? frokTok on "++show x

quoted :: Lexer [Token] -> Lexer ()
quoted job = (mapM_ $ store . Quoted) `fmap` fromTok =<< subLex job

lexDoubleQuotes :: Lexer Bool
lexDoubleQuotes = do char '"' >> storeQ '"'
                     quoted $ lex [endOn '"',
                                   lexBackslash $ oneOf "\\\"$`",
                                   lexDollar, lexBacktick,
                                   anyChar >>= storeL >> loop]
                     char '"' >> storeQ '"'
                     loop

lexParen :: Lexer Bool
lexParen = do char '('
              delimit -- probably doesn't matter...?
              tell $ Oper LParen
              lex $ normalLex $ Just ')'-- go up to next paren...
              char ')' -- make sure it's actually there...
              tell $ Oper RParen
              loop

lexComment :: Lexer Bool
lexComment = do char '#'
                mt <- last
                if isJust mt then storeL '#'
                             else skipMany $ noneOf "\n"
                loop

one :: Functor f => f a -> f [a]
one = fmap (\a -> [a])
cat :: Monoid a => Lexer a -> Lexer a
cat = fmap mconcat . many

lexArithmetic :: Lexer String
lexArithmetic = cat parens +++ closeArith
    where closeArith = do char ')' -- first unmatched rparen...
                          ((char ')' >> return "") <|> -- suppress "))" output
                           return ")" +++ cat parens +++ closeArith)
                        <?> "\"))\""
          parens :: Lexer String
          parens = one (char '(') +++ cat parens +++ one (char ')') 
                   <|> many1 (noneOf "()") <?> ""

runLexer :: String -> Maybe [Token]
runLexer s = case runParser (lex $ normalLex Nothing) ([],[]) "" s of
               Left e  -> trace ("Error: "++show e) Nothing
               Right x -> Just x
\end{code}