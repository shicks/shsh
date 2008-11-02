\chapter{Lexer module}

This is the module that breaks up the input into tokens.  It comes before
parsing, and is where we stop if we need to ask for another line of input.

\begin{code}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module System.Console.ShSh.Lexer where

import Control.Monad ( when )
import Data.Maybe ( isJust, fromJust, listToMaybe )
import Data.Monoid ( Monoid, mappend, mconcat )

import Debug.Trace ( trace )

import Prelude hiding ( lex, last )
import Text.ParserCombinators.Parsec (
                     Parser, CharParser, GenParser, parse, runParser,
                     getState, setState,
                     choice, try, (<|>),
                     many1, many, eof, lookAhead,
                     char, letter, alphaNum, digit,
                     oneOf, noneOf, anyChar )

import System.Console.ShSh.Operator ( Operator(..), toOperator )

data Lexeme = Delimiter         -- ^spaces/tabs
            | Quote Char        -- ^we need to retain the quote chars
            | Literal Char      -- ^a letter
            | Quoted Lexeme     -- ^a quoted letter (or other thing)
            | Oper Operator     -- ^basic operator
            | ParamExp [Lexeme] -- ^parameter expansion $... and ${...}
            | Command [Lexeme]  -- ^command substitution `...` and $(...)
            | ArithExp String   -- ^arithmetic expansion
            | Newline           -- ^simple enough...
            | EOF               -- ^what does this do?
            deriving ( Eq, Show )

-- when parsing `...`, we first slurp everything up into a String,
-- quoting \\, \`, and \$ into \, `, $, and leavinf all other \ alone...
-- THEN, re-lex the inner part...

-- First pass: classify characters as quoted or not, spaces as
-- delimiters or not (grouped), and identify candidates for expansion.

-- This could possibly go into a separate Lister module...
data CmdList = Simple [Lexeme]
             | SubShell CmdList
             | CmdList :||: CmdList
             | CmdList :&&: CmdList
             | CmdList :>>: CmdList -- what about terminal ;?
             | CmdList :>>>: CmdList -- terminal \n?
             | CmdList :&: CmdList
             | CmdList :|: CmdList

-- Second pass: group lists - note that control structures make use
-- of ; and \n, etc, so we can't go too crazy with the above... maybe
-- merge them together?  Get rid of the Untokenized constructor, and
-- as soon as we start a new CmdList, check for aliases, etc...

-- We need to wait until we know we need to run the branch to even
-- try to parse the lexemes into tokens, since false && ${B$} doesn't
-- give an error, and echo ${X:=1} ${X:=2} prints "1 1"... so we can't
-- interpret the expansions till we actually need to do them!  But then
-- we never need to bother storing the tokens in the AST...

-- This will probably go into the separate parser module...
data Token = For   -- these need data...?
           | While
           | Case
           | If    -- ...
           | Redirection Redir
           | Word String
           | CommandTok String
           | Assignment String String

-- rudimentary structure for redirections...?
data InOut = Input | Output | ReadWrite
data Target = Fd Int | File FilePath | Close
data Redir = Redir InOut Target Target

-- Third pass: expansions, classifications, etc... - only when
-- needed...

-- |State: current tok, output so far
type Lexer = CharParser [Lexeme]

tell :: Lexeme -> Lexer ()
tell t = getState >>= (setState . (t:))

-- |Tell a quote
tellQ :: Char -> Lexer ()
tellQ = tell . Quote

-- |Tell a literal
tellL :: Char -> Lexer ()
tellL = tell . Literal

-- |Tell a quoted literal
tellQL :: Char -> Lexer ()
tellQL = tell . Quoted . Literal

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
delimit :: Lexer ()
delimit = do ts <- getState
             case ts of
               Delimiter:_ -> return ()
               _ -> setState $ Delimiter:ts

-- |Return the accumulated tokens (in the correct order)
output :: Lexer [Lexeme]
output = fmap reverse getState

last :: Lexer (Maybe Lexeme)
last = fmap listToMaybe getState

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
lex :: [Lexer Bool] -> Lexer [Lexeme]
lex m = whileM_ (choice m) >> output

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
                          anyChar >>= tellL >> loop]

lexOperator :: String -> Lexer Bool
lexOperator x = do o <- oneOf "<>|&;)"
                   case toOperator $ x++[o] of
                     Just op -> (try $ lexOperator $ x++[o]) <|>
                                do tell $ Oper $ fromJust $ toOperator x
                                   delimit -- never have Oper as last tok
                                   loop
                     Nothing -> fail "" -- unread last char...

endOn :: Char -> Lexer Bool
endOn delim = lookAhead (char delim) >> done

lexEOF :: Maybe Char -> Lexer Bool
lexEOF d = do tok <- last
              eof
              when (isJust d) $ fail $ "expected "++[fromJust d]
              when (tok == Just Delimiter) $ tell EOF
              done

-- |This takes a lexer, either anyChar or oneOf "..."
lexBackslash :: Lexer Char -> Lexer Bool
lexBackslash f = do char '\\'
                    choice [newline -- just gobble the \ and the \n
                           ,do c <- f -- fails on line continuation
                               tellQ '\\'
                               tellQL c
                           ,eof >> fail "" -- @@@TEST THIS!
                           ,tellL '\\']
                    loop

lexSingleQuotes :: Lexer Bool
lexSingleQuotes = do char '\''
                     s <- many $ noneOf "'"
                     char '\''
                     tellQ '\'' >> mapM_ tellQL s >> tellQ '\''
                     loop

-- Instead, just make lex be "many $ choice modules" and have modules
-- return a Bool for done...?  endOn then gets a fail instead of a done.
-- We may need to modify many to instead run as many as possible and
-- return the bool of the last successful one...?  hmm...

subLex :: Lexer a -> Lexer a
subLex job = do s <- getState
                setState []
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
                                             anyChar >>= tellL >> loop]
                          char '}'
                          tell $ ParamExp s
                      ,do char '('
                          choice [do char '(' -- arithmetic expansion
                                     s <- subLex lexArithmetic
                                     tell $ ArithExp s
                                 ,do t <- subLex $ lex $ normalLex $ Just ')'
                                     char ')'
                                     tell $ Command t]
                      ,(name <|> special) >>= (tell . ParamExp)]
               loop

lexBacktick :: Lexer Bool
lexBacktick = do char '`'
                 s <- cat $ choice [char '`' >> fail "" -- done
                                   ,char '\\' >> (one (oneOf "\\`$]") <|>
                                                  fmap ('\\':) (one anyChar))
                                   ,one anyChar]
                 case runParser (lex $ normalLex Nothing) [] "" s of
                   Left e  -> fail (show e) -- is this all we can do?
                   Right x -> tell $ Command x
                 loop

quoted :: Lexer [Lexeme] -> Lexer ()
quoted job = (mapM_ $ tell . Quoted) =<< subLex job

lexDoubleQuotes :: Lexer Bool
lexDoubleQuotes = do char '"' >> tellQ '"'
                     quoted $ lex [endOn '"',
                                   lexBackslash $ oneOf "\\\"$`",
                                   lexDollar, lexBacktick,
                                   anyChar >>= tellL >> loop]
                     char '"' >> tellQ '"'
                     loop

lexParen :: Lexer Bool
lexParen = do char '('
              delimit -- probably doesn't matter...?
              tell $ Oper LParen
              lex $ normalLex $ Just ')'-- go up to next paren...
              char ')' -- make sure it's actually there...
              tell $ Oper RParen
              delimit -- ???
              loop

lexComment :: Lexer Bool
lexComment = do char '#'
                mt <- last
                if isComment mt then many_ $ noneOf "\n"
                                else tellL '#'
                loop
    where isComment Nothing = True
          isComment (Just Newline) = True
          isComment (Just EOF) = True
          isComment (Just (Oper _)) = True
          isComment _ = False

one :: Functor f => f a -> f [a]
one = fmap (\a -> [a])
cat :: Monoid a => Lexer a -> Lexer a
cat = fmap mconcat . many
many_ :: Lexer a -> Lexer ()
many_ f = many f >> return ()

lexArithmetic :: Lexer String
lexArithmetic = cat parens +++ closeArith
    where closeArith = do char ')' -- first unmatched rparen...
                          ((char ')' >> return "") <|> -- suppress "))" output
                           return ")" +++ cat parens +++ closeArith)
          parens :: Lexer String
          parens = one (char '(') +++ cat parens +++ one (char ')') 
                   <|> many1 (noneOf "()")

runLexer :: String -> Maybe [Lexeme]
runLexer s = case runParser (lex $ normalLex Nothing) [] "" s of
               Left e  -> trace ("Error: "++show e) Nothing
               Right x -> Just x
\end{code}