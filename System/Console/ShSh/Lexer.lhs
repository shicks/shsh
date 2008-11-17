
\chapter{Lexer module}

This is the module that breaks up the input into tokens.  It comes before
parsing, and is where we stop if we need to ask for another line of input.

\begin{code}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module System.Console.ShSh.Lexer where

import Control.Monad ( when, unless )
import Control.Monad.State ( State, execState, gets, modify )

import Data.Char ( isLetter, isDigit, isAlphaNum )
import Data.Maybe ( isJust, fromJust, listToMaybe )
import Data.Monoid ( Monoid, mappend, mconcat )

import Debug.Trace ( trace )

import System.Console.ShSh.Operator ( Operator(..), toOperator )

tr = const id
--tr = trace

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

data LexerMode = Normal -- make an Alias mode, like normal but preventing loops?
               | Op String
               | DQuote
               | SQuote String
               | Backslash LexerMode   -- ^behavior depends on prev mode
               | Paren       -- ^subshell (could save "parent" mode!)
               | Dollar String         -- ^varname (so far)
               | DollarBrace           -- ^collect lexemes (incl $(), etc)
               | DollarParen
               | DollarDParen String Int
               | Backtick String       -- ^lex afterwards
               | Eat Char
               | Comment
               deriving ( Show )

data LexerState = LS { input :: String,
                       word :: [Lexeme],
                       stream :: [Token],
                       aliases :: [(String,String)],
                       aliasOK :: Bool,
                       modes :: [LexerMode],
                       stack :: [([Lexeme],[Token])],
                       inputStack :: [String]
                     } -- or just store words & stream?  aliases?

-- Try something simpler...
type Lexer = State LexerState

runLexer :: [(String,String)] -> String -> Either String [Token]
runLexer as s = let result = execState (lexInput >> lexEOF) $
                             LS s [] [] as True [Normal] [] []
                in case modes result of
                     [] -> Left "Impossible: Normal is always at bottom"
                     [Normal] -> Right $ reverse $ stream result
                     (Op _:_) -> Left "Impossible: Unterminated operator"
                     (DQuote:_) -> Left "Expecting `\"'"
                     (SQuote _:_) -> Left "Expecting `''"
                     (Backslash _:_) -> Left "Unexpected EOF"
                     (Paren:_) -> Left "Expecting `)'"
                     (Dollar _:_) -> Left "Impossible: Unterminated varname"
                     (DollarBrace:_) -> Left "Expecting `}'"
                     (DollarParen:_) -> Left "Expecting `)'"
                     (DollarDParen _ _:_) -> Left "Expecting `))'"
                     (Backtick _:_) -> Left "Expecting ``'"
                     (Comment:_) -> Left "Impossible: Unterminated comment"

-- |This is the main loop (very simple now!)
lexInput :: Lexer ()
lexInput = do i <- gets input
              case i of
                ""   -> return ()
                c:cs -> do m <- head `fmap` gets modes -- unsafe?
                           tr ("Input: "++[c]++", Mode: "++show m) consume
                           w <- gets word
                           unless (null w) $ lexDelimiter m
                           lexChar m c
                           lexInput

-- |Add a single lexeme onto the accumulating word
store :: Lexeme -> Lexer ()
store l = modify $ \s -> s { word = l:word s }

-- |Tell a quote
storeQ :: Char -> Lexer ()
storeQ = store . Quote

-- |Tell a literal
storeL :: Char -> Lexer ()
storeL = store . Literal

-- |Tell a quoted literal
storeQL :: Char -> Lexer ()
storeQL = store . Quoted . Literal

-- |Consume the front character off the input stream.  This happens
-- by default before calling 'lexChar'.
consume :: Lexer ()
consume = modify $ \s -> s { input = drop 1 $ input s }

-- |Replace the given character back onto the input stream
replace :: Char -> Lexer ()
replace c = modify $ \s -> s { input = c:input s }

-- |Pop a lexer mode
popMode :: Lexer ()
popMode = modify $ \s -> tr ("popMode: "++show (head $ modes s)) $ s { modes = drop 1 $ modes s }

-- |Push a lexer mode
pushMode :: LexerMode -> Lexer ()
pushMode m = modify $ \s -> tr ("pushMode: "++show m) $ s { modes = m:modes s }

-- |Replace the current (top) mode
setMode :: LexerMode -> Lexer ()
setMode m = do ms <- gets modes
               tr ("setMode: "++show m++" (modes="++show ms) $
                     popMode >> pushMode m
               ms' <- gets modes
               modify $ \s -> tr ("after: "++show ms') s

-- |Return the current mode
getMode :: Lexer LexerMode
getMode = head `fmap` gets modes

-- |This is the work horse.  A few of the more complicated modes
-- are broken out into dedicated functions.
lexChar :: LexerMode -> Char -> Lexer ()

lexChar Normal c = lexNormal c

lexChar (Op s) c = lexOperator s c

lexChar DQuote c = lexDQuote c

lexChar (SQuote s) '\'' = do mapM_ storeQL $ reverse s
                             storeQ '\''
                             popMode
lexChar (SQuote s) c = setMode $ SQuote $ c:s

lexChar (Backslash m) c = lexEscape m c

lexChar Paren ')' = popMode >> replace ')' >> lexInput
lexChar Paren c = lexNormal c -- doesn't matter if inside $()...

lexChar (Dollar "") '{' = saveState >> setMode DollarBrace
lexChar (Dollar "") '(' = saveState >> setMode DollarParen
lexChar (Dollar "") c | isLetter c || c=='_' = setMode $ Dollar [c]
                      | isDigit c || c `elem` "*@#?-$!" -- test: echo $A^
                          = do store $ ParamExp $ [Literal c]
                               popMode
                      | otherwise = do replace c
                                       popMode
                                       m <- getMode
                                       case m of
                                         DQuote -> storeQL '$'
                                         _      -> storeL '$'
                                       
lexChar (Dollar s) c | isAlphaNum c || c=='_' = setMode $ Dollar $ c:s
                     | otherwise = do store $ ParamExp $ reverse $ literal s
                                      replace c
                                      popMode

-- Extended expansions ${ ... }
lexChar DollarBrace '}' = undefined -- end expansion here (popState, etc)
lexChar DollarBrace c = undefined -- should be sort of like normalLex

-- Command substitutions $( ... )
lexChar DollarParen '(' = do ws <- getWS
                             if null (fst ws) && null (snd ws)
                                then setMode $ DollarDParen "" 0
                                else lexNormal '('
lexChar DollarParen ')' = do delimit -- maybe?
                             ts <- gets stream
                             store $ ParamExp $ reverse ts
                             restoreState
lexChar DollarParen c = lexNormal c

-- Arithmetic substitutions $(( ... ))
lexChar (DollarDParen s n) '(' = setMode $ DollarDParen ('(':s) (n+1)
lexChar (DollarDParen s (-1)) ')' = do popMode
                                       storeQQ $ ArithExp $
                                               take (length s - 1) s
lexChar (DollarDParen s n) ')' = setMode $ DollarDParen (')':s) (n-1)
lexChar (DollarDParen s n) c = setMode $ DollarDParen (c:s) n

-- Backtick substitution ` ... `
lexChar (Backtick s) '`' = do case runLexer s of
                                Left err -> fail err -- FATAL error...?
                                Right ts -> store $ CommandSub ts
                              popMode
-- cf. $ false && echo `echo "1` && "
--     dash: Syntax error

lexChar (Backtick s) '\\' = pushMode $ Backslash $ Backtick s
lexChar (Backtick s) c = setMode $ Backtick $ c:s

lexChar (Eat c) c' | c==c'     = popMode
                   | otherwise = replace c' >> popMode

lexChar Comment '\n' = replace '\n' >> popMode -- combine these?!?
lexChar Comment '\r' = replace '\r' >> popMode
lexChar Comment _ = return ()

-- |@lexEOF@ takes care of wrapping up anything like 'Dollar' mode
-- that can end on an EOF.  We do this by calling a helper function,
-- @lexEOF'@, which will often call @lexEOF@ back again after popping
-- the mode.
lexEOF :: Lexer ()
lexEOF = getMode >>= lexEOF'

lexEOF' :: LexerMode -> Lexer ()
lexEOF' (Dollar s) = store (ParamExp $ reverse $ literal s) >> popMode >> lexEOF
lexEOF' Comment = popMode >> lexEOF
lexEOF' (Op s) = do tell $ Oper $ fromJust $ toOperator s
                    popMode >> lexEOF
lexEOF' Normal = do delimit -- could put this in an unless...
lexEOF' _ = return () -- no recovery possible...

saveState :: Lexer ()
saveState = modify $ \s -> s { word = [], stream = [],
                               stack = (word s,stream s):stack s }

restoreState :: Lexer ()
restoreState = modify $ \s -> let w = fst $ head $ stack s
                                  t = snd $ head $ stack s
                              in s { word = w, stream = s,
                                     stack = drop 1 $ stack s }

isNewline :: Char -> Bool
isNewline = (`elem` "\n\r")

eatOtherNewline :: Char -> Lexer ()
eatOtherNewline c = pushMode $ Eat $ fromJust $ lookup c
                                                [('\n','\r'),('\r','\n')]
isBlank :: Char -> Bool
isBlank = (`elem` " \t")

lexNormal '\\' = pushMode $ Backslash Normal
lexNormal '"' = storeQ '"' >> pushMode DQuote
lexNormal '\'' = storeQ '\'' >> pushMode (SQuote "")
lexNormal '$' = pushMode $ Dollar ""
lexNormal '`' = pushMode $ Backtick ""
lexNormal '(' = tell (Oper LParen) >> pushMode Paren
lexNormal c | c `elem` "<>" = delimitIO >> pushMode (Op [c])
            | c `elem` ")&|;" = delimit >> pushMode (Op [c])
            | isNewline c = delimit >> tell Newline >> eatOtherNewline c
            | isBlank c   = delimit
lexNormal '#' = do w <- gets word
                   if null w then storeL '#'
                             else pushMode Comment
lexNormal c = storeL c

-- somewhere buried here is where aliases go...

lexOperator :: String -> Char -> Lexer ()
lexOperator s c = case toOperator $ s++[c] of
                    Just op -> setMode $ Op $ s++[c]
                    Nothing -> do replace c
                                  popMode
                                  tell $ Oper $ fromJust $ toOperator s

-- |Deal with backslashes...
lexEscape _ c | isNewline c = popMode >> eatOtherNewline c
lexEscape Normal c = storeQ '\\' >> storeL c >> popMode
lexEscape DQuote c | c `elem` "\"$`" = storeQ '\\' >> storeQL c
                   | otherwise       = storeQL '\\' >> storeQL c
lexEscape DollarBrace c = undefined -- ,,,? - probably escape ANY (cf Normal)
lexEscape DollarParen c = undefined -- ... - cf normal
lexEscape (DollarDParen s n) c = undefined -- EEK!  `` and $() are allowed!
-- We might want to make DollarDParen behave more like DollarParen... storing
-- words and tokens, etc...!

-- |@lexDQuote@ just builds onto the current literal - never tokenizes
lexDQuote '$' = pushMode $ Dollar ""
lexDQuote '"' = store (Quote '"') >> popMode
lexDQuote '`' = pushMode $ Backtick ""
lexDQuote '\\' = pushMode $ Backslash DQuote
lexDQuote c = storeQL c



getWS :: Lexer ([Lexeme],[Token])
getWS = gets $ \s -> (word s,stream s)

-- |Add a delimiter if there's not already one there...
-- We should hold onto some state - maybe a bool - for whether the
-- next token is subject to alias expansion.  Basically, this is at
-- the beginning, as well as after any control operator (;, \n, &, &&, ||, |).
delimit :: Lexer ()
delimit = do (ls,ts) <- getWS
             unless (null ls) $ modify $
                        \s -> s { word = [], stream = Word (reverse ls):ts }
--             unless (null ls) $ if a
--                                then expandAlias as (reverse ls) ts
--                                else setState $ LS [] (Word (reverse ls):ts) as ms

-- |This is a version that incidentally checks if the word it's delimiting
-- is eligible to be an @IONum@.
delimitIO :: Lexer ()
delimitIO = do (ls,ts) <- getWS
               unless (null ls) $
                      if digits ls
                      then modify $ \s ->
                          s { word = [], 
                              stream = IONum (read $ reverse $
                                              map fromLiteral ls):ts }
                      else modify $ \s -> s { word = [],
                                              stream = Word (reverse ls):ts }
    where digits [] = True
          digits (Literal c:xs) = c `elem` "0123456789"
          fromLiteral (Literal c) = c -- overwrites what we define just below
          fromLiteral _ = error "Impossible"

-- export these and remove from Expansions
fromLiteral :: Monad m => [Lexeme] -> m String
fromLiteral = mapM $ \x -> do {Literal c <- return x; return c}

literal :: String -> [Lexeme]
literal = map Literal

tell :: Token -> Lexer ()
tell t = do delimit
            modify $ \s -> s { stream = t:stream s }
\end{code}