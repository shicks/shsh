-- |Here we use the stuff defined in the AST and Parsec modules
-- to parse things.

module Language.Sh.Parser ( parse ) where

import Language.Sh.Parser.Internal
import Language.Sh.Parser.Parsec
import Language.Sh.Syntax

import Text.ParserCombinators.Parsec ( choice, manyTill, eof, many1,
                                       skipMany,
                                       (<|>), (<?>), many, try, count,
                                       sepBy1, notFollowedBy, lookAhead,
                                       getInput, setInput, runParser
                                     )
import Control.Monad ( unless )
import Data.List ( (\\) )
import Data.Char ( isDigit )
import Debug.Trace ( trace )

-- We don't actually really need Parsec3 - could adapt that Parsec2 source...
-- Also, this should maybe be a debug switch...?
-- #ifdef HAVE_PARSEC3
-- #include "Language/Sh/Parser/safemany.h"
-- #endif

data WordContext = NormalContext | ParameterContext

delimiters :: WordContext -> String
delimiters NormalContext = "&|;<>()# \t\r\n"
delimiters ParameterContext = "}" -- don't separate on spaces - do that later...

cnewline :: P ()
cnewline = do spaces
              (do char '#'
                  skipMany (noneOf "\n\r")
                  newline <|> eof) <|> newline     <?> ""

statement :: P Statement
statement = spaces >> aliasOn >>
            choice [do char '('
                       openParen
                       cs <- many command
                       schar ')'
                       closeParen
                       rs <- many redirection
                       spaces
                       return $ Subshell cs rs
                   ,do s <- statementNoSS
                       case s of -- needed to prevent errors w/ 'many'
                         Statement [] [] [] -> fail "empty statement"
                         s -> return s
                   ]

-- Once we know we don't have a subshell...
-- We could probably wrap these into one function, since there's a fair
-- amount of repitition here...
statementNoSS :: P Statement
statementNoSS = do spaces >> aliasOn
                   s <- choice [expandAlias >> statementNoSS
                               ,try $ do a <- assignment
                                         fmap (addAssignment a) statementNoSS
                               ,try $ do r <- redirection
                                         fmap (addRedirection r) statementNoSS
                               ,simpleStatement]
                   spaces
                   return s

simpleStatement :: P Statement
simpleStatement = do spaces
                     s <- choice [expandAlias >> simpleStatement
                                 ,try $ do r <- redirection
                                           fmap (addRedirection r)
                                                simpleStatement
                                 ,try $ do w <- word NormalContext
                                           fmap (addWord w) simpleStatement
                                 ,return $ Statement [] [] []]
                     spaces
                     return s

expandAlias :: P () -- lookahead
expandAlias = try $ do (aok,as,ip) <- getAliasInfo
                       unless aok $ fail ""
                       a <- many $ noneOf "()|&;<> \t\r\n" -- correct set?
                       case lookup a as of
                         Nothing -> fail ""
                         Just s  -> injectAlias a s as ip

-- |We do some weird (scary) stuff here...  in particular, we inject
-- the control codes /after/ the first character in the stream, which
-- must have been a delimiter of some sort.  This is so that /all/ the
-- sub-expansions that occur here will stack properly and /not/ consume
-- the @Aliases@ token prematurely, thus permanently losing the outermost
-- alias.  I.e.
--   $ alias foo="echo "; alias bar=foo
--   $ foo bar bar
-- If we just inject before @i@ then we end up with "echo echo bar", because
-- the bar expands to "foo\CTRL ..." and then the control codes get eaten
-- up when expanding foo, and that's bad.
injectAlias :: String -> String -> [(String,String)] -> Bool -> P ()
injectAlias a s as ip = do i <- getInput
                           let (h,t) = splitAt 1 i
                               aOn = if isBlank $ last s
                                     then (Ctl (AliasOn True):)
                                     else id -- don't turn /off/
                           setInput $ map Chr s ++
                                      Ctl (IncPos ip):h ++
                                      Ctl (Aliases as):
                                      -- These next two may be gratuitous
                                      aOn t
                           setAliasInfo (True,as\\[(a,s)],False)
                           unless True $
                                do l <- getInput
                                   setInput $ trace ("input: "++show l) l

pipeline :: P Pipeline
pipeline = fmap Pipeline $ statement `sepBy1` pipe

pipe :: P ()
pipe = try $ do char '|'
                notFollowedBy $ fmap Chr $ char '|'

andorlist :: P AndOrList
andorlist = assocL pipeline (try $ (string "||" >> return (:||:))
                               <|> (string "&&" >> return (:&&:)))
                   Singleton

-- |Here is where we need to be careful about parens, at least once we
-- get to the case statements...?
command :: P Command
command = choice [fail "" -- reserved words, ...
                 ,do l <- andorlist <?> "list"
                     t <- commandTerminator <?> "terminator"
                     return $ if t then Asynchronous l else Synchronous l]
          <?> "command"

commandTerminator :: P Bool
commandTerminator = do ip <- insideParens
                       res <- choice [char '&' >> return True
                                     ,char ';' >> return False
                                     ,cnewline >> return False
                                     ,eof      >> return False
                                     ,do unless ip $ fail ""
                                         lookAhead $ char ')'
                                         return False
                                     ] <?> "terminator"
                       newlines
                       return res

newlines :: P ()
newlines = (try (many cnewline) <|> return [()]) >> return ()

-- |Parse a single word.  We need to take a @String@ input so that
-- we can conditionally end on certain delimiters, e.g. @}@.
-- Note that #()|&<>; are in fact all allowed inside ${A:- }, so
-- we'll need to take them all as inputs.
word :: WordContext -> P Word
word context = do spaces
                  ip <- insideParens -- ')' below was '('; only mattered in {}
                  let del = (if ip then (')':) else id) $ delimiters context
                  xs <- word' del <:> many (word' $ del\\"#")
                  case fromLiteral $ GenWord $ concat xs of
                    Just xs' -> return $ LitWord xs'
                    Nothing  -> return $ GenWord $ concat xs
    where word' :: String -> P [Lexeme]
          word' s = choice [do char '\\'
                               try (newline >> return [])  <|>
                                   do c <- anyChar
                                      return [Quote '\\',ql c]
                           ,do char '"'
                               w <- dqWord
                               char '"'
                               return $ Quote '"':w++[Quote '"']
                           ,do char '\''
                               w <- many $ noneOf "\'"
                               char '\''
                               return $ Quote '\'':map ql w++[Quote '\'']
                           ,one expansion
                           ,do c <- noneOf s
                               return [Literal c]
                           ] <?> "word"

dqWord :: P [Lexeme]
dqWord = many $ choice [do char '\\'
                           choice [ql `fmap` oneOf "\\$`\""
                                  ,return $ ql '\\'
                                  ]
                       ,Quoted `fmap` expansion
                       ,ql `fmap` noneOf "\""]

isName :: String -> Bool
isName = const True -- undefined
fatal :: Monad m => String -> m a
fatal = fail -- maybe we can distinguish fatal errors by
-- whether the parser went to EOF or not...?

expansion :: P Lexeme
expansion =
    choice [do char '$'
               choice [try $ do n <- name
                                return $ Expand $ SimpleExpansion n
                      ,do char '{'
                          choice [do char '#'
                                     n <- many $ noneOf "}"
                                     char '}'
                                     if isName n
                                       then return $ Expand $ LengthExpansion n
                                       else fatal $
                                                "${#"++n++"}: bad substitution"
                                 ,try $ do n <- many $ noneOf ":-=?+"
                                           -- check isName again...
                                           c <- zeroOne $ char ':'
                                           op <- oneOf "-=?+"
                                           rest <- word ParameterContext
                                           char '}'
                                           return $ Expand $
                                             FancyExpansion n op (not $ null c)
                                                            rest
                                 ,do n <- many $ noneOf "}"
                                     char '}'
                                     if isName n
                                       then return $ Expand $ SimpleExpansion n
                                       else fatal $ "${"++n++
                                                      "}: bad substitution"]
                      ,do char '('
                          openParen
                          l <- choice [do char '('
                                          a <- arithmetic -- use parenDepth?
                                          return $ Expand a
                                      ,do c <- many $ command
                                          char ')'
                                          return $ Expand $ CommandSub c]
                          closeParen
                          return l
                      ,return $ Literal '$'
                      ]
           ,do char '`'
               s <- many $ escape "`$\\" <|> noneOf "`"
               char '`'
               undefined -- run the parser all over again here! (aliases!)
           ]

arithmetic = undefined

{- TEST:
$ alias bar=foo
$ alias foo='echo $(bar)'
$ foo
dash: foo: not found
-}

escape :: String -> P Char
escape s = char '\\' >> (oneOf s <|> return '\\')

name :: P String
name = count 1 (oneOf "@*#?-$!" <|> digit) <|>
       alphaUnder <:> many alphaUnderNum
         <?> "name"

assignment :: P Assignment
assignment = do spaces
                var <- name
                char '='
                val <- word NormalContext
                return $ var := val
             <?> "assignment"

redirection :: P Redir
redirection = try (do spaces
                      d <- many digit
                      o <- redirOperator
                      spaces
                      t <- word NormalContext
                      mkRedir o (if null d then Nothing else Just $ read d) t)
                <?> "redirection"

commands :: P [Command]
commands = newlines >> many (newlines >> command) >>= (\s -> eof >> return s)

--parse :: [(String,String)] -> String -> Either String [Command]
parse as s = case runParser commands (startState as) "" (map Chr s) of
               Left err -> Left $ err
               Right cs -> Right cs
