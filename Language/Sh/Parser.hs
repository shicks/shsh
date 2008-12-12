-- |Here we use the stuff defined in the AST and Parsec modules
-- to parse things.

module Language.Sh.Parser ( parse, hereDocsComplete ) where

import Language.Sh.Parser.Internal
import Language.Sh.Parser.Parsec
import Language.Sh.Syntax

import Text.ParserCombinators.Parsec.Error ( ParseError )
import Text.ParserCombinators.Parsec ( choice, manyTill, eof, many1,
                                       skipMany,
                                       (<|>), (<?>), many, try, count,
                                       sepBy1, notFollowedBy, lookAhead,
                                       getInput, setInput, runParser
                                     )
import Control.Monad ( unless, when, liftM2, ap )
import Data.List ( (\\) )
import Data.Char ( isDigit )
import Data.Maybe ( isJust )

import Debug.Trace ( trace )

-- We don't actually really need Parsec3 - could adapt that Parsec2 source...
-- Also, this should maybe be a debug switch...?
-- #ifdef HAVE_PARSEC3
-- #include "Language/Sh/Parser/safemany.h"
-- #endif

data WordContext = NormalContext | ParameterContext | HereEndContext
     deriving ( Enum, Ord, Eq )

delimiters :: WordContext -> String
delimiters NormalContext = "&|;<>()# \t\r\n"
delimiters ParameterContext = "}" -- don't delimit spaces yet
delimiters HereEndContext = "&|;<>()# \t\r\n"

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
                       a <- many $ noneOf "\\\"'()|&;<> \t\r\n" -- correct set?
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
                                   setInput l -- $ trace ("input: "++show l) l

pipeline :: P Pipeline
pipeline = (try $ do spaces
                     char '!'
                     fmap BangPipeline $ statement `sepBy1` pipe
           ) <|> (fmap Pipeline $ statement `sepBy1` pipe)

pipe :: P ()
pipe = try $ do char '|'
                notFollowedBy $ fmap Chr $ char '|'

andorlist :: P AndOrList
andorlist = assocL pipeline (try $ (string "||" >> return (:||:))
                               <|> (string "&&" >> return (:&&:)))
                   Singleton

-- |Here is where we need to be careful about parens, at least once we
-- get to the case statements...?

-- |Also, we can use 'commandTerminator' to substitute heredocs safely because
-- @<<@ are not allowed in non-command arguments to control structures anyway.
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
                                     ,do cnewline
                                         hd <- nextHereDoc
                                         case hd of
                                           Just s -> readHD s
                                           Nothing -> return ()
                                         return False
                                     ,do eof
                                         closeHereDocs -- ?
                                         return False
                                     ,do unless ip $ fail ""
                                         lookAhead $ char ')'
                                         return False
                                     ] <?> "terminator"
                       newlines
                       return res

manyTill' :: P a -> P end -> P ([a],end)
manyTill' p end = scan
    where scan = do e <- end
                    return ([],e)
                 <|> do x <- p
                        (xs,e) <- scan
                        return ((x:xs),e)

-- |Given a delimiter, parses a heredoc and moves the delimiter off the
-- delimiter list and instead replaces the replacement text onto the
-- 'readHereDocs' list.  Note that we want to end with a newline, but it's
-- being read by the "till" parser.  Instead, we use a 'wPutStrLn' in the
-- 'Shell' module, rather than attempting to add the newline back in here.
readHD :: String -> P ()
readHD delim = popHereDoc =<< manyTill' (dqLex "\\$`")
                                (choice [try $ do newline
                                                  string delim
                                                  newline <|> eof
                                                  return True
                                        ,eof >> return False])

dqLex :: String -> P Lexeme -- input: chars to escape with \
dqLex escape = choice [do char '\\'
                          choice [newline >> dqLex escape
                                 ,ql `fmap` oneOf escape
                                 ,return $ ql '\\'
                                 ]
                      ,Quoted `fmap` expansion
                      ,ql `fmap` anyChar
                      ]

-- |Nothing left after command terminator, so turn all the heredocs into
-- empty @False@s.
closeHereDocs :: P ()
closeHereDocs = do hd <- nextHereDoc
                   case hd of
                     Nothing -> return ()
                     Just _  -> popHereDoc ([],False) >> closeHereDocs

-- |How can the many cnewline possibly fail...?  If spaces end in something
-- else... So we should move over to gobbling spaces after words, rather
-- than before...
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
                  fmap concat $ word' del <:> many (word' $ del\\"#")
    where word' :: String -> P Word
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
                           ,do when (context==HereEndContext) $ fail ""
                               one expansion
                           ,do c <- noneOf s
                               return [Literal c]
                           ] <?> "word"

dqWord :: P Word
dqWord = fmap concat $ many $
         choice [do char '\\'
                    choice [newline >> return []
                           ,map ql `fmap` one (oneOf "\\$`\"")
                           ,return $ [ql '\\']
                           ]
                ,map Quoted `fmap` one expansion
                ,map ql `fmap` one (noneOf "\"")
                ]

isName :: String -> Bool
isName s = case parse' [] (only name) s of
             Right _ -> True
             Left _  -> False

-- dqWord :: P Word
-- dqWord = manyTill (dqLex "\\$`\"") (char '"')

-- dqLex :: String -> P Lexeme -- input: chars to escape with \
-- dqLex escape = choice [do char '\\'
--                           choice [newline >> dqLex escape
--                                  ,ql `fmap` oneOf escape
--                                  ,return $ ql '\\'
--                                  ]
--                       ,Quoted `fmap` expansion
--                       ,ql `fmap` anyChar
--                       ]

-- Technically, we're not saving the \ quotes here....... does this matter?

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
                                           char '}' <|> (char ')' >> unexEOF)
                                           return $ Expand $
                                             FancyExpansion n op (not $ null c)
                                                            rest
                                 ,do ip <- insideParens
                                     let p = if ip then (')':) else id
                                     n <- many $ noneOf $ p "}"
                                     char '}' <|> (char ')' >> unexEOF)
                                     if isName n
                                       then return $ Expand $ SimpleExpansion n
                                       else fatal $ "${"++n++
                                                      "}: bad substitution"]
                      ,do char '('
                          openParen
                          l <- choice [do char '('
                                          a <- arithmetic -- use parenDepth?
                                          return $ Expand a
                                      ,do c <- commands
                                          char ')'
                                          return $ Expand $ CommandSub c]
                          closeParen
                          return l
                      ,return $ Literal '$'
                      ]
           ,do char '`'
               s <- many $ escape "`$\\" <|> noneOf "`"
               char '`'
               (_,as,_) <- getAliasInfo -- cf. bash: alias foo='`foo`'
               case parse' as (only commands) s of
                 Left err -> fatal $ "command substitution: "
                                     ++ show (unFatal err)
                 Right cs -> return $ Expand $ CommandSub cs
           ]
    where unexEOF = fatal "unexpected EOF while looking for matching `}'"

arithmetic :: P Expansion
arithmetic = do w <- arithWord =<< getParenDepth
                return $ Arithmetic $ ql '(':w 

arithWord :: Int -> P Word
arithWord d0 = aw -- now we can forget about d0
    where aw = do d <- getParenDepth
                  if d==d0-1
                     then (char ')' >> openParen >> return [])
                              <|> (openParen >> aw')
                     else aw'
          aw' :: P Word
          aw' = choice [do char '\\'
                           choice [newline >> aw'
                                  ,liftM2 (:) (ql `fmap` oneOf "\\$`\"") aw'
                                  ,liftM2 (\c r -> ql '\\':ql c:r) anyChar aw']
                       ,do { ex <- expansion; fmap (Quoted ex:) aw' }
                       ,char '(' >> openParen >> (ql '(':) `fmap` aw'
                       ,char ')' >> closeParen >> (ql ')':) `fmap` aw -- no '
                       ,liftM2 (:) (ql `fmap` anyChar) aw'
                       ] <?> "arithmetic word"

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

basicName :: P String
basicName = alphaUnder <:> many alphaUnderNum <?> "name"

assignment :: P Assignment
assignment = do spaces
                var <- basicName <?> "name"
                char '='
                val <- word NormalContext
                return $ var := val
             <?> "assignment"

redirection :: P Redir
redirection = try (do spaces
                      d <- many digit
                      o <- redirOperator
                      spaces
                      let fd = if null d then Nothing else Just $ read d
                      if o `elem` ["<<","<<-"]
                         then do t <- hereEnd
                                 mkHereDoc o fd t
                         else do t <- word NormalContext
                                 mkRedir o fd t)
                <?> "redirection"

-- |Parse the heredoc delimiter.  Technically this is supposed to be a
-- word, but we don't make certain distinctions that @sh@ does (i.e. @$a@
-- vs @${a}@), so I think we're better off just using a string...
hereEnd :: P String
hereEnd = fromLit `fmap` word HereEndContext
    where fromLit [] = ""
          fromLit (Quote _:xs) = fromLit xs
          fromLit (Quoted q:xs) = fromLit $ q:xs
          fromLit (Literal l:xs) = l:fromLit xs

commands :: P [Command]
commands = do newlines
              c <- many $ newlines >> command -- maybe get rid of newlines?
              expandHereDocs c -- may not be exhaustive...?

-- |This function simply iterates through the commands until it finds
-- unexpanded heredocs.  As long as there's a @readHereDoc@ left in the
-- queue, it substitutes it.
mapRedirsM :: (Monad m,Functor m)
           => (Redir -> m Redir) -> [Command] -> m [Command]
mapRedirsM f cs = mapM e1 cs
    where e1 (Synchronous l) = Synchronous `fmap` e2 l
          e1 (Asynchronous l) = Asynchronous `fmap` e2 l
          e1 (For s ss cs') = For s ss `fmap` mapM e1 cs'
          -- more e1 defs as we expand the grammar
          e2 (Singleton p) = Singleton `fmap` e3 p
          e2 (l :&&: p) = (:&&:) `fmap` e2 l `ap` e3 p
          e2 (l :||: p) = (:||:) `fmap` e2 l `ap` e3 p
          e3 (Pipeline ps) = Pipeline `fmap` mapM e4 ps
          e3 (BangPipeline ps) = BangPipeline `fmap` mapM e4 ps
          e4 (Subshell cs rs) = Subshell `fmap` mapM e1 cs
                                `ap` mapM f rs
          e4 (Statement ws rs as) = Statement `fmap` mapM (mapM e5) ws
                                    `ap` mapM f rs `ap` return as
          e5 (Quoted lexeme) = Quoted `fmap` e5 lexeme
          e5 (Expand xp) = Expand `fmap` e6 xp
          e5 lexeme = return lexeme
          e6 (FancyExpansion s c b w) = FancyExpansion s c b `fmap` mapM e5 w
          e6 (CommandSub cs) = CommandSub `fmap` mapM e1 cs
          e6 (Arithmetic w) = Arithmetic `fmap` mapM e5 w
          e6 expansion = return expansion

-- The order is wrong here, since we could put the Redir's either before or
-- after the Word's...  We'll need to figure something out to deal with that.
-- Easiest would be to just number them or something, and go through one at
-- a time at the end to "de-number" them.

expandHereDocs :: [Command] -> P [Command]
expandHereDocs = mapRedirsM eHD
    where --eHD (i :<< s) = mk i id
          --eHD (i :<<- s) = mk i stripTabs
          eHD r = return r
          stripTabs [] = []
          stripTabs (Literal n:Literal '\t':rest)
              | n `elem` "\n\r" = stripTabs (Literal n:rest)
          stripTabs (x:xs) = x:stripTabs xs
--           mk i f = do mwb <- nextHDReplacement
--                       case mwb of -- do we need the Nothing case? (impossible?)
--                         Just (w,b) -> return $ Heredoc i b (f w)
--                         Nothing    -> return $ Heredoc i False []

-- here's a smart use of the Monad class...! :-)
hereDocsComplete :: [Command] -> Bool
hereDocsComplete = isJust . mapRedirsM complete
    where complete r = case r of
                         (_:<<_)  -> Nothing
                         (_:<<-_) -> Nothing
                         Heredoc _ False _ -> Nothing
                         r        -> Just r

-- |Ensures there's an 'eof' after whatever we parse.
only :: P a -> P a
only p = p >>= (\a -> eof >> return a)

-- |We need to run the parser occasionally from within, so we provide
-- a simpler interface that does all the mapping, etc, for us.
parse' :: [(String,String)] -> P a -> String -> Either ParseError a
parse' as p s = runParser p (startState as) "" (map Chr s)

-- |This is the main export here.  We take a list of aliases for the
-- environment and a @String@ to parse.  The return type is @Right
-- [Command]@ if parsing succeeded and @Left (String,Bool)@ upon
-- failure.  The @Bool@ is @True@ when the error was fatal/unrecoverable.
parse :: [(String,String)]  -- ^list of alises to expand
      -> String             -- ^input string
      -> Either (String,Bool) [Command]
parse as s = case parse' as (only commands) s of
               Left err -> case getFatal err of
                             Just f  -> Left (f,True)
                             Nothing -> Left (show err,False)
               Right cs -> Right cs
