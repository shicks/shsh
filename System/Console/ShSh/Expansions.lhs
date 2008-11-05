\chapter{Expansions module}

Here is where we do the various expansions.

\begin{code}

module System.Console.ShSh.Expansions ( expansions ) where

import System.Console.ShSh.Operator ( Operator(..) )
import System.Console.ShSh.Lexer ( Lexeme(..), Token(..), runLexer )
import System.Console.ShSh.Parser ( parse, reservedWords )

import Control.Monad.Trans ( lift, liftIO )

import Data.Char ( isAlphaNum )
import Data.List ( takeWhile, dropWhile )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Monoid, mappend )

import Text.Parsec ( ParsecT, runParserT, (<|>),
                     choice, try, many, many1, eof,
                     string, char, anyChar, letter, alphaNum, oneOf, digit )

import System.Console.ShSh.Foreign.Pwd ( getHomeDir )
import System.Console.ShSh.Shell ( Shell, setEnv, getEnv )
import System.Console.ShSh.ShellError ( throw )

mapWords :: ([Lexeme] -> [Lexeme]) -> Token -> Token
mapWords f (Word x) = Word $ f x
mapWords _ x = x

removeQuotes :: [Token] -> [Token]
--removeQuotes =  mapExpr $ map $ mapWords $ f
removeQuotes = map $ mapWords f
    where f (Quote _:x) = f x
          f (Literal c:x) = Literal c:f x
          f (Quoted (Literal c):x) = Literal c:f x
          f [] = []
          f l = error $ "Cannot removeQuotes on "++show l

-- This is TOO LATE!  Alias expansion needs to occur DURING
-- LEXING...  i.e.
--   $ alias foo='echo 1; sort'
--   $ df | foo | cat
--   vs.
--   $ df | (foo) | cat
-- We see that the ; in the foo is taken OUTSIDE of the pipeline...!
-- We might go so far as to do this during LEXING...  Likewise with
-- tilde expansion...?

-- But there was another issue, too... something about late-binding of aliases...
--   $ unalias foo
--   $ alias foo=echo && alias foo
--   foo='echo'
--   $ unalias foo
--   $ alias foo=echo && foo bar
--   dash: foo: not found
--   $ unalias foo
--   $ alias foo=echo; foo bar
--   dash: foo: not found
-- Same thing happens in bash......  so we're fine there too...?

fromLiteral :: [Lexeme] -> Maybe String
fromLiteral = mapM $ \x -> do {Literal c <- return x; return c}

literal :: String -> [Lexeme]
literal = map Literal

dropLast :: [a] -> [a]
dropLast xs = take (length xs - 1) xs

expansions :: [Token] -> Shell [Token]
expansions ts = return $ removeQuotes ts -- for now, this is all we'll do
                -- After this, everything should be literal...

{-
-- |This is the main function.  Every expansion we do only acts on
-- words, so we single them out and move on otherwise.  And we make
-- sure to do them in the correct order.
expansions :: [Token] -> Shell [Token]
expansions t = do t' <- expansions' t
                  return $ clean $ retokenize t' -- retokenze at the end!

expansions' :: [Token] -> Shell [Token] -- manual threading -> error prone
expansions' (Word x:ts) = do t1 <- tildeExpansion $ catLiterals x
                             ts2 <- parameterExpansion t1
                             fmap (Word ts2:) $ expansions ts
expansions' (t:ts) = fmap (t:) $ expansions ts
expansions' [] = return []

splitAtChar :: Eq a => a -> [a] -> ([a],[a])
splitAtChar c' (c:cs) | c==c'     = ([],cs)
                      | otherwise = let (f,s) = splitAtChar c' cs
                                    in  (c:f,s)
splitAtChar _ []  = ([],[])

-- |The first step in expansion is tilde expansion, so we define that
-- here.  Note that there is a discrepancy between bash and sh here, which
-- makes things like Setup.hs more usable: echo prefix=~ tilde expands in
-- bash, though this is non-compliant.  We /could/ add a non-compliance
-- mode if we wanted to...  As it is, parsing for "=" will be tricky (we'll
-- want to look for /unquoted/ '=' in a command name...)
tildeExpansion :: [EString] -> Shell [EString]
tildeExpansion x@(Literal ('~':rest):es)
    | '/' `elem` rest = do let (user,path) = splitAtChar '/' rest
                           dir <- homedir user
                           return $ Literal (dir++"/"++path):es
    | null es         = do dir <- homedir rest
                           return $ [Literal dir]
tildeExpansion x = return x -- everybody else...

homedir :: String -> Shell String
homedir "" = getEnv "HOME"
--homedir user = return $ "/fakehome/"++user -- for now
homedir user = liftIO $ fromMaybe ("~"++user) `fmap` getHomeDir user

parameterExpansion :: [EString] -> Shell [EString]
parameterExpansion (x:ts) = do this <- expandOne False x
                               fmap (this++) $ parameterExpansion ts
parameterExpansion [] = return []

-- |@catLiterals@ simply combines any neighboring literal expressions.
catLiterals :: [EString] -> [EString]
catLiterals (Literal x:Literal y:xs) = catLiterals $ Literal (x++y):xs
catLiterals (Quoted (Literal x):Quoted (Literal y):xs)
    = catLiterals $ Quoted (Literal (x++y)):xs
catLiterals (x:xs) = x:catLiterals xs
catLiterals [] = []

-- |@removeNulls@ removes any empty literals from the list.
removeNulls :: [EString] -> [EString]
removeNulls (Literal "":xs) = removeNulls xs
removeNulls (Quoted (Literal ""):xs) = removeNulls xs
removeNulls (x:xs) = x:removeNulls xs
removeNulls [] = []

-- |Simple one-deep quoting...
mapQuote :: Bool -> [EString] -> [EString]
mapQuote True = map Quoted
mapQuote False = id

-- Bool is whether we're quoted or not... - no depth any more...
expandOne :: Bool -> EString -> Shell [EString]
expandOne q (Literal s) = return $ mapQuote q [Literal s]
expandOne _ (Quoted s) = expandOne True s -- only effect of quotes now
expandOne q (ParamExp s) = do result <- runParserT (parseExp q) () "" s
                              case result of 
                                Left  e -> fail $ show e -- doesn't happen?
                                Right x -> return x
expandOne q s = return $ mapQuote q [s] -- Command sub and arithmetic go later..

-- |expandVar takes quoting and the colon into consideration...!
-- It returns @Nothing@ when the variable is undefined, or when it's
-- null and the colon is set.

-- What difference does this make?  (does what make?)
-- This doesn't do much of anything special just yet...
expandVar :: Bool -> Bool -> String -> Shell (Maybe [EString])
expandVar quoted colon var = do mval <- getEnv var
                                case mval of
                                  Nothing -> return Nothing
                                  Just "" -> if colon
                                             then return Nothing
                                             else return $ Just [q $ Literal ""]
                                  Just x  -> return $ Just [q $ Literal x]
    where q = if quoted then Quoted else id

-- |Here is a parser for expansions.  We may need to specify e=Bool so that
-- we can keep track of whether or not we're quoted and behave accordingly...
type ExpansionParser = ParsecT String () Shell

maybeColon :: String -> ExpansionParser Bool
maybeColon s = choice [try $ string (':':s) >> return True
                      ,string s >> return False]

-- |These are copied from Lexer, but we're currently using a different
-- type of parser.  We can unify at some point.
name :: Monad m => ParsecT String u m String
name = many1 (letter <|> char '_') +++ many (alphaNum <|> char '_')

special :: Monad m => ParsecT String u m String
special = do c <- oneOf "*@#?-$!" <|> digit -- positional parameters too...
             return [c]

infixl 3 +++
(+++) :: (Monad m,Monoid w) => ParsecT s u m w -> ParsecT s u m w 
      -> ParsecT s u m w
a +++ b = do w <- a
             w' <- b
             return $ w `mappend` w'

-- |This is where the parsing actually takes place.  We still have a small
-- problem: the substitution texts (i.e. "word") are regarded as /literals/,
-- but real shell allows actual 'Token's.  I see several possible solutions:
--  (1) Move this parsing into the lexer.  This is messy.
--  (2) Make the lexer give us back whole @Token@s here.  This will likely
--      require us to do some silliness breaking the tokens back into literals
--      and then parsing on the literals.
--  (3) Re-lex the literals.  This would absolutely require re-escaping any
--      dangerous characters, and would work easiest if we added an artificial
--      constructor to 'EString' to store arbitrary @[Token]@.  We'd then
--      run a function on the output to break the @Token@s out of the
--      @EString@s, before running the command substitution.
-- This may all be a moot point.  Apparently I've been removing my
-- quotes (single, double, and escapes) too early anyway, and I don't
-- understand the distinction between tokenization and field
-- separation...  Apparently we need to do these in /parallel/ anyway!
-- eek.  Also, [[:blank:]] = [ \t] apparently.  And we should match
-- newline with \n, \r, \n\r, and \r\n for maximum portability.
-- Experiments show that syntax errors come before substitutions, but
-- that the substitutions are non-atomic - a future subs error doesn't
-- wipe out an earlier ${A=} sub.
parseExp :: Bool -> ExpansionParser [EString]
parseExp q = choice [do char '#' -- get length of var
                        var <- name <|> special
                        eof
                        lift $ fmap (literal . show . length .
                                     fromMaybe "") $ getEnv var
                    ,do var <- name <|> special
                        choice [do eof
                                   lift $ (alt "") `fmap` expandVar q False var
                               ,do c <- maybeColon "-"
                                   word <- many anyChar
                                   lift $ (alt word) `fmap` expandVar q c var
                               ,do c <- maybeColon "="
                                   word <- many anyChar
                                   e <- lift $ expandVar q c var
                                   case e of
                                     Just ts -> return ts
                                     Nothing -> lift $ do setEnv var word
                                                          return $ literal word
                               ,do c <- maybeColon "?"
                                   msg <- many anyChar
                                   e <- lift $ expandVar q c var
                                   case e of
                                     Just ts -> return ts
                                     Nothing -> errMsg c var msg
                               ,do c <- maybeColon "+"
                                   word <- many anyChar
                                   e <- lift $ expandVar q c var
                                   case e of
                                     Just ts -> return $ literal word
                                     Nothing -> return $ []
                               ]
                    ]
    where literal :: String -> [EString]
          literal s = if q then [Quoted $ Literal s] else [Literal s]
          alt :: String -> Maybe [EString] -> [EString]
          alt = fromMaybe . literal
          errMsg :: Bool -> String -> String -> ExpansionParser a
          errMsg c var [] = throw $ var++": parameter "++nullor++"not set"
              where nullor = if c then "null or " else ""
          errMsg _ _ s = throw s


-- |Various expansions have messed up our tokenization by inserting spaces
-- here and there.  We now need to retokenize by scanning all the un@Quoted@
-- @Literal@s for whitespace and breaking them apart.
retokenize :: [Token] -> [Token]
retokenize = concat . map retok
    where retok :: Token -> [Token]
          retok (Word xs) = let (front,back) = split xs
                            in (Word front):if null back
                                               then []
                                               else retok $ Word back
          retok x = [x]
          split :: [EString] -> ([EString],[EString])
          split (Literal x:xs) | ' ' `elem` x
                                   = let (front,back) = splitAtChar ' ' x
                                         back' = dropWhile (==' ') back
                                     in ([Literal front],Literal back':xs)
          split (x:xs) = let (front,back) = split xs
                         in (x:front,back)
          split [] = ([],[])

-- |We also need functions to clean up messes left by these sorts of
-- retokenize operations.  @clean@ goes through and removes empty literals
-- and concatenates neighboring literals.  We use 'catLiterals' and
-- 'removeNulls' from above, but generalize to @[Token]@ instead.
clean :: [Token] -> [Token]
clean = map cl
    where cl (Word x) = Word $ catLiterals $ removeNulls x
          cl x = x

-}

\end{code}