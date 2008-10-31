\chapter{Expansions module}

Here is where we do the various expansions.

\begin{code}

module System.Console.ShSh.Expansions ( expansions ) where

import System.Console.ShSh.Lexer ( Token(..), Operator(..), EString(..),
                                   runLexer )

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

-- |This is the main function.  Every expansion we do only acts on
-- words, so we single them out and move on otherwise.  And we make
-- sure to do them in the correct order.
expansions :: [Token] -> Shell [Token] -- manual threading -> error prone
expansions (Word x:ts) = do t1 <- tildeExpansion $ catLiterals x
                            ts2 <- parameterExpansion t1
                            fmap (Word ts2:) $ expansions ts
expansions (t:ts) = fmap (t:) $ expansions ts
expansions [] = return []

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

catLiterals :: [EString] -> [EString]
catLiterals (Literal x:Literal y:xs) = catLiterals $ Literal (x++y):xs
catLiterals (Quoted (Literal x):Quoted (Literal y):xs)
    = catLiterals $ Quoted (Literal (x++y)):xs
catLiterals (x:xs) = x:catLiterals xs
catLiterals [] = []

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



\end{code}