\chapter{Parse.Parsec module}

Here we define the interface to Parsec, resulting in a
GenParser type that behaves much like a stateful CharParser,
but with the added abstraction of dealing with aliases.

\begin{code}
{-# LANGUAGE CPP #-}

module System.Console.ShSh.Parse.Parsec where

import Text.ParserCombinators.Parsec ( GenParser, getState, setState,
                                       tokenPrim, count, (<|>), (<?>),
                                       skipMany, many )
import Text.ParserCombinators.Parsec.Pos ( updatePosChar )
import Data.Char ( isUpper, isLower, isAlpha, isAlphaNum,
                   isDigit, isHexDigit, isOctDigit )
import Data.Monoid ( Monoid, mappend )
import Control.Monad ( unless )
import Debug.Trace ( trace )

-- |Generalized @Char@.
type MChar = Either Control Char

-- |We need to intersperse control codes with the @Char@s.  These
-- will have monadic actions, but will not affect the 'uncons'.
data Control = AliasOn Bool
             | Aliases [(String,String)]
             | IncPos Bool -- ^turn on/off SourcePos counting.

instance Show Control where show _ = ""

-- |Much-reduced state to keep track of.
data ParserState = PS { aliasOK :: Bool
                      , aliases :: [(String,String)]
                      , incPos :: Bool
                      , parenDepth :: Int }

type P = GenParser MChar ParserState

startState :: [(String,String)] -> ParserState
startState as = PS True as True 0

modify :: (ParserState -> ParserState) -> P ()
modify f = setState =<< fmap f getState

getAliasInfo :: P (Bool, [(String,String)], Bool)
getAliasInfo = fmap (\(PS a b c _) -> (a,b,c)) getState

setAliasInfo :: (Bool, [(String,String)], Bool) -> P ()
setAliasInfo (a,b,c) = modify $ \(PS _ _ _ d) -> PS a b c d

insideParens :: P Bool
insideParens = fmap (\s -> parenDepth s > 0) getState

openParen :: P ()
openParen = modify $ \(PS a b c d) -> PS a b c (d+1)

closeParen :: P ()
closeParen = modify $ \(PS a b c d) -> PS a b c (d-1)

-- |This is a useful combinator.
infixl 3 <++>, <:>
(<++>) :: Monoid w => GenParser i s w -> GenParser i s w -> GenParser i s w
a <++> b = do w <- a
              w' <- b
              return $ w `mappend` w'

(<:>) :: GenParser i s a -> GenParser i s [a] -> GenParser i s [a]
a <:> b = do w <- a
             w' <- b
             return $ w:w'

tr :: Show a => String -> P a -> P a
tr s p = do a <- p
            return $ trace (s++": "++show a) a
--catMany :: Show a => P [a] -> P [a]
--catMany = fmap concat . many . tr "catMany"

-- * Here we re-implement much of Text.Parsec.Char
oneOf :: [Char] -> P Char
oneOf cs = satisfy (\c -> elem c cs)

noneOf :: [Char] -> P Char
noneOf cs = satisfy (\c -> not (elem c cs))

spaces :: P ()
spaces = skipMany space        <?> "white space"

space :: P Char
space = satisfy isBlank        <?> "space"

space_ :: P ()
space_ = space >> return ()

isBlank :: Char -> Bool
isBlank = (`elem` " \t")

one :: P a -> P [a]
one = sequence . replicate 1

zeroOne :: P Char -> P [Char]
zeroOne p = one p <|> return []

newline :: P () -- how does this affect SourcePos?
newline = (count 1 (char '\n') >> zeroOne (char '\r') >> return ()) <|>
          (count 1 (char '\r') >> zeroOne (char '\n') >> return ())
          <?> "newline"

tab :: P Char
tab = char '\t'             <?> "tab"

upper :: P Char
upper = satisfy isUpper       <?> "uppercase letter"

lower :: P Char
lower = satisfy isLower       <?> "lowercase letter"

alphaNum :: P Char
alphaNum = satisfy isAlphaNum    <?> "letter or digit"

alphaUnder :: P Char
alphaUnder = satisfy (\c -> isAlpha c || c=='_') <?> "letter or underscore"

alphaUnderNum :: P Char
alphaUnderNum = satisfy (\c -> isAlphaNum c || c=='_')
                <?> "letter, number, or underscore"

letter :: P Char
letter = satisfy isAlpha       <?> "letter"

digit :: P Char
digit = satisfy isDigit       <?> "digit"

hexDigit :: P Char
hexDigit = satisfy isHexDigit    <?> "hexadecimal digit"

octDigit :: P Char
octDigit = satisfy isOctDigit    <?> "octal digit"

char :: Char -> P Char
char c = satisfy (==c)  <?> show [c]

anyChar :: P Char
anyChar = satisfy (const True)

-- |This is where all the real work is done...  we just make sure
-- to always call everything in terms of @satisfy@ now.
satisfy :: (Char -> Bool) -> P Char
satisfy f = do ip <- incPos `fmap` getState
               let update = if ip then updatePosChar else const
               r <- tokenPrim (\t -> case t of
                                       Left _  -> ""
                                       Right c -> show [c])
                              (\pos t _ts -> case t of
                                               Left _  -> pos
                                               Right c -> update pos c)
                              (\t -> case t of
                                       Left c  -> Just $ Left c
                                       Right c -> if f c then Just $ Right c
                                                         else Nothing)
               case r of
                 Left c -> act c >> satisfy f
                 Right c -> do unless (isBlank c) $ act $ AliasOn False
                               return c
             where act (AliasOn b)  = modify $ \s -> s { aliasOK = b }
                   act (Aliases as) = modify $ \s -> s { aliases = as }
                   act (IncPos b)  = modify $ \s -> s { incPos = b }

aliasOn :: P ()
aliasOn = modify $ \s -> s { aliasOK = True }

string :: String -> P String
string []     = return []
string (c:cs) = do c <- char c
                   fmap (c:) $ string cs -- errors should work correctly...

schar :: Char -> P Char
schar c = spaces >> char c




\end{code}