-- |Here we define the interface to 'Parsec', resulting in a
-- 'GenParser' type that behaves much like a stateful 'CharParser',
-- but with the added abstraction of dealing with aliases.

module Language.Sh.Parser.Parsec where

import Text.ParserCombinators.Parsec ( GenParser, getState, setState,
                                       tokenPrim, count, (<|>), (<?>),
                                       skipMany, many,
                                       getInput, setInput )
import Text.ParserCombinators.Parsec.Pos ( updatePosChar )
import Data.Char ( isUpper, isLower, isAlpha, isAlphaNum,
                   isDigit, isHexDigit, isOctDigit )
import Data.Monoid ( Monoid, mappend )
import Control.Monad ( unless )
import Debug.Trace ( trace )

-- |Generalized @Char@.
data MChar = Ctl Control | Chr Char

instance Show MChar where
    show (Ctl (AliasOn b)) = "AliasOn "++show b
    show (Ctl (Aliases s)) = "Aliases "++show s
    show (Ctl (IncPos b))  = "IncPos "++show b
    show (Chr c) = show c

-- |We need to intersperse control codes with the @Char@s.  These
-- will have monadic actions, but will not affect the 'uncons'.
data Control = AliasOn Bool
             | Aliases [(String,String)]
             | IncPos Bool -- ^turn on/off SourcePos counting.

-- instance Show Control where show _ = ""

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
oneOf cs = satisfy' ("oneOf: "++show cs) (\c -> elem c cs)

noneOf :: [Char] -> P Char
noneOf cs = satisfy' ("noneOf: "++show cs) (\c -> not (elem c cs))

spaces :: P ()
spaces = skipMany space        <?> "white space"

space :: P Char
space = satisfy' ("space") isBlank        <?> "space"

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
alphaNum = satisfy' "alphaNum" isAlphaNum    <?> "letter or digit"

alphaUnder :: P Char
alphaUnder = satisfy' "alphaUnder" (\c -> isAlpha c || c=='_') <?> "letter or underscore"

alphaUnderNum :: P Char
alphaUnderNum = satisfy' "alphaUnderNum" (\c -> isAlphaNum c || c=='_')
                <?> "letter, number, or underscore"

letter :: P Char
letter = satisfy' "alpha" isAlpha       <?> "letter"

digit :: P Char
digit = satisfy' "digit" isDigit       <?> "digit"

hexDigit :: P Char
hexDigit = satisfy' "hexDigit" isHexDigit    <?> "hexadecimal digit"

octDigit :: P Char
octDigit = satisfy' "octDigit" isOctDigit    <?> "octal digit"

char :: Char -> P Char
char c = satisfy' ("char: "++show c) (==c)  <?> show [c]

anyChar :: P Char
anyChar = satisfy' "anyChar" (const True)

satisfy' :: String -> (Char -> Bool) -> P Char
-- satisfy' m f = satisfy'' True $ trace m f
satisfy' _ = satisfy'' False

-- |This is where all the real work is done...  we just make sure
-- to always call everything in terms of @satisfy@ now.
-- This seems to be a bit broken... I think we need to read
-- the @Ctl@ tokens immediately along with anything else, so that
-- @Consumed@ will be accurate...

-- The other option, I guess, would be to use a type
--   @data MChar = MChar [Control] Char@
-- and then just stack the control codes on either the space or else
-- the next eligible letter.

satisfy = satisfy'' False

-- This is for debugging...
satisfy'' :: Bool -> (Char -> Bool) -> P Char
satisfy'' v f = do ip <- incPos `fmap` getState
                   let update = if ip then updatePosChar else const
                   c <- tokenPrim showToken (nextpos update) test
                   unless (isBlank c) $ modify $ \s -> s { aliasOK = False }
                   runCtls v
                   return c
    where showToken (Chr c) = show c
          nextpos u p (Chr c) _ = u p c
          test (Chr c) = if f c then Just c else Nothing

runCtls :: Bool -> P ()
runCtls v = getInput >>= run >>= setInput
    where run [] = return []
          run (Ctl a:xs) = act a >> run xs
          run xs = return xs
          act (AliasOn b)  = modify $ t "AliasOn" b $ \s -> s { aliasOK = b }
          act (Aliases as) = modify $ t "Aliases" as $ \s -> s { aliases = as }
          act (IncPos b)  = modify $ t "IncPos" b $ \s -> s { incPos = b }
          t s x = if v then trace (s++": "++show x) else id

-- From the source, it appears the state gets threaded through <|> correctly.
-- i.e. (setState ... >> fail ...) <|> (return ())
--      -> doesn't change the state (since that's bound up with reading)

aliasOn :: P ()
aliasOn = modify $ \s -> s { aliasOK = True }

string :: String -> P String
string []     = return []
string (c:cs) = do c <- char c
                   fmap (c:) $ string cs -- errors should work correctly...

schar :: Char -> P Char
schar c = spaces >> char c

-- *More general functions

assocL :: P a -> P (b -> a -> b) -> (a -> b) -> P b
assocL p op single = do x <- p
                        rest $ single x
    where rest x = do f <- op
                      y <- p
                      rest (f x y)
                   <|> return x
