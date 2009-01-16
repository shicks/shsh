-- |Here we define the interface to 'Parsec', resulting in a
-- 'GenParser' type that behaves much like a stateful 'CharParser',
-- but with the added abstraction of dealing with aliases.

module Language.Sh.Parser.Parsec where

import Text.ParserCombinators.Parsec ( GenParser, getState, setState,
                                       tokenPrim, count, (<|>), (<?>),
                                       skipMany, many, eof,
                                       getInput, setInput )
import Text.ParserCombinators.Parsec.Pos ( updatePosChar )
import Text.ParserCombinators.Parsec.Error ( ParseError, Message(..),
                                             errorMessages, errorPos,
                                             newErrorMessage )
import Data.Char ( isUpper, isLower, isAlpha, isAlphaNum,
                   isDigit, isHexDigit, isOctDigit )
import Data.Monoid ( Monoid, mappend )
import Data.Maybe ( listToMaybe )
import Control.Monad ( unless, when )
import Debug.Trace ( trace )

import Language.Sh.Syntax ( Word )

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
                      , parenDepth :: Int
                      , hereDocs :: [String]
                      , readHereDocs :: [(Word,Bool)] }

type P = GenParser MChar ParserState

startState :: [(String,String)] -> ParserState
startState as = PS True as True 0 [] []

modify :: (ParserState -> ParserState) -> P ()
modify f = setState =<< fmap f getState

getAliasInfo :: P (Bool, [(String,String)], Bool)
getAliasInfo = fmap (\(PS a b c _ _ _) -> (a,b,c)) getState

setAliasInfo :: (Bool, [(String,String)], Bool) -> P ()
setAliasInfo (a,b,c) = modify $ \(PS _ _ _ d h h') -> PS a b c d h h'

insideParens :: P Bool
insideParens = fmap (\s -> parenDepth s > 0) getState

openParen :: P ()
openParen = modify $ \s -> s { parenDepth = parenDepth s+1 }

closeParen :: P ()
closeParen = modify $ \s -> s { parenDepth = parenDepth s-1 }

getParenDepth :: P Int
getParenDepth = fmap parenDepth getState

addHereDoc :: String -> P ()
addHereDoc d = modify $ \s -> s { hereDocs = hereDocs s ++ [d] }

nextHereDoc :: P (Maybe String)
nextHereDoc = fmap (listToMaybe . hereDocs) getState

popHereDoc :: (Word,Bool) -> P ()
popHereDoc (w,b) = modify $ \s -> s { hereDocs = drop 1 $ hereDocs s
                                       , readHereDocs = readHereDocs s ++ [(w,b)] }

nextHDReplacement :: P (Maybe (Word,Bool))
nextHDReplacement = do rhd <- readHereDocs `fmap` getState
                       case rhd of
                         (next:rest) -> do modify $
                                             \s -> s { readHereDocs = rest }
                                           return $ Just next
                         [] -> return Nothing

fatal :: String -> P a
fatal = fail . ('!':)

getFatal :: ParseError -> Maybe String
getFatal e = listToMaybe $ filter (not . null) $ map isFatal $ errorMessages e
    where isFatal (Message ('!':s)) = s
          isFatal _ = ""

unFatal :: ParseError -> ParseError
unFatal e = case getFatal e of
              Just s -> newErrorMessage (Message s) (errorPos e)
              Nothing -> e

-- fatal :: String -> P a
-- fatal err = do modify $ \s -> s { fatalError = True }
--                fail err

-- isFatal :: P Bool
-- isFatal = fmap fatalError getState

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

zeroOne :: P a -> P [a]
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
schar c = do x <- char c
             spaces
             return x

-- *More general functions

assocL :: P a -> P (b -> a -> b) -> (a -> b) -> P b
assocL p op single = do x <- p
                        rest $ single x
    where rest x = do f <- op
                      y <- p
                      rest (f x y)
                   <|> return x

getInput' :: P String
getInput' = do ts <- getInput
               return $ concatMap f ts
    where f (Chr c) = [c]
          f _ = []

tok :: Char -> String
tok c | c `elem` "\n\r" = "newline"
      | otherwise       = [c]

-- |Parse spaces afterwards
token :: P a -> P a
token p = do p' <- p
             spaces
             return p'

unexpectedToken :: P a
unexpectedToken = do s <- getInput'
                     when (null s) $ err '\n'
                     err (head s)
    where err c = fatal $ "syntax error near unexpected token `"++tok c++"'"

putBack :: Char -> P ()
--putBack c = setInput =<< ((Chr c:) `fmap` getInput)
putBack c = do i <- getInput
               setInput $ Chr c:trace ("putting back a "++[c]++": "++show i) i

-- |This version allows a newline/eof without being fatal.
unexpected :: P a
unexpected = (anyChar >>= putBack >> unexpectedToken) <|> fail ""

unexpectedNoEOF :: P a
unexpectedNoEOF = unexpected
