\chapter{Expansions module}

Here is where we do some expansions.  Particularly, we just substitute
environment variables and \verb|~|.

\begin{code}

module System.Console.ShSh.Expansions ( expansions ) where

import System.Console.ShSh.Lexer ( Token(..), Operator(..), EString(..),
                                   runLexer )

import Data.Char ( isAlphaNum )
import Data.List ( takeWhile, dropWhile )

import Text.Parsec ( ParsecT, parse, (<|>) )

import System.Console.ShSh.Shell ( Shell, getEnv, tryEnv )

-- |This is the main function.  Every expansion we do only acts on
-- words, so we single them out and move on otherwise.  And we make
-- sure to do them in the correct order.
expansions :: [Token] -> Shell [Token] -- manual threading -> error prone
expansions (Word x:ts) = do t1 <- tildeExpansion $ catLiterals x
                            ts2 <- parameterExpansion t1
                            fmap (ts2++) $ expansions ts
expansions (t:ts) = fmap (t:) $ expansions ts

-- |The first step in expansion is tilde expansion, so we define that
-- here.  Note that there is a discrepancy between bash and sh here, which
-- makes things like Setup.hs more usable: echo prefix=~ tilde expands in
-- bash, though this is non-compliant.  We /could/ add a non-compliance
-- mode if we wanted to...  As it is, parsing for "=" will be tricky (we'll
-- want to look for /unquoted/ '=' in a command name...)
tildeExpansion :: [EString] -> Shell [EString]
tildeExpansion x@(Literal ('~':rest):es)
    | '/' `elem` rest = do let (name,path) = splitAt '/' rest
                           dir <- homedir name
                           return $ Literal (dir++"/"++path):es
    | null es         = homedir name
    | otherwise       = x

homedir :: String -> Shell String
homedir "" = getEnv "HOME"
homedir user = undefined -- for now

parameterExpansion :: [Token] -> Shell [Token]
parameterExpansion (Word xs:ts) = pExpand False xs:parameterExpansion ts
parameterExpansion (t:ts) = t:parameterExpansion ts

pExpand :: [EString] -> Shell [EString]
pExpand = catLiterals `fmap` mapM expandOne

catLiterals :: [EString] -> [EString]
catLiterals (Literal x:Literal y:xs) = catLiterals $ Literal (x++y):xs
catLiterals (Quoted x:Quoted y:xs) = catLiterals $ Quoted (x++y):xs
catLiterals (Quoted x:xs) = Quoted (catLiterals x):catLiterals xs
catLiterals (x:xs) = x:catLiterals xs

-- Bool is whether we're quoted or not...
expandOne :: Bool -> EString -> Shell EString
expandOne _ (Literal s) = return $ Literal s
expandOne _ (Quoted s) = return $ Quoted (pExpand True s)
expandOne q (ParamExp s) = case parse (anExpansion q) "" s of
                             Left  e -> fail e
                             Right x -> undefined -- I had tokens...?
expandOne _ s = s -- Command sub and arithmetic go later..

-- |expandVar takes quoting and the colon into consideration...!
expandVar :: Bool -> Bool -> String -> Shell (Maybe [Token])
expandVar quoted colon var = undefined

removeEOF :: [Token] -> [Token]
removeEOF = mapMaybe f
    where f EOF = Nothing
          f x = Just x

alt :: String -> Maybe [Token] -> ShellT e [Token]
alt _ (Just t) = t
alt s Nothing  = do mts <- runLexer s
                    case mts of
                      Just ts -> return $ removeEOF ts
                      Nothing -> fail "couldn't parse substitution text"

-- |Here is a parser for expansions.  We may need to specify e=Bool so that
-- we can keep track of whether or not we're quoted and behave accordingly...
type ExpansionParser = ParsecT () Char ShellP

literal :: String -> [Token]
literal s = [Word $ Literal s]

maybeColon :: String -> ExpansionParser Bool
maybeColon s = choice [try string (':':s) >> return True
                      ,string s >> return False]

anExpansion :: Bool -> ExpansionParser [Token]
anExpansion q = choice [do char '#' -- get length of var
                           var <- name <|> special
                           eof
                           lift $ fmap (literal . show . length . fromMaybe "") $
                                       getEnv var
                       ,do var <- name <|> special
                           choice [do eof
                                      lift $ alt "" =<< expandVar q False var
                                  ,do c <- maybeColon "-"
                                      word <- many anyChar
                                      lift $ alt word =<< expandVar q c var
                                  ,do c <- maybeColon "="
                                      word <- many anyChar
                                      e <- lift $ expandVar q c var
                                      case e of
                                        Just ts -> return ts
                                        Nothing -> lift $ do setVar var word
                                                             alt word Nothing
                                  ,do c <- maybeColon "?"
                                      msg <- many anyChar
                                      e <- lift $ expandVar q c var
                                      case e of
                                        Just ts -> return ts
                                        Nothing -> fail $ errMsg c var msg
                                  ,do c <- maybeColon "+"
                                      word <- many anyChar
                                      e <- lift $ expandVar q c var
                                      case e of
                                        Just ts -> alt word Nothing
                                        Nothing -> []
                                  ]
                       ]

doExpansion :: String -> Shell [Token]
doExpansion text = undefined -- run the parser!





(~||~) f g a = (f a) || (g a)

se s "" = return s
se s ('#':cs) = return s -- cheap way to do comments...
se s ('~':cs) = do mh <- getEnv "HOME"
                   case mh of
                     Just h  -> se (s++h) cs
                     Nothing -> se (s++"~") cs
se s ('$':cs) = do let var = takeWhile (isAlphaNum ~||~ (=='_')) cs
                       rest = dropWhile (isAlphaNum ~||~ (=='_')) cs
                       (var',rest') = if null var
                                      then (take 1 rest,drop 1 rest)
                                      else (var,rest)
                   v <- getEnv var
                   se (s++v) rest'
se s (c:cs) = se (s++[c]) cs

\end{code}