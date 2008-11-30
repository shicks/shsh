-- |This is the expansion module.  It provides an interface for a monad
-- in which expansions can happen, and then defines the expansions.

module Language.Sh.Expansion ( ExpansionFunctions(..),
                               noGlobExpansion,
                               expand, expandWord ) where

import Control.Monad ( forM_ )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.Trans ( lift )
import Data.Char ( isAlphaNum )
import Data.List ( takeWhile, dropWhile, groupBy )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Monoid, mappend, mempty )

import Language.Sh.Compat ( on )
import Language.Sh.Syntax ( Command, Word, Lexeme(..),
                            Expansion(..), Glob, GlobChar(..) )

import Language.Sh.Arithmetic ( runMathParser )

data ExpansionFunctions m = ExpansionFunctions {
      getAllEnv :: m [(String,String)],
      setEnv :: String -> String -> m (),
      homeDir :: String -> m (Maybe String), -- default: return . Just
      expandGlob :: Glob -> m [String],
      commandSub :: [Command] -> m String
    }

-- |This is a private monad we use to pass around the functions...
type Exp m = ReaderT (ExpansionFunctions m) m

-- |And here's the easiest way to use them...
get' :: Monad m => Exp m [(String,String)]
get' = asks getAllEnv >>= lift
get :: Monad m => String -> Exp m (Maybe String)
get s = lookup s `fmap` get'
set :: Monad m => String -> String -> Exp m ()
set s v = use2 setEnv s v
home :: Monad m => String -> Exp m (Maybe String)
home u = use homeDir u
glob :: Monad m => Glob -> Exp m [String]
glob g = use expandGlob g
run :: Monad m => [Command] -> Exp m String
run cs = use commandSub cs

-- |Helper functions to define these accessors
use :: Monad m => (ExpansionFunctions m -> a -> m b) -> a -> Exp m b
use f a = asks f >>= lift . ($a)
use2 :: Monad m => (ExpansionFunctions m -> a -> b -> m c) -> a -> b -> Exp m c
use2 f a b = asks f >>= lift . ($b) . ($a)


-- |This is a default function that basically treats globs as literals.
noGlobExpansion :: (Monad m,Functor m) => Glob -> m [String]
noGlobExpansion x = do s <- nge x
                       return [s]
    where nge [] = return []
          nge (Lit c:gs) = (c:) `fmap` nge gs
          nge (One:gs) = ('?':) `fmap` nge gs
          nge (Many:gs) = ('*':) `fmap` nge gs
          nge (OneOf cs:gs) = (\s->'[':cs++']':s) `fmap` nge gs
          nge (NoneOf cs:gs) = (\s->"[^"++cs++']':s) `fmap` nge gs

expand :: (Monad m,Functor m) => ExpansionFunctions m -> [Word] -> m [String]
expand fs ws = runReaderT (expandE ws) fs

expandE :: (Monad m,Functor m) => [Word] -> Exp m [String]
expandE ws = fmap (map removeQuotes) $ splitFields =<< mapM expand' ws

expandWord :: (Monad m,Functor m) => ExpansionFunctions m -> Word -> m String
expandWord fs w = runReaderT (expandWordE w) fs

expandWordE :: (Monad m,Functor m) => Word -> Exp m String
expandWordE w = fmap removeQuotes $ expand' w

expand' :: (Monad m,Functor m) => Word -> Exp m Word
expand' = expandParams <=< expandTilde

f <=< g = \a -> g a >>= f
infixr 1 <=<

-- |First step: tilde expansion.
expandTilde :: (Monad m,Functor m) => Word -> Exp m Word
expandTilde w = let (lit,rest) = span isLiteral w
                in case (fromLit lit) of
                     '~':s -> exp s rest
                     _     -> return w
    where exp s r | '/' `elem` s = do let (user,path) = break (=='/') s
                                      dir <- homedir user
                                      return $ map Literal (dir++"/"++path) ++ r
          exp s [] = do dir <- homedir s
                        return $ map Literal dir
          exp s r = return $ map Literal s ++ r
          isLiteral (Literal _) = True
          isLiteral _ = False
          fromLit [] = []
          fromLit (Literal c:xs) = c:fromLit xs -- don't need other case

homedir :: (Monad m,Functor m) => String -> Exp m String
homedir "" = fromMaybe ("~") `fmap` get "HOME"
homedir user = fromMaybe ("~"++user) `fmap` home user

quote :: Bool -> Word -> Word
quote True = map Quoted
quote False = id

quoteLiteral :: Bool -> String -> Word
quoteLiteral q = quote q . map Literal

-- |Parameter expansion
expandParams :: (Monad m,Functor m) => Word -> Exp m Word
expandParams = expandWith e
    where e q (SimpleExpansion n) = getEnvQ q n
          e q (LengthExpansion n) = do v <- getEnvQ q n
                                       return $ quoteLiteral q $
                                              show $ length v
          e q (FancyExpansion n o c w)
              = do v <- getEnvQC q c n
                   case o of
                     '-' -> return $ fromMaybe w v
                     '=' -> case v of
                              Nothing -> do setEnvW n w
                                            return w
                              Just v' -> return v'
                     '?' -> case v of -- if w then use that as message...
                              Nothing -> fail $ n++": undefined or null"
                              Just v' -> return v'
                     '+' -> return $ maybe mempty (const w) v
          e q (CommandSub cs) = (quoteLiteral q . removeNewlines) `fmap` run cs
          e q (Arithmetic w) = fmap (quoteLiteral q) $
                                 arithExpand =<< expandWordE w
          --e _ x = fail $ "Expansion "++show x++" not yet implemented"
          removeNewlines = reverse . dropWhile (`elem`"\r\n") . reverse

-- crap - need to fully expand all letters...?

arithExpand :: Monad m => String -> Exp m String
arithExpand s = fmap show $ doMath s

-- This doesn't work with ++ and -- operators.....?
-- there's no postfix in parsec2... (but we could do it by hand in term parser)
-- this is a bit broken maybe...
-- plan: first clean up any unexpected tokens (\, #, etc) after
-- an initial expansion run.
-- maybe do real passes of group-words, expand, repeat...?
-- what to do with variables...?
-- dash has a much simpler arithexp than bash..  in particular,
-- a=5+10
-- echo $((++a))
-- echo $((a))   -- even this fails in dash...
-- echo $((2*$a*4)) -- 50 in both...  $-expansion comes first
-- echo $((2*a*4)) -- 120 in bash... so this expansion is LATER
-- b=c
-- c=10
-- echo $((++b))
-- ------> dash doesn't even support ++ at all...!

{-
expandLetters :: String -> Exp m String
expandLetters [] = return []
expandLetters cs | not $ null name = do e <- fromMaybe "" `fmap` getEnv name
                                        return $ expandLetters $
                                               name:expandLetters rest
                 | otherwise = do let (a,b) = break endTok cs
                                      (a',b') = span endTok cs
                                  rest <- expandLetters b'
                                  return $ a'++a''++rest
    where (name,rest) = spanName cs
          spanName (x:xs) | isAlpha x || x=='_' = let (c,rest)=span isANU xs
                                                  in (x:c,rest)
          spanName xs = ([],xs) -- not a name
          isANU x = isAlphaNum x || x=='_'
          endTok = (`elem` " \t\r\n()+-*/%^|&<>=!~?:") -- lots of operators...
-}

-- one possibility: perform all expansions by encasing first in parens?
-- BUT... a=\(; b=\); echo $(($a 5+10$b*2)) works in both shells...

-- |Helper functions...
setEnvW :: (Monad m,Functor m) => String -> Word -> Exp m () -- set a variable
setEnvW s w = do v <- expandWordE w
                 set s v

getEnvQC :: Monad m => Bool -> Bool -> String -> Exp m (Maybe Word)
getEnvQC q c n = do v <- get n
                    case v of
                      Nothing -> return Nothing
                      Just "" -> if c then return Nothing
                                      else return $ Just []
                      Just v' -> return $ Just $ quoteLiteral q v'

getEnvQ :: Monad m => Bool -> String -> Exp m Word
getEnvQ True "*" = undefined -- special treatment
getEnvQ q n = do v <- get n
                 case v of
                   Nothing -> return $ []
                   Just v' -> return $ quoteLiteral q v'

-- |Helper function for expansions...  The @Bool@ argument is for
-- whether or not we're quoted.
expandWith :: Monad m => (Bool -> Expansion -> Exp m Word)
           -> Word -> Exp m Word
expandWith f (Expand x:xs) = do x' <- f False x
                                xs' <- expandWith f xs
                                return $ x' ++ xs'
expandWith f (Quoted (Expand x):xs) = do x' <- f True x
                                         xs' <- expandWith f xs
                                         return $ x' ++ xs'
expandWith f (x:xs) = do fmap (x:) $ expandWith f xs
expandWith _ [] = return []

-- |Use @$IFS@ to split fields.
splitFields :: Monad m => [Word] -> Exp m [Word]
splitFields w = do ifs <- fmap (fromMaybe " \t") $ get "IFS"
                   let f (Literal c) = c `elem` ifs
                       f _ = False
                       split = filter (any (not . f)) . (groupBy ((==) `on` f))
                   return $ concatMap split w

-- |This always returns a LitWord.
removeQuotes :: Word -> String
removeQuotes [] = ""
removeQuotes (Quote _:xs) = removeQuotes xs
removeQuotes (Quoted x:xs) = removeQuotes $ x:xs
removeQuotes (Expand _:xs) = undefined -- shouldn't happen
removeQuotes (Literal c:xs) = c:removeQuotes xs

-- *Math-parsing
-- |We use a stateful parser, keeping track of all the current expansions,
-- as well as all the new assignments we need to make...
-- How can we do the ternary operator with parsec...? its slowness makes
-- it at least somewhat tractable...
doMath :: Monad m => String -> Exp m Int
doMath s = do subs <- get'
              case runMathParser subs s of
                Left err -> fail err
                Right (r,ss) -> do forM_ ss $ \(n,v) -> set n $ show v
                                   return r

---
