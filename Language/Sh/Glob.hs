{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module Language.Sh.Glob ( expandGlob, matchPattern,
                          removePrefix, removeSuffix ) where

import Control.Monad.Trans ( MonadIO )
import Control.Monad.State ( runState, put )
import Data.List ( isPrefixOf, partition )
import Data.Maybe ( isJust, listToMaybe )
import Text.Regex.PCRE.Light.Char8 ( Regex, compileM, match, ungreedy )

import Language.Sh.Syntax ( Lexeme(..), Word )

-- we might get a bit fancier if older glob libraries will support
-- a subset of what we want to do...?
import Control.Monad.Trans ( liftIO )
#ifdef HAVE_GLOB
import System.FilePath.Glob ( compileWith, compPosix,
                              globDir, commonDirectory )
#else
import Data.List ( sort, tails )
import System.Directory ( getDirectoryContents )
#endif

expandGlob :: MonadIO m => Word -> m [FilePath]
#ifdef HAVE_GLOB
expandGlob w = case mkGlob w of
                 Nothing -> return []
                 Just g  -> let g' = compileWith compPosix g
                            in liftIO $
                                do let (dir,g'') = commonPrefix g'
                                   liftIO $ putStrLn $ show (dir,g'')
                                   hits <- globDir [g''] dir
                                   return $ head $ fst $ hits

-- By the time this is called, we should only have quotes and quoted
-- literals to worry about.  In the event of finding an unquoted glob
-- char (and if the glob matches) we'll automatically remove quotes, etc.
-- (since the next stage is, after all, quote removal).
-- mkGlob :: Word -> Maybe String
-- mkGlob w = case runState (mkG w) False of
--              (s,True) -> Just s
--              _  -> Nothing
--     where mkG [] = return []
--           mkG (Literal '[':xs) = case mkClass xs of
--                                    Just (g,xs') -> fmap (g++) $ mkG xs'
--                                    Nothing -> fmap ((mkLit '[')++) $ mkG xs
--           mkG (Literal '*':Literal '*':xs) = mkG $ Literal '*':xs
--           mkG (Literal '*':xs) = put True >> fmap ('*':) (mkG xs)
--           mkG (Literal '?':xs) = put True >> fmap ('?':) (mkG xs)
--           mkG (Literal c:xs) = fmap (mkLit c++) $ mkG xs
--           mkG (Quoted (Literal c):xs) = fmap (mkLit c++) $ mkG xs
--           mkG (Quoted q:xs) = mkG $ q:xs
--           mkG (Quote _:xs) = mkG xs
--           mkG l = error $ "bad lexeme: "++show l
--           mkLit c | c `elem` "[*?<" = ['[',c,']']
--                   | otherwise       = [c]
#else
expandGlob [] = return []
expandGlob w = do let breakd [] = []
                      breakd x = case break isd x of
                                 (p,[]) -> [p]
                                 (p,ps) -> p : breakd (dropWhile isd ps)
                      isd (Literal '/') = True
                      isd (Quoted l) = isd l
                      isd _ = False
                      l2c (Literal c) = c
                      l2c (Quoted l) = l2c l
                      l2c l = error $ "bad lexeme in l2gs: "++ show l
                      isclose (Literal ']') = True
                      isclose _ = False
                      w2g [] = []
                      w2g (Literal '[':Literal '!':r) =
                          case break isclose r of
                          (m,_:r') -> NoneOf (map l2c m) : w2g r'
                          _ -> Lit '[' : Lit '!' : w2g r -- not a range
                      w2g (Literal '[':Literal '^':r) =
                          case break isclose r of
                          (m,_:r') -> NoneOf (map l2c m) : w2g r'
                          _ -> Lit '[' : Lit '^' : w2g r
                      w2g (Literal '[':r) =
                          case break isclose r of
                          (m,_:r') -> Alt (map l2c m) : w2g r'
                          _ -> Lit '[' : w2g r
                      w2g (Literal '*':r) = Many : w2g r
                      w2g (Literal '?':r) = One : w2g r
                      w2g (Literal c:r) = Lit c : w2g r
                      w2g (Quote _:r) = w2g r
                      w2g (Quoted (Quoted q):r) = w2g (Quoted q:r)
                      w2g (Quoted (Literal c):r) = Lit c : w2g r
                      w2g (Quoted (Quote _):r) = w2g r
                      w2g (Quoted x:r) = w2g (x:r) -- only expansions left
                      w2g l = error $ "bad lexeme: "++show l
                      whichd = if isd $ head w
                               then "/"
                               else "."
                  liftIO $ filePathMatches (map w2g $ breakd w) whichd

data Glob = Lit Char | Many | One | Alt [Char] | NoneOf [Char]
            deriving ( Show )

simpleMatch :: [Glob] -> String -> Bool
simpleMatch [] "" = True
simpleMatch (Many:rest) s = any (simpleMatch rest) $ tails s
simpleMatch (One:rest) (_:s) = simpleMatch rest s
simpleMatch (Lit x:rest) (c:cs) | x == c = simpleMatch rest cs
simpleMatch (Alt xs:rest) (c:cs) | c `elem` xs = simpleMatch rest cs
simpleMatch (NoneOf xs:rest) (c:cs) | c `notElem` xs = simpleMatch rest cs
simpleMatch _ _ = False

filePathMatches :: [[Glob]] -> FilePath -> IO [FilePath]
filePathMatches [] _ = return []
filePathMatches (g:gs) d = do xs <- filter (`notElem` [".",".."])
                                    `fmap` (getDirectoryContents d
                                            `catch` \_ -> return [])
                              let xs' = filter (simpleMatch g) $ case g of
                                                                 Lit _:_ -> xs
                                                                 _ -> filter notdot xs
                                  notdot ('.':_) = False
                                  notdot _ = True
                                  fpm x = map ((x++"/")++)
                                          `fmap` filePathMatches gs (d++'/':x)
                              case gs of
                                [] -> return $ sort xs'
                                _ -> (sort . concat) `fmap` mapM fpm xs'
#endif

-- This is basically gratuitously copied from Glob's internals.
mkClass :: Word -> Maybe (String,Word)
mkClass xs = let (range, rest) = break (isLit ']') xs
             in if null rest then Nothing
                else if null range
                     then let (range', rest') = break (isLit ']') (tail rest)
                          in if null rest' then Nothing
                             else do x <- cr' range'
                                     return (x,tail rest')
                     else do x <- cr' range
                             return (x,tail rest)
    where cr' s = Just $ "["++movedash (filter (not . isQuot) s)++"]"
          isLit c x = case x of { Literal c' -> c==c'; _ -> False }
          isQuot x = case x of { Quote _ -> True; _ -> False }
          quoted c x = case x of Quoted (Quoted x') -> quoted c $ Quoted x'
                                 Quoted (Literal c') -> c==c'
                                 _ -> False
          movedash s = let (d,nd) = partition (quoted '-') s
                           bad = null d || (isLit '-' $ head $ reverse s)
                       in map fromLexeme $ if bad then nd else nd++d
          fromLexeme x = case x of { Literal c -> c; Quoted q -> fromLexeme q;
                                     l -> error $ "bad lexeme "++show l }

{-
expandGlob :: MonadIO m => Word -> m [FilePath]
expandGlob w = case mkGlob w of
                 Nothing -> return []
                 Just g  -> case G.unPattern g of
                              (G.PathSeparator:_) -> liftIO $
                                do hits <- G.globDir [g] "/" -- unix...?
                                   let ps = [pathSeparator]
                                   return $ head $ fst $ hits
                              _ -> liftIO $
                                do cwd <- getCurrentDirectory
                                   hits <- G.globDir [g] cwd
                                   let ps = [pathSeparator]
                                   return $ map (removePrefix $ cwd++ps) $
                                          head $ fst $ hits
    where removePrefix pre s | pre `isPrefixOf` s = drop (length pre) s
                             | otherwise = s
-}

-- Two issues: we can deal with them here...
--  1. if glob starts with a dirsep then we need to go relative to root...
--     (what about in windows?)
--  2. if not, then we should remove the absolute path from the beginning of
--     the results (should be easy w/ a map)

{-
-- This is a sort of default matcher, but needn't be used...
matchGlob :: MonadIO m => Glob -> m [FilePath]
matchGlob g = matchG' [] $ splitDir return $ do -- now we're in the list monad...
    where d = splitDir g
          splitDir (c:xs) | ips c = []:splitDir (dropWhile ips xs)
          splitDir xs = filter (not . null) $
                        filter (not . all ips) $
                        groupBy ((==) on ips) xs
          ips x = case x of { Lit c -> isPathSeparator c; _ -> False }
-}



----------------------------------------------------------------------
-- This is copied from above, but it's used separately for non-glob --
-- pattern matching.  Maybe we'll combine them someday.             --
----------------------------------------------------------------------

match' :: Regex -> String -> Maybe String
match' regex s = listToMaybe =<< match regex s []

matchPattern :: Word -> String -> Bool
matchPattern w s = case mkRegex False False "^" "$" w of
                     Just r  -> isJust $ match r s []
                     Nothing -> fromLit w == s

removePrefix :: Bool   -- ^greediness
             -> Word   -- ^pattern
             -> String -- ^haystack
             -> String
removePrefix g n h = case mkRegex g False "^" "" n of
                       Just r -> case match' r h of
                                   Just m -> drop (length m) h
                                   Nothing -> h
                       Nothing -> if l `isPrefixOf` h
                                  then drop (length l) h
                                  else h
    where l = fromLit n

removeSuffix :: Bool   -- ^greediness
             -> Word   -- ^pattern
             -> String -- ^haystack
             -> String
removeSuffix g n h = case mkRegex g True "^" "" n of
                       Just r -> case match' r hr of
                                   Just m -> reverse $ drop (length m) hr
                                   Nothing -> h
                       Nothing -> if l `isPrefixOf` hr
                                  then reverse $ drop (length l) hr
                                  else h
    where l = reverse $ fromLit n
          hr = reverse h

mkRegex :: Bool   -- ^greedy?
        -> Bool   -- ^reverse? (before adding pre/suff)
        -> String -- ^prefix
        -> String -- ^suffix
        -> Word   -- ^pattern
        -> Maybe Regex
mkRegex g r pre suf w
    = case runState (mkR w) False of
        (s,True) -> mk' $ concat $ affix $ (if r then reverse else id) s
        _        -> Nothing
    where mkR [] = return []
          mkR (Literal '[':xs) = case mkClass xs of
                                   Just (c,xs') -> fmap (c:) $ mkR xs'
                                   Nothing -> fmap ((mkLit '['):) $ mkR xs
          mkR (Literal '*':Literal '*':xs) = mkR $ Literal '*':xs
          mkR (Literal '*':xs) = put True >> fmap (".*":) (mkR xs)
          mkR (Literal '?':xs) = put True >> fmap (".":) (mkR xs)
          mkR (Literal c:xs) = fmap (mkLit c:) $ mkR xs
          mkR (Quoted (Literal c):xs) = fmap (mkLit c:) $ mkR xs
          mkR (Quoted q:xs) = mkR $ q:xs
          mkR (Quote _:xs) = mkR xs
          mkR l = error $ "bad lexeme: "++show l
          mkLit c | c `elem` "[](){}|^$.*+?\\" = ['\\',c]
                  | otherwise                  = [c]
          affix s = pre:s++[suf]
          mk' s = case compileM s (if g then [] else [ungreedy]) of
                    Left _      -> Nothing
                    Right regex -> Just regex

fromLit :: Word -> String
fromLit = concatMap $ \l -> case l of Literal c -> [c]
                                      Quoted q  -> fromLit [q]
                                      _         -> []
