{-# LANGUAGE CPP #-}
module Language.Sh.Glob where

import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.State ( runState, put )
import Data.Char ( ord, chr )
import Data.List ( isPrefixOf, partition )
import System.Directory ( getCurrentDirectory )
import System.FilePath ( pathSeparator, isPathSeparator, isExtSeparator )

import Language.Sh.Syntax ( Word, Lexeme(..) )

-- we might get a bit fancier if older glob libraries will support
-- a subset of what we want to do...?
#ifdef HAVE_GLOB
import System.FilePath.Glob ( tryCompile, globDir )
#endif

-- By the time this is called, we should only have quotes and quoted
-- literals to worry about.  In the event of finding an unquoted glob
-- char (and if the glob matches) we'll automatically remove quotes, etc.
-- (since the next stage is, after all, quote removal).
mkGlob :: Word -> Maybe String
mkGlob w = case runState (mkG w) False of
             (s,True) -> Just s
             _  -> Nothing
    where mkG [] = return []
          mkG (Literal '[':xs) = case mkClass xs of
                                   Just (g,xs') -> fmap (g++) $ mkG xs'
                                   Nothing -> fmap ((mkLit '[')++) $ mkG xs
          mkG (Literal '*':Literal '*':xs) = mkG $ Literal '*':xs
          mkG (Literal '*':xs) = put True >> fmap ('*':) (mkG xs)
          mkG (Literal '?':xs) = put True >> fmap ('?':) (mkG xs)
          mkG (Literal c:xs) = fmap (mkLit c++) $ mkG xs
          mkG (Quoted (Literal c):xs) = fmap (mkLit c++) $ mkG xs
          mkG (Quoted q:xs) = mkG $ q:xs
          mkG (Quote _:xs) = mkG xs
          mkLit '[' = "[[]"
          mkLit '*' = "[*]"
          mkLit '?' = "[?]"
          mkLit '<' = "[<]"
          mkLit  c  =  [c]

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
          quoted c x = case x of Quoted (Quoted x) -> quoted c $ Quoted x
                                 Quoted (Literal c') -> c==c'
                                 _ -> False
          movedash s = let (d,nd) = partition (quoted '-') s
                           bad = null d || (isLit '-' $ head $ reverse s)
                       in map fromLexeme $ if bad then nd else nd++d
          fromLexeme x = case x of { Literal c -> c; Quoted q -> fromLexeme q }

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

expandGlob :: MonadIO m => Word -> m [FilePath]
#ifdef HAVE_GLOB
expandGlob w = case mkGlob w of
                 Nothing -> return []
                 Just g  -> case tryCompile g of
                              Right g' -> liftIO $
                                do cwd <- getCurrentDirectory
                                   hits <- globDir [g'] cwd
                                   let ps = [pathSeparator]
                                   return $ map (removePrefix $ cwd++ps) $
                                          head $ fst $ hits
                              _ -> return []
    where removePrefix pre s | pre `isPrefixOf` s = drop (length pre) s
                             | otherwise = s
#else
expandGlob = const $ return []
#endif

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