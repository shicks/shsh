{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances,
             FlexibleInstances, FlexibleContexts, CPP #-}

-- |This module defines all the helper functions for dealing
-- environment variables.  We also have resorted to a newtype
-- now for actually storing the things internally, so that
-- we can get the case-sensitivity at the type/class level.

-- |I would do better to define a class or other data type and
-- only expose its interface...  then we can ensure that, e.g.
-- no duplicate keys ever creep in, thus eliminating a possible
-- need for 'alter'' and such.  This would also clean up the
-- need for a @newtype Var@, although it would introduce phantom
-- types, possibly, for collation.

module System.Console.ShSh.Var ( Equiv, (===), convR, convL,
                                 Var(..), VarFlags(..),
                                 exportedVar, setExportedFlag,
                                 lookup, update, alter, alterM,
                                 Maybeable, toMaybe,
                                 Stringy, maybeToStringy,
                                 setVarHelper, setFlagHelper ) where

import Prelude hiding ( lookup )

import Control.Monad ( MonadPlus, mzero )
#ifdef WINDOWS
import Data.Char ( toLower )
#endif
import Data.Maybe ( listToMaybe )
import Data.Monoid ( Monoid, mempty, mappend )

class Equiv a b where
    (===) :: a -> b -> Bool
    convR :: a -> b
    convL :: b -> a

instance Eq a => Equiv a a where
    (===) = (==)
    convR = id
    convL = id

newtype Var = Var { unVar :: String }
instance Show Var where
    show (Var v) = "Var "++v

instance Eq Var where
#ifdef WINDOWS
    (Var a) == (Var b) = map toLower a == map toLower b
#else
    (Var a) == (Var b) = a == b
#endif

instance Equiv Var String where
    (===) v b = (===) v (Var b)
    convR = unVar
    convL = Var
instance Equiv String Var where
    (===) a v = (===) (Var a) v
    convR = Var
    convL = unVar

data VarFlags = VarFlags { exported :: Bool, readonly :: Bool }

instance Show VarFlags where
    show (VarFlags x r) = "(" ++ (if x then " exported" else "")
                              ++ (if r then " readonly" else "") ++ " )"

-- This is indeed a monoid under ||, but is that really what we want?
-- Since we never actually use mappend, is it better to just define
-- our own noFlags function?  Or we could just define an Emptyable
-- class with a single empty function?
instance Monoid VarFlags where
    mempty = VarFlags False False
    VarFlags a b `mappend` VarFlags a' b' = VarFlags (a||a') (b||b')

exportedVar :: VarFlags
exportedVar = VarFlags True False

setExportedFlag :: Bool -> VarFlags -> VarFlags
setExportedFlag x f = f { exported = x }

-- Simple routines to check/update associative list elements.
lookup :: Equiv a' a => a -> [(a',b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((x',y):xs) | x'===x    = Just y
                     | otherwise = lookup x xs

update :: Equiv a' a => a -> b -> [(a',b)] -> [(a',b)]
update x y [] = [(convL x,y)]
update x y ((x',y'):xs) | x'===x    = (x',y):xs
                        | otherwise = (x',y'):update x y xs

-- alter' :: Equiv a' a => a -> (Maybe b -> Maybe b) -> [(a',b)] -> [(a',b)]
-- alter' x f [] = case f Nothing of
--                   Just y  -> [(convL x,y)]
--                   Nothing -> []
-- alter' x f ((x0,y0):xs) | x0===x    = case f $ Just y0 of
--                                         Just y' -> (x0,y'):alter' x f xs
--                                         Nothing -> alter x f xs
--                         | otherwise = (x0,y0):alter' x f xs

-- -- More elaborate updateWith...
-- alterM' :: (Monad m,Functor m,Equiv a' a)
--         => a -> (Maybe b -> m (Maybe b)) -> [(a',b)] -> m [(a',b)]
-- alterM' x f [] = do y <- f Nothing
--                     return $ case y of Nothing -> []
--                                        Just y' -> [(convL x,y')]
-- alterM' x f ((x0,y0):xs) | x0===x = do y <- f $ Just y0
--                                        xs' <- alterM' x f xs -- might duplicate!
--                                        return $ case y of
--                                          Nothing -> xs'
--                                          Just y' -> (x0,y'):xs'
--                          | otherwise = ((x0,y0):) `fmap` alterM' x f xs

alter :: Equiv a' a => a -> (Maybe b -> Maybe b) -> [(a',b)] -> [(a',b)]
alter x f [] = case f Nothing of
                 Just y  -> [(convL x,y)]
                 Nothing -> []
alter x f ((x0,y0):xs) | x0===x    = case f $ Just y0 of
                                       Just y' -> (x0,y'):xs
                                       Nothing -> xs
                       | otherwise = (x0,y0):alter x f xs

-- More elaborate updateWith...
alterM :: (Monad m,Functor m,Equiv a' a)
       => a -> (Maybe b -> m (Maybe b)) -> [(a',b)] -> m [(a',b)]
alterM x f [] = do y <- f Nothing
                   return $ case y of Nothing -> []
                                      Just y' -> [(convL x,y')]
alterM x f ((x0,y0):xs) | x0===x = do y <- f $ Just y0
                                      return $ case y of
                                        Nothing -> xs
                                        Just y' -> (x0,y'):xs
                        | otherwise = ((x0,y0):) `fmap` alterM x f xs

class Maybeable a m where
    toMaybe :: m -> Maybe a
instance Maybeable a (Maybe a) where
    toMaybe = id
instance Maybeable a [a] where
    toMaybe = listToMaybe
instance Maybeable a a where
    toMaybe = Just

class Stringy m s where
    maybeToStringy :: Maybe String -> String -> m s
instance (Monad m,MonadPlus n) => Stringy m (n String) where
    maybeToStringy (Just x) _ = return $ return x
    maybeToStringy Nothing  _ = return $ mzero
instance Monad m => Stringy m String where
    maybeToStringy (Just x) _ = return x
    maybeToStringy Nothing  s = fail $ s ++ " not set"

-- We could be a bit smarter, but we might as well just try to emulate
-- bash's behavior... (i.e. we could set the global version also no 
-- matter what)

-- |This has gotten a bit trickier... typically we pass this a
-- Maybe (Maybe String).  The first Maybe tells whether to eliminate
-- the record entirely, the second whether to simply mask it.  This
-- is tricky semantics, with export, readonly, locals, etc...
setVarHelper :: (Monad m, Functor m, Equiv v' v, Equiv String v)
             => v -> Maybe (Maybe String)
             -> [(v',(VarFlags,Maybe String))]
             -> m [(v',(VarFlags,Maybe String))]
setVarHelper n Nothing xs = alterM n f xs
    where f (Just (flags,_)) | readonly flags = fail $ convL n++": is read only"
          f _ = return Nothing -- any other flags -> delete
setVarHelper n (Just v) xs = alterM n f xs
    where f Nothing = return $ Just (mempty,v) -- v could be Nothing...
          f (Just (flags,_)) | readonly flags = fail $ convL n++": is read only"
                             | otherwise = return $ Just (flags,v)

setFlagHelper :: Equiv v' v => v -> (VarFlags -> VarFlags)
              -> [(v',(VarFlags,Maybe String))]
              -> [(v',(VarFlags,Maybe String))]
setFlagHelper n f xs = alter n ff xs
    where ff Nothing = Just (f mempty,Nothing)
          ff (Just (flags,x)) = Just (f flags,x)
