module Language.Sh.Compat ( on, (<=<) ) where

-- |This module just defines functions that aren't in ghc-6.6.
-- Once 6.6 falls out of debian stable, we can switch to just importing
-- them from base.  For this reason, we mustn't expose this module!

-- |This is in Data.Function, starting in 6.8
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a a' = g a `f` g a'

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) g f a = f a >>= g
