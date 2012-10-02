-- | This module temporarily holds this class until it can find a better home.

{-# LANGUAGE Rank2Types #-}

module Control.MFunctor (
    -- * Monads over functors
    MFunctor(..)
    ) where

-- | A functor in the category of monads
class MFunctor t where
    {-| Lift a monad morphism from @m@ to @n@ into a monad morphism from
        @(t m)@ to @(t n)@ -}
    mapT :: (Monad m, Monad n) => (forall a . m a -> n a) -> t m b -> t n b
