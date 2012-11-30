{-| This module temporarily holds this class until it can find a better home.

    Note: When combining two monad transformers with two different base monads,
    use @(hoist lift)@ to lift one, not @(hoist runXXX)@ to lower one.  The
    former approach not only works better but also provides 'hoist' with a monad
    morphism, which is what it expects in theory.
-}

{-# LANGUAGE Rank2Types #-}

module Control.MFunctor (
    -- * Monads over functors
    MFunctor(..)
    ) where

-- | A functor in the category of monads
class MFunctor t where
    {-| Lift a monad morphism from @m@ to @n@ into a monad morphism from
        @(t m)@ to @(t n)@ -}
    hoist :: (Monad m) => (forall a . m a -> n a) -> t m b -> t n b
