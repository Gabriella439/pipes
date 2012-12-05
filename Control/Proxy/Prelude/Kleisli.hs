{-# LANGUAGE Rank2Types #-}

-- | Utility functions for Kleisli arrows

module Control.Proxy.Prelude.Kleisli (
    -- * Core utility functions
    foreverK,
    replicateK,
    liftK,
    hoistK,
    ) where

import Control.MFunctor (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))

{-| Compose a \'@K@\'leisli arrow with itself forever

    Use 'foreverK' to abstract away the following common recursion pattern:

> p a = do
>     ...
>     a' <- respond b
>     p a'

    Using 'foreverK', you can instead write:

> p = foreverK $ \a -> do
>     ...
>     respond b
-}
foreverK :: (Monad m) => (a -> m a) -> (a -> m b)
foreverK k = let r = \a -> k a >>= r in r
{- foreverK uses 'let' to avoid a space leak.
   See: http://hackage.haskell.org/trac/ghc/ticket/5205 -}

-- | Repeat a \'@K@\'leisli arrow multiple times
replicateK :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
replicateK n0 k = go n0 where
    go n
        | n < 1     = return
        | n == 1    = k
        | otherwise = \a -> k a >>= go (n - 1)

{-| Convenience function equivalent to @(lift .)@

> liftK f >=> liftK g = liftK (f >=> g)
>
> liftK return = return
-}
liftK :: (Monad m, MonadTrans t) => (a -> m b) -> (a -> t m b)
liftK k a = lift (k a)
-- liftK = (lift .)

{-| Convenience function equivalent to @(hoist f .)@

> hoistK f >-> hoistK g = hoistK (f >-> g)
>
> hoistK idPull = idPull
-}
hoistK
 :: (Monad m, MFunctor t)
 => (forall a . m a -> n a) -> ((b' -> t m b) -> (b' -> t n b))
hoistK k p a' = hoist k (p a')
-- hoistK k = (hoist k .)
