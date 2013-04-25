-- | Utility functions for Kleisli arrows

{-# LANGUAGE Rank2Types #-}

module Control.Proxy.Prelude.Kleisli (
    -- * Core utility functions
    foreverK,
    replicateK,
    liftK,
    hoistK,
    raise,
    raiseK,
    hoistPK,
    raiseP,
    raisePK
    ) where

import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (Proxy)
import Control.Proxy.Morph (PFunctor(hoistP))
import Control.Proxy.Trans (ProxyTrans(liftP))

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
   See: http://hackage.haskell.org/trac/ghc/ticket/5205
-}
{-# INLINABLE foreverK #-}

-- | Repeat a \'@K@\'leisli arrow multiple times
replicateK :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
replicateK n0 k = go n0 where
    go n
        | n < 1     = return
        | n == 1    = k
        | otherwise = \a -> k a >>= go (n - 1)
{-# INLINABLE replicateK #-}

{-| Convenience function equivalent to @(lift .)@

> liftK f >=> liftK g = liftK (f >=> g)
>
> liftK return = return
-}
liftK :: (Monad m, MonadTrans t) => (a -> m b) -> (a -> t m b)
liftK k a = lift (k a)
-- liftK = (lift .)
{-# INLINABLE liftK #-}

-- | Convenience function equivalent to @(hoist f .)@
hoistK
    :: (Monad m, MFunctor t)
    => (forall a . m a -> n a)  -- ^ Monad morphism
    -> (b' -> t m b)            -- ^ Kleisli arrow
    -> (b' -> t n b)
hoistK k p a' = hoist k (p a')
-- hoistK k = (hoist k .)
{-# INLINABLE hoistK #-}

{-| Lift the base monad

> raise = hoist lift
-}
raise :: (Monad m, MFunctor t1, MonadTrans t2) => t1 m r -> t1 (t2 m) r
raise = hoist lift
{-# INLINABLE raise #-}

{-| Lift the base monad of a \'@K@\'leisli arrow

> raiseK = hoistK lift
-}
raiseK
    :: (Monad m, MFunctor t1, MonadTrans t2)
    => (q -> t1 m r) -> (q -> t1 (t2 m) r)
raiseK = (hoist lift .)
{-# INLINABLE raiseK #-}

-- | Convenience function equivalent to @(hoistP f .)@
hoistPK
    :: (Monad m1, Proxy p1, PFunctor t)
    => (forall _a' _a _b' _b _r .
            p1 _a' _a _b' _b m1 _r -> p2 _a' _a _b' _b m2 _r)
    -- ^ Proxy morphism
    -> (q -> t p1 a' a b' b m1 r) -- ^ Proxy Kleisli arrow
    -> (q -> t p2 a' a b' b m2 r)
hoistPK f = (hoistP f .)
{-# INLINABLE hoistPK #-}

{-| Lift the base proxy

> raiseP = hoistP liftP
-}
raiseP
    :: (Monad m, Proxy p, PFunctor t1, ProxyTrans t2)
    => t1 p a' a b' b m r -- ^ Proxy
    -> t1 (t2 p) a' a b' b m r
raiseP = hoistP liftP
{-# INLINABLE raiseP #-}

{-| Lift the base proxy of a \'@K@\'leisli arrow

> raisePK = hoistPK liftP
-}
raisePK
    :: (Monad m, Proxy p, PFunctor t1, ProxyTrans t2)
    => (q -> t1 p a' a b' b m r) -- ^ Proxy Kleisli arrow
    -> (q -> t1 (t2 p) a' a b' b m r)
raisePK = hoistPK liftP
{-# INLINABLE raisePK #-}
