-- | This module defines functors in the category of proxies

{-# LANGUAGE KindSignatures, Rank2Types #-}

module Control.PFunctor (
    -- * Functors over Proxies
    PFunctor(..),
    raiseP
    ) where

import Control.Proxy.Class (Proxy)
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | A functor in the category of monads
class PFunctor (
    t :: (* -> * -> * -> * -> (* -> *) -> * -> *)
      ->  * -> * -> * -> * -> (* -> *) -> * -> * ) where
    {-| Lift a proxy morphism from @p@ to @q@ into a proxy morphism from
        @(t p)@ to @(t q)@ -}
    hoistP
     :: (Monad m, Proxy p)
     => (forall a' a b' b r1 . p a' a b' b m r1 -> q a' a b' b m r1)
     -> (t p a' a b' b m r2 -> t q a' a b' b m r2)

{-| Lift the base proxy

> raiseP = hoistP liftP
-}
raiseP
 :: (Monad m, Proxy p, PFunctor t1, ProxyTrans t2)
 => t1 p a' a b' b m r -> t1 (t2 p) a' a b' b m r
raiseP = hoistP liftP
