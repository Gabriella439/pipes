-- | This module defines functors in the category of proxies

{-# LANGUAGE KindSignatures, Rank2Types, Safe #-}

module Control.PFunctor (
    -- * Functors over Proxies
    PFunctor(..),
    hoistPK,
    raiseP,
    raisePK,
    ) where

import Control.Proxy.Class (Proxy)
import Control.Proxy.Trans (ProxyTrans(liftP))

{-| A functor in the category of proxies

> hoistP f . hoistP g = hoistP (f . g)
>
> hoistP id = id

    If @f@ is a proxy morphism, then @hoistP f@ is a proxy morphism, meaning
    that @hoistPK f = (hoistP f .)@ defines a functor between five categories.

    Functor between Kleisli categories:

> hoistPK f p1 >=> hoistPK f p2 = hoistPK f (p1 >=> p2)
>
> hoistPK f return = return

    Functor between 'P.Proxy' categories:

> hoistPK f p1 >-> hoistPK f p2 = hoistPK f (p1 >-> p2)
>
> hoistPK f idT = idT

> hoistPK f p1 >~> hoistPK f p2 = hoistPK f (p1 >~> p2)
>
> hoistPK f coidT = coidT

    Functor between \"request\" categories:

> hoistPK f p1 \>\ hoistPK f p2 = hoistPK f (p2 \>\ p2)
>
> hoistPK f request = request

    Functor between \"respond\" categories:

> hoistPK f p1 />/ hoistPK f p2 = hoistPK f (p2 />/ p2)
>
> hoistPK f respond = respond
-}
class PFunctor (t
    :: (* -> * -> * -> * -> (* -> *) -> * -> *)
    ->  * -> * -> * -> * -> (* -> *) -> * -> * ) where
    {-| Lift a proxy morphism from @p1@ to @p2@ into a proxy morphism from
        @(t p1)@ to @(t p2)@ -}
    hoistP
     :: (Monad m, Proxy p1)
     => (forall r1 . p1 a' a b' b m r1 ->   p2 a' a b' b n r1) -- ^ Proxy morphism
     -> (          t p1 a' a b' b m r2 -> t p2 a' a b' b n r2)

-- | Convenience function equivalent to @(hoistP f .)@
hoistPK
 :: (Monad m, Proxy p1, PFunctor t)
 => (forall r1 . p1 a' a b' b m r1 -> p2 a' a b' b n r1) -- ^ Proxy morphism
 -> (q -> t p1 a' a b' b m r2) -- ^ Proxy Kleisli arrow
 -> (q -> t p2 a' a b' b n r2)
hoistPK f = (hoistP f .)

{-| Lift the base proxy

> raiseP = hoistP liftP
-}
raiseP
 :: (Monad m, Proxy p, PFunctor t1, ProxyTrans t2)
 => t1 p a' a b' b m r -- ^ Proxy
 -> t1 (t2 p) a' a b' b m r
raiseP = hoistP liftP

{-| Lift the base proxy of a \'@K@\'leisli arrow

> raisePK = hoistPK liftP
-}
raisePK
 :: (Monad m, Proxy p, PFunctor t1, ProxyTrans t2)
 => (q -> t1 p a' a b' b m r) -- ^ Proxy Kleisli arrow
 -> (q -> t1 (t2 p) a' a b' b m r)
raisePK = hoistPK liftP
