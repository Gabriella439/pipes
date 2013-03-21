{-| A proxy morphism is a natural transformation:

> morph :: forall r . p a' a b' b m r -> q a' a b' b n r

    ... that defines a functor between five categories:

    * Functor between Kleisli categories:

> morph p1 >=> morph p2 = morph (p1 >=> p2)
>
> morph return = return

    * Functor between 'P.Proxy' composition categories:

> morph p1 >-> morph p2 = morph (p1 >-> p2)
>
> morph idT = idT

> morph p1 >~> morph p2 = morph (p1 >~> p2)
>
> morph coidT = coidT

    * Functor between 'ListT' Kleisli categories:

> morph p1 \>\ morph p2 = morph (p2 \>\ p2)
>
> morph request = request

> morph p1 />/ morph p2 = morph (p2 />/ p2)
>
> morph respond = respond

    Examples of proxy morphisms include:

    * 'liftP' (from 'ProxyTrans')

    * 'squashP' (See below)

    * @'hoistP' f@ (See below) if @f@ is a proxy morphism

    * @(f . g)@, if @f@ and @g@ are both proxy morphisms

    * 'id'

    Proxy morphisms commonly arise when manipulating existing proxy transformer
    code for compatibility purposes.  The 'PFUnctor', 'ProxyTrans', and 'PMonad'
    classes define standard ways to change proxy transformer stacks:

    * 'liftP' introduces a new proxy transformer layer of any type:.

    * 'squashP' flattens two identical monad transformer layers into a single
      layer of the same type.

    * 'hoistP' maps proxy morphisms to modify deeper layers of the proxy
      transformer stack.
-}

{-# LANGUAGE KindSignatures, Rank2Types #-}

module Control.Proxy.Morph (
    -- * Functors over Proxies
    PFunctor(..),

    -- * Monads over Proxies
    PMonad(..),
    squashP
    ) where

import Control.Proxy.Class (Proxy)
import Control.Proxy.Trans (ProxyTrans(liftP))

{-| A functor in the category of proxies, using 'hoistP' as the analog of
    'fmap':

> hoistP f . hoistP g = hoistP (f . g)
>
> hoistP id = id
-}
class PFunctor (t
    :: (* -> * -> * -> * -> (* -> *) -> * -> *)
    ->  * -> * -> * -> * -> (* -> *) -> * -> * ) where
    {-| Lift a proxy morphism from @p1@ to @p2@ into a proxy morphism from
        @(t p1)@ to @(t p2)@
    -}
    hoistP
        :: (Monad m, Proxy p1)
        => (forall r1 . p1 a' a b' b m r1 ->   p2 a' a b' b n r1) -- ^ Proxy morphism
        -> (          t p1 a' a b' b m r2 -> t p2 a' a b' b n r2)

{-| A monad in the category of monads, using 'liftP' from 'ProxyTrans' as the
    analog of 'return' and 'embedP' as the analog of ('=<<'):

> embedP liftP = id
>
> embedP f (liftP p) = f p
>
> embed g (embed f t) = embed (\p -> embed g (f p)) t
-}
class (PFunctor t, ProxyTrans t) => PMonad t where
    {-| Embed a newly created 'PMonad' layer within an existing layer

        'embedP' is analogous to ('=<<')
    -}
    embedP
        :: (Monad n, Proxy p2)
        => (forall r1 . p1 a' a b' b m r1 -> t p2 a' a b' b n r1)
        -> (          t p1 a' a b' b m r2 -> t p2 a' a b' b n r2)

{-| Squash to 'PMonad' layers into a single layer

    'squashP' is analogous to 'join'
-}
squashP
    :: (Monad m, Proxy p, PMonad t)
    => t (t p) a' a b' b m r -> t p a' a b' b m r
squashP = embedP id
{-# INLINABLE squashP #-}
