{-| You can define your own proxy extensions by writing your own \"proxy
    transformers\".  Proxy transformers are monad transformers that also
    correctly lift all proxy operations from the base proxy type to the
    extended proxy type.  Stack multiple proxy transformers to chain features
    together.
-}
    
module Control.Proxy.Trans (
    -- * Proxy Transformers
    ProxyTrans(..),
    mapP

    -- * Laws
    -- $laws
    ) where

import Control.Proxy.Class (Proxy)

-- | Uniform interface to lifting proxies
class ProxyTrans t where
    liftP :: (Monad m, Proxy p) => p a' a b' b m r -> t p a' a b' b m r

{-| Lift a 'Proxy' Kleisli arrow

> mapP = (lift .)
-}
mapP :: (Monad m, Proxy p, ProxyTrans t)
     => (q -> p a' a b' b m r) -> (q -> t p a' a b' b m r)
mapP = (liftP .)

{- $laws
     'mapP' defines a functor that preserves five categories:

    * Kleisli category

    * The two Proxy categories

    * \"request\" category

    * \"respond\" category

    Laws:

    * Functor between 'Proxy' categories

> mapP (f >-> g) = mapP f >-> mapP g
>
> mapP idT = idT

> mapP (f >~> g) = mapP f >~> mapP g
>
> mapP idPush = idPush

    * Functor between Kleisli categories

> mapP (f <=< g) = mapP f <=< mapP g
>
> mapP return = return

    * Functor between \"request\" categories

> mapP (f /</ g) = mapP f /</ mapP g -- when /</ is defined
>
> mapP request = request

    * Functor between \"respond\" categories

> mapP (f \<\ g) = mapP f \<\ mapP g -- when \<\ is defined
>
> mapP respond = respond
-}
