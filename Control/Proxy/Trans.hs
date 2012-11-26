{-| You can define your own proxy extensions by writing your own \"proxy
    transformers\".  Proxy transformers are monad transformers that correctly
    lift all proxy operations from the base proxy type to the extended proxy
    type.  Stack multiple proxy transformers to chain features together.
-}
    
module Control.Proxy.Trans (
    -- * Proxy Transformers
    ProxyTrans(..)
    ) where

import Control.Proxy.Class

{-| 'mapP' defines a functor that preserves four categories:

    * Proxy category

    * Kleisli category

    * \"request\" category

    * \"respond\" category

    Laws:

    * Functor between 'Proxy' categories

> mapP (f <-< g) = mapP f <-< mapP g
>
> mapP idT = idT

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

    Minimal complete definition:

    * 'mapP' or 'liftP'

    Defining 'liftP' is more efficient.
-}
class ProxyTrans t where
    liftP :: (Monad m, Proxy p)
          => p b c d e m r -> t p b c d e m r
    liftP f = mapP (\() -> f) ()

    mapP :: (Monad m, Proxy p)
         => (a -> p b c d e m r) -> (a -> t p b c d e m r)
    mapP = (liftP .)
