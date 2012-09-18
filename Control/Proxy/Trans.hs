{-| This module exports the 'ProxyTrans' class which allows users to define
    their own proxy transformers. -}

module Control.Proxy.Trans (
    -- * Proxy Transformers
    -- $proxytrans
    ProxyTrans(..)
    )where

import Control.Proxy.Class

-- Imports for documentation
import Control.Monad ((<=<))

{- $proxytrans
    You can define your own extension to 'Proxy'-like types by structuring your
    extension as a proxy transformer.  A proxy transformer only assumes that the
    base type implements the 'ProxyC' and 'Monad' type classes and derives an
    extended type that also implements 'ProxyC' and 'Monad'.  You can then layer
    this extension within any proxy transformer stack.
-}

{-| 'mapP' defines a functor from four base categories to four transformed
    categories.  For simplicity I refer to each category by its composition
    operator:

    Laws:

    * Functor from base ('<=<') category to transformed ('<=<') category

> mapP (f <=< g) = mapP f <=< mapP g
> mapP return = return

    * Functor from base ('<-<') category to transformed ('<-<') category

> mapP (f <-< g) = mapP f <-< mapP g
> mapP idT = idT

    * Functor from base ('/</') category to transformed ('/</') category

> mapP (f /</ g) = mapP f /</ mapP g
> mapP request = request

    * Functor from base ('\<\') category to transformed ('\<\') category

> mapP (f \<\ g) = mapP f \<\ mapP g
> mapP respond = respond

    Minimal definition, 'mapP' or 'liftP'
-}
class ProxyTrans t where
    liftP :: (ProxyC p, Monad (p b c d e m))
          => p b c d e m r -> t p b c d e m r
    liftP f = mapP (\() -> f) ()

    mapP :: (ProxyC p, Monad (p b c d e m))
         => (a -> p b c d e m r) -> (a -> t p b c d e m r)
    mapP = (liftP .)
