{-# LANGUAGE KindSignatures #-}

{-| You can define your own extensions to the 'Proxy' type by writing your own
    \"proxy transformers\".  Proxy transformers are monad transformers that
    correctly lift 'Proxy' composition from the base monad.  Stack multiple
    proxy transformers to chain features together. -}
    
module Control.Proxy.Trans (
    -- * Proxy Transformers
    ProxyTrans(..)
    )where

import Control.Proxy.Class

{-| 'mapP' defines a functor that preserves 'Proxy' composition and Kleisli
    composition.

    Laws:

    * Functor between 'Proxy' categories

> mapP (f <-< g) = mapP f <-< mapP g
> mapP idT = idT

    * Functor between Kleisli categories

> mapP (f <=< g) = mapP f <=< mapP g
> mapP return = return

    Minimal complete definition: 'mapP' or 'liftP'.  Defining 'liftP' is more
    efficient.
-}
class ProxyTrans
      (t :: (* -> * -> * -> * -> (* -> *) -> * -> *)
         ->  * -> * -> * -> * -> (* -> *) -> * -> * )
      where
    liftP :: (Monad m, MonadP p, Channel p, MFunctorP p)
          => p b c d e m r -> t p b c d e m r
    liftP f = mapP (\() -> f) ()

    mapP :: (Monad m, MonadP p, Channel p, MFunctorP p)
         => (a -> p b c d e m r) -> (a -> t p b c d e m r)
    mapP = (liftP .)
