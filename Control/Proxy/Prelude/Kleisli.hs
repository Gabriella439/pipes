-- | Utility functions for Kleisli arrows

module Control.Proxy.Prelude.Kleisli (
    -- * Core utility functions
    -- $utility
    foreverK,
    replicateK,
    mapK
    ) where

import Control.Monad (forever, (>=>))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (Interact(request, respond))
import Data.Closed (C)

{- $utility
    Use 'foreverK' to abstract away the following common pattern:

> p a = do
>     ...
>     a' <- respond b
>     p a'

    Using 'foreverK', you can avoid the manual recursion:

> p = foreverK $ \a -> do
>     ...
>     respond b
-}

-- | Compose a \'@K@\'leisli arrow with itself forever
foreverK :: (Monad m) => (a -> m a) -> (a -> m b)
foreverK k = let r = k >=> r in r
{- foreverK uses 'let' to avoid a space leak.
   See: http://hackage.haskell.org/trac/ghc/ticket/5205 -}

-- | Repeat a \'@K@\'leisli arrow multiple times
replicateK :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
replicateK n k = go n where
    go n
        | n < 1     = return
        | n == 1    = k
        | otherwise = k >=> go (n - 1)

{-| Convenience function equivalent to @(lift .)@

> mapK f >=> mapK g = mapK (f >=> g)
>
> mapK return = return
-}
mapK :: (Monad m, MonadTrans t) => (a -> m b) -> (a -> t m b)
mapK = (lift .)
{-# INLINABLE mapK #-}
