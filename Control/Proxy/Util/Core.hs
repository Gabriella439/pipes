-- | Utility functions that other utility functions require

module Control.Proxy.Util.Core (
    -- * Core utility functions
    -- $utility
    foreverK,
    replicateK
    ) where

import Control.Monad (forever, (>=>))
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

-- | Compose a \'K\'leisli arrow with itself forever
foreverK :: (Monad m) => (a -> m a) -> (a -> m b)
foreverK k = let r = k >=> r in r
{- foreverK uses 'let' to avoid a space leak.
   See: http://hackage.haskell.org/trac/ghc/ticket/5205 -}

-- | Repeat a \'K\'leisli arrow multiple times
replicateK :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
replicateK n k = go n where
    go n
        | n < 1     = return
        | n == 1    = k
        | otherwise = k >=> go (n - 1)
