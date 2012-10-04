-- | Utility functions that other utility functions require

module Control.Proxy.Util.Core (
    -- * Core utility functions
    -- $utility
    discard,
    ignore,
    foreverK,
    replicateK
    ) where

import Control.Monad (forever, (>=>))
import Control.Proxy.Class (Interact(request, respond))
import Data.Closed (C)

{- $utility
    'discard' provides a fallback client that gratuitously 'request's input
    from a server, but discards all responses.

    'ignore' provides a fallback server that trivially 'respond's with output
    to a client, but ignores all request parameters.

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

-- | Discard all responses
discard :: (Monad m, Monad (p () a () C m), Interact p) => () -> p () a () C m r
discard () = forever $ request ()

-- | Ignore all requests
ignore  :: (Monad m, Monad (p C () a () m), Interact p) => a -> p C () a () m r
ignore  _  = forever $ respond ()

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
