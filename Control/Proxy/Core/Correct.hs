{-| This module provides the correct proxy implementation which strictly
    enforces the monad transformer laws.  You can safely import this module
    without violating any laws or invariants.

    However, I advise that you stick to the 'Proxy' type class API rather than
    import this module so that your code works with both 'Proxy' implementations
    and also works with all proxy transformers.
-}
module Control.Proxy.Core.Correct (
    -- * Types
    ProxyCorrect(..),
    ProxyF(..),

    -- * Run Sessions 
    -- $run
    runProxy,
    runProxyK
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (
    Proxy(request, respond, (->>), (>>~), (>\\), (//>)),
    ProxyInternal(return_P, (?>=), lift_P, liftIO_P, hoist_P, thread_P) )

{-| A 'ProxyCorrect' communicates with an upstream interface and a downstream
    interface.

    The type variables of @ProxyCorrect req_a' resp_a req_b' resp_b m r@
    signify:

    * @req_a'@ - The request supplied to the upstream interface

    * @resp_a@ - The response provided by the upstream interface

    * @req_b'@ - The request supplied by the downstream interface

    * @resp_b@ - The response provided to the downstream interface

    * @m     @ - The base monad

    * @r     @ - The final return value
-}
data ProxyCorrect a' a b' b m  r =
    Proxy { unProxy :: m (ProxyF a' a b' b r (ProxyCorrect a' a b' b m r)) }

-- | The base functor for the 'ProxyCorrect' type
data ProxyF a' a b' b r x
    = Request a' (a  -> x)
    | Respond b  (b' -> x)
    | Pure    r

instance (Monad m) => Functor (ProxyCorrect a' a b' b m) where
    fmap f p0 = go p0 where
        go p = Proxy (do
            x <- unProxy p
            return (case x of
                Request a' fa  -> Request a' (\a  -> go (fa  a ))
                Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
                Pure       r   -> Pure (f r) ) )

instance (Monad m) => Applicative (ProxyCorrect a' a b' b m) where
    pure r = Proxy (return (Pure r))
    pf <*> px = go pf where
        go p = Proxy (do
            x <- unProxy p
            case x of
                Request a' fa  -> return (Request a' (\a  -> go (fa  a )))
                Respond b  fb' -> return (Respond b  (\b' -> go (fb' b')))
                Pure       f   -> unProxy (fmap f px) )

instance (Monad m) => Monad (ProxyCorrect a' a b' b m) where
    return = \r -> Proxy (return (Pure r))
    p0 >>= f = go p0 where
        go p = Proxy (do
            x <- unProxy p
            case x of
                Request a' fa  -> return (Request a' (\a  -> go (fa  a )))
                Respond b  fb' -> return (Respond b  (\b' -> go (fb' b')))
                Pure       r   -> unProxy (f r) )

instance MonadTrans (ProxyCorrect a' a b' b) where
    lift m = Proxy (m >>= \r -> return (Pure r))

instance MFunctor (ProxyCorrect a' a b' b) where
    hoist nat p0 = go p0 where
        go p = Proxy (nat (do
            x <- unProxy p
            return (case x of
                Request a' fa  -> Request a' (\a  -> go (fa  a ))
                Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
                Pure       r   -> Pure r )))

instance (MonadIO m) => MonadIO (ProxyCorrect a' a b' b m) where
    liftIO m = Proxy (liftIO (m >>= \r -> return (Pure r)))

instance ProxyInternal ProxyCorrect where
    return_P = return
    (?>=)    = (>>=)

    lift_P   = lift

    hoist_P  = hoist

    liftIO_P = liftIO

    thread_P p s = Proxy (do
        x <- unProxy p
        return (case x of
            Request a' fa  ->
                Request (a', s) (\(a , s') -> thread_P (fa  a ) s')
            Respond b  fb' ->
                Respond (b , s) (\(b', s') -> thread_P (fb' b') s')
            Pure       r   ->
                Pure (r, s) ) )

instance Proxy ProxyCorrect where
    fb' ->> p = Proxy (do
        x <- unProxy p
        case x of
            Request b' fb  -> unProxy (fb' b' >>~ fb)
            Respond c  fc' -> return (Respond c (\c' -> fb' ->> fc' c'))
            Pure       r   -> return (Pure r) )
    p >>~ fb = Proxy (do
        x <- unProxy p
        case x of
            Request a' fa  -> return (Request a' (\a -> fa a >>~ fb))
            Respond b  fb' -> unProxy (fb' ->> fb b)
            Pure       r   -> return (Pure r) )

    request = \a' -> Proxy (return (Request a' (\a  ->
        Proxy (return (Pure a )))))
    respond = \b  -> Proxy (return (Respond b  (\b' ->
        Proxy (return (Pure b')))))

    fb' >\\ p0 = go p0
      where
        go p = Proxy (do
            y <- unProxy p
            case y of
                Request b' fb  -> unProxy (fb' b' >>= \b -> go (fb b))
                Respond x  fx' -> return (Respond x (\x' -> go (fx' x')))
                Pure       a   -> return (Pure a) )
    p0 //> fb = go p0
      where
        go p = Proxy (do
            y <- unProxy p
            case y of
                Request x' fx  -> return (Request x' (\x -> go (fx x)))
                Respond b  fb' -> unProxy (fb b >>= \b' -> go (fb' b'))
                Pure       a   -> return (Pure a) )

{- $run
    The following commands run self-sufficient proxies, converting them back to
    the base monad.

    These are the only functions specific to the 'ProxyCorrect' type.
    Everything else programs generically over the 'Proxy' type class.

    Use 'runProxyK' if you are running proxies nested within proxies.  It
    provides a Kleisli arrow as its result that you can pass to another
    'runProxy' / 'runProxyK' command.
-}

run :: (Monad m) => ProxyCorrect a' () () b m r -> m r
run p = do
    x <- unProxy p
    case x of
        Request _ fa  -> run (fa  ())
        Respond _ fb' -> run (fb' ())
        Pure      r   -> return r

{-| Run a self-sufficient 'ProxyCorrect' Kleisli arrow, converting it back to
    the base monad
-}
runProxy :: (Monad m) => (() -> ProxyCorrect a' () () b m r) -> m r
runProxy k = run (k ()) where
{-# INLINABLE runProxy #-}

{-| Run a self-sufficient 'ProxyCorrect' Kleisli arrow, converting it back to a
    Kleisli arrow in the base monad
-}
runProxyK :: (Monad m) => (q -> ProxyCorrect a' () () b m r) -> (q -> m r)
runProxyK k q = run (k q)
{-# INLINABLE runProxyK #-}
