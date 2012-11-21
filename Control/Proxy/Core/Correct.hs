{-# LANGUAGE Rank2Types #-}

{-| A 'Proxy' 'request's input from upstream and 'respond's with output to
    downstream.

    For an extended tutorial, consult "Control.Proxy.Tutorial". -}

module Control.Proxy.Core.Correct (
    -- * Types
    Proxy(..),

    -- * Run Sessions 
    -- $run
    runProxy,
    runProxyK,
    runSession,
    runSessionK,

    -- * Utility Proxies
    -- $utility
    discard,
    ignore
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (ap, forever, liftM, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class
import Data.Closed (C)

{-| A 'Proxy' communicates with an upstream interface and a downstream
    interface.

    The type variables of @Proxy req_a resp_a req_b resp_b m r@ signify:

    * @req_a @ - The request supplied to the upstream interface

    * @resp_a@ - The response provided by the upstream interface

    * @req_b @ - The request supplied by the downstream interface

    * @resp_b@ - The response provided to the downstream interface

    * @m     @ - The base monad

    * @r     @ - The final return value -}
data Proxy a' a b' b m  r =
    Proxy { unProxy :: m (ProxyF a' a b' b r (Proxy a' a b' b m r)) }

-- | The base functor for the 'Proxy' type
data ProxyF a' a b' b r x
  = Request a' (a  -> x)
  | Respond b  (b' -> x)
  | Pure    r

instance (Monad m) => Functor (Proxy a' a b' b m) where
    fmap f p0 = go p0 where
        go p = Proxy (do
            x <- unProxy p
            return (case x of
                Request a' fa  -> Request a' (\a  -> go (fa  a ))
                Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
                Pure       r   -> Pure (f r) ) )

instance (Monad m) => Applicative (Proxy a' a b' b m) where
    pure r = Proxy (return (Pure r))
    pf <*> px = go pf where
        go p = Proxy (do
            x <- unProxy p
            case x of
                Request a' fa  -> return (Request a' (\a  -> go (fa  a )))
                Respond b  fb' -> return (Respond b  (\b' -> go (fb' b')))
                Pure       f   -> unProxy (fmap f px) )

instance (Monad m) => Monad (Proxy a' a b' b m) where
    return = \r -> Proxy (return (Pure r))
    p0 >>= f = go p0 where
        go p = Proxy (do
            x <- unProxy p
            case x of
                Request a' fa  -> return (Request a' (\a  -> go (fa  a )))
                Respond b  fb' -> return (Respond b  (\b' -> go (fb' b')))
                Pure       r   -> unProxy (f r) )

instance MonadTrans (Proxy a' a b' b) where
    lift = _lift

_lift :: (Monad m) => m r -> Proxy a' a b' b m r
_lift m = Proxy (m >>= \r -> return (Pure r))

{-# RULES
    "_lift m >>= f" forall m f .
        _lift m >>= f = Proxy (m >>= \r -> unProxy (f r))
  ; "_lift m ?>= f" forall m f .
        _lift m ?>= f = Proxy (m >>= \r -> unProxy (f r))
  #-}

instance (MonadIO m) => MonadIO (Proxy a' a b' b m) where
    liftIO m = Proxy (liftIO (m >>= \r -> return (Pure r)))
 -- liftIO = Proxy . liftIO . liftM Pure

instance MonadIOP Proxy where
    liftIO_P = liftIO

instance ProxyP Proxy where
    idT = \a' ->
        Proxy (return (Request a' (\a ->
        Proxy (return (Respond a idT)) )))
    k1 <-< k2_0 = \c' -> k1 c' |-< k2_0 where
        p1 |-< k2 = Proxy (do
            x <- unProxy p1
            case x of
                Request b' fb  -> unProxy (fb <-| k2 b')
                Respond c  fc' -> return (Respond c (\c' -> fc' c' |-< k2))
                Pure       r   -> return (Pure r) )
        fb <-| p2 = Proxy (do
            x <- unProxy p2
            case x of
                Request a' fa  -> return (Request a' (\a -> fb <-| fa a))
                Respond b  fb' -> unProxy (fb b |-< fb')
                Pure       r   -> return (Pure r) )

    {- For some reason, these must be defined in separate functions for the
       RULES to fire. -}
    request = _request
    respond = _respond

    return_P = return
    (?>=)   = (>>=)

    lift_P = _lift

_request :: (Monad m) => a' -> Proxy a' a b' b m a
_request = \a' -> Proxy (return (Request a' (\a  -> Proxy (return (Pure a )))))

_respond :: (Monad m) => b -> Proxy a' a b' b m b'
_respond = \b  -> Proxy (return (Respond b  (\b' -> Proxy (return (Pure b')))))

{-# RULES
    "_request a' >>= f" forall a' f .
        _request a' >>= f = Proxy (return (Request a' f))
  ; "_respond b  >>= f" forall b  f .
        _respond b  >>= f = Proxy (return (Respond b  f))
  ; "_request a' ?>= f" forall a' f .
        _request a' ?>= f = Proxy (return (Request a' f))
  ; "_respond b  ?>= f" forall b  f .
        _respond b  ?>= f = Proxy (return (Respond b  f))
  #-}

instance InteractP Proxy where
    k1 /</ k2 = \a' -> go (k1 a') where
        go p = Proxy (do
            x <- unProxy p
            case x of
                Request b' fb  -> unProxy (k2 b' >>= \b -> go (fb b))
                Respond x  fx' -> return (Respond x (\x' -> go (fx' x')))
                Pure       a   -> return (Pure a) )
    k1 \<\ k2 = \a' -> go (k2 a') where
        go p = Proxy (do
            x <- unProxy p
            case x of
                Request x' fx  -> return (Request x' (\x -> go (fx x)))
                Respond b  fb' -> unProxy (k1 b >>= \b' -> go (fb' b'))
                Pure       a   -> return (Pure a) )

instance MFunctor (Proxy a' a b' b) where
    mapT nat p0 = go p0 where
        go p = Proxy (nat (do
            x <- unProxy p
            return (case x of
                Request a' fa  -> Request a' (\a  -> go (fa  a ))
                Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
                Pure       r   -> Pure r )))

instance MFunctorP Proxy where
    mapT_P = mapT

{-| @Server req resp@ receives requests of type @req@ and sends responses of
    type @resp@.

    'Server's only 'respond' and never 'request' anything. -}
type Server req resp m r = forall a' a      . Proxy a'  a    req resp m r

{-| @Client req resp@ sends requests of type @req@ and receives responses of
    type @resp@.

    'Client's only 'request' and never 'respond' to anything. -}
type Client req resp m r = forall      b' b . Proxy req resp b'  b    m r

{-| A self-contained 'Session', ready to be run by 'runSession'

    'Session's never 'request' anything or 'respond' to anything. -}
type Session         m r = forall a' a b' b . Proxy a'  a    b'  b    m r

{- $run
    I provide two ways to run proxies:

    * 'runProxy', which discards unhandled output from either end

    * 'runSession', which type restricts its argument to ensure no loose ends

    Both functions require that the input to each end is trivially satisfiable,
    (i.e. @()@).

    I recommend 'runProxy' for most use cases since it is more convenient.

    'runSession' only accepts sessions that do not send unhandled data flying
    off each end, which provides the following benefits:

    * It prevents against accidental data loss.

    * It protects against silent failures

    * It prevents wastefully draining a scarce resource by gratuitously
      driving it to completion

    However, this restriction means that you must either duplicate every utility
    function to specialize them to the end-point positions (which I do not do),
    or explicitly close loose ends using the 'discard' and 'ignore' proxies:

> runSession $ discard <-< p <-< ignore

    Use the \'@K@\' versions of each command if you are running sessions nested
    within sessions.  They provide a Kleisli arrow as their result suitable to
    be passed to another 'runProxy' / 'runSession' command.
-}

{-| Run a self-sufficient 'Proxy' Kleisli arrow, converting it back to the base
    monad -}
runProxy :: (Monad m) => (() -> Proxy a' () () b m r) -> m r
runProxy k = go (k ()) where
    go p = do
        x <- unProxy p
        case x of
            Request _ fa  -> go (fa  ())
            Respond _ fb' -> go (fb' ())
            Pure      r   -> return r
{-| Run a self-sufficient 'Proxy' Kleisli arrow, converting it back to a Kleisli
    arrow in the base monad -}
runProxyK :: (Monad m) => (() -> Proxy a' () () b m r) -> (() -> m r)
runProxyK p = \() -> runProxy p

{-| Run a self-contained 'Session' Kleisli arrow, converting it back to the base
    monad -}
runSession :: (Monad m) => (() -> Proxy C () () C m r) -> m r
runSession = runProxy

{-| Run a self-contained 'Session' Kleisli arrow, converting it back to a
    Kleisli arrow in the base monad -}
runSessionK :: (Monad m) => (() -> Proxy C () () C m r) -> (() -> m r)
runSessionK = runProxyK

{- $utility
    'discard' provides a fallback client that gratuitously 'request's input
    from a server, but discards all responses.

    'ignore' provides a fallback server that trivially 'respond's with output
    to a client, but ignores all request parameters.
-}

-- | Discard all responses
discard :: (Monad m) => () -> Proxy () a () C m r
discard _ = go where
    go = Proxy (return (Request () (\_ -> go)))

-- | Ignore all requests
ignore  :: (Monad m) => a -> Proxy C () a () m r
ignore _ = go where
    go = Proxy (return (Respond () (\_ -> go)))
