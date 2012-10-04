{-| A 'Proxy' 'request's input from upstream and 'respond's with output to
    downstream.

    For an extended tutorial, consult "Control.Proxy.Tutorial". -}

module Control.Proxy.Core (
    -- * Types
    -- $types
    ProxyF(..),
    Proxy(..),
    C,
    Server,
    Client,
    Session,
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
import Control.Monad.Trans.Free (
    FreeF(Free, Pure), FreeT(FreeT, runFreeT), liftF, hoistFreeT, wrap )
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT, (<-<)), Interact(request, (/</), respond, (/>/)) )
import Data.Closed (C)

{- $types
    A 'Proxy' communicates with an upstream interface and a downstream
    interface.

    The type variables of @Proxy req_a resp_a req_b resp_b m r@ signify:

    * @req_a @ - The request supplied to the upstream interface

    * @resp_a@ - The response provided by the upstream interface

    * @req_b @ - The request supplied by the downstream interface

    * @resp_b@ - The response provided to the downstream interface

    * @m     @ - The base monad

    * @r     @ - The final return value -}

-- | The base functor for the 'Proxy' type
data ProxyF a' a b' b x = Request a' (a -> x) | Respond b (b' -> x)

instance Functor (ProxyF a' a b' b) where
    fmap f (Respond b  fb') = Respond b  (f . fb')
    fmap f (Request a' fa ) = Request a' (f . fa )

-- | A 'Proxy' converts one interface to another
newtype Proxy a' a b' b m r = Proxy { unProxy :: FreeT (ProxyF a' a b' b) m r }

instance (Monad m) => Functor (Proxy a' a b' b m) where
    fmap = liftM

instance (Monad m) => Applicative (Proxy a' a b' b m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (Proxy a' a b' b m) where
    return  = Proxy . return
    m >>= f = Proxy $ unProxy m >>= unProxy . f

instance MonadTrans (Proxy a' a b' b) where
    lift = Proxy . lift

instance (MonadIO m) => MonadIO (Proxy a' a b' b m) where
    liftIO = Proxy . liftIO

instance Channel Proxy where
    idT       = Proxy . idT'
    p1 <-< p2 = Proxy . ((unProxy . p1) <-<? (unProxy . p2))

idT' :: (Monad m) => a' -> FreeT (ProxyF a' a a' a) m r
idT' a' = wrap $ Request a' $ \a -> wrap $ Respond a idT'

(<-<?) :: (Monad m)
 => (c' -> FreeT (ProxyF b' b c' c) m r)
 -> (b' -> FreeT (ProxyF a' a b' b) m r)
 -> (c' -> FreeT (ProxyF a' a c' c) m r)
p1 <-<? p2 = \c' -> FreeT $ do
    x1 <- runFreeT $ p1 c'
    runFreeT $ case x1 of
        Pure             r    -> return r
        Free (Respond c  fc') -> wrap $ Respond c (fc' <-<? p2)
        Free (Request b' fb ) -> FreeT $ do
            x2 <- runFreeT $ p2 b'
            runFreeT $ case x2 of
                Pure             r    -> return r
                Free (Respond b  fb') -> ((\_ -> fb b) <-<? fb') c'
                Free (Request a' fa ) -> do
                    let p1' = \_ -> FreeT $ return x1
                    wrap $ Request a' $ \a -> (p1' <-<? (\_ -> fa a)) c'

instance Interact Proxy where
    request a' = Proxy $ liftF $ Request a' id
    p1 /</ p2 = (Proxy .) $ (unProxy . p1) /</? (unProxy . p2)
    respond a = Proxy $ liftF $ Respond a id
    p1 />/ p2 = (Proxy .) $ (unProxy . p1) />/? (unProxy . p2)

(/</?)
 :: (Monad m)
 => (c' -> FreeT (ProxyF b' b x' x) m c)
 -> (b' -> FreeT (ProxyF a' a x' x) m b)
 -> (c' -> FreeT (ProxyF a' a x' x) m c)
f1 /</? f2 = \a' -> FreeT $ do
    x1 <- runFreeT $ f1 a'
    runFreeT $ case x1 of
        Pure a                -> return a
        Free (Respond x  fx') -> wrap $ Respond x $ fx' /</? f2
        Free (Request b' fb ) -> (f2 >=> (fb /</? f2)) b'

(/>/?)
 :: (Monad m)
 => (a -> FreeT (ProxyF x' x b' b) m a')
 -> (b -> FreeT (ProxyF x' x c' c) m b')
 -> (a -> FreeT (ProxyF x' x c' c) m a')
f1 />/? f2 = \a' -> FreeT $ do
    x1 <- runFreeT $ f1 a'
    runFreeT $ case x1 of
        Pure a'               -> return a'
        Free (Respond b  fb') -> (f2 >=> (fb' />/? f2)) b
        Free (Request x' fx ) -> wrap $ Request x' $ fx />/? f2

instance MFunctor (Proxy a' a b' b) where
    mapT nat = Proxy . hoistFreeT nat . unProxy

{-| @Server req resp@ receives requests of type @req@ and sends responses of
    type @resp@.

    'Server's only 'respond' and never 'request' anything. -}
type Server req resp = Proxy C   ()   req resp

{-| @Client req resp@ sends requests of type @req@ and receives responses of
    type @resp@.

    'Client's only 'request' and never 'respond' to anything. -}
type Client req resp = Proxy req resp ()  C

{-| A self-contained 'Session', ready to be run by 'runSession'

    'Session's never 'request' anything or 'respond' to anything. -}
type Session         = Proxy C   ()   ()  C

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
    be passed to another 'runProxy'/'runSession' command.
-}

{-| Run a self-sufficient 'Proxy' Kleisli arrow, converting it back to the base
    monad -}
runProxy :: (Monad m) => (() -> Proxy a () () b m r) -> m r
runProxy p = runProxyK p ()

{-| Run a self-sufficient 'Proxy' Kleisli arrow, converting it back to a Kleisli
    arrow in the base monad -}
runProxyK :: (Monad m) => (() -> Proxy a () () b m r) -> (() -> m r)
runProxyK p = runProxy' . unProxy . p

runProxy' :: (Monad m) => FreeT (ProxyF a () () b) m r -> m r
runProxy' p = do
    x <- runFreeT p
    case x of
        Pure            r    -> return r
        Free (Respond _ fb ) -> runProxy' $ fb  ()
        Free (Request _ fa') -> runProxy' $ fa' ()

{-| Run a self-contained 'Session' Kleisli arrow, converting it back to the base
    monad -}
runSession :: (Monad m) => (() -> Session m r) -> m r
runSession = runProxy

{-| Run a self-contained 'Session' Kleisli arrow, converting it back to a
    Kleisli arrow in the base monad -}
runSessionK :: (Monad m) => (() -> Session m r) -> (() -> m r)
runSessionK = runProxyK

{- $utility
    'discard' provides a fallback client that gratuitously 'request's input
    from a server, but discards all responses.

    'ignore' provides a fallback server that trivially 'respond's with output
    to a client, but ignores all request parameters.
-}

-- | Discard all responses
discard :: (Monad m, Monad (p () a () C m), Interact p) => () -> p () a () C m r
discard () = forever $ request ()

-- | Ignore all requests
ignore  :: (Monad m, Monad (p C () a () m), Interact p) => a -> p C () a () m r
ignore  _  = forever $ respond ()
