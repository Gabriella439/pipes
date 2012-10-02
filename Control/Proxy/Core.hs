{-| A 'Proxy' 'request's input from upstream and 'respond's with output to
    downstream.

    For an extended tutorial, consult "Control.Proxy.Tutorial". -}

module Control.Proxy.Core (
    -- * Types
    -- $types
    ProxyF(..),
    Proxy(..),
    Server,
    Client,
    Session,
    -- * Run Sessions 
    -- $run
    runSession,
    -- * Utility functions
    -- $utility
    discard,
    ignore,
    foreverK
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (ap, forever, liftM, (>=>))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Free
import Control.MFunctor
import Control.Proxy.Class
import Data.Void (Void)

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
    return = Proxy . return
    m >>= f = Proxy $ unProxy m >>= unProxy . f

instance MonadTrans (Proxy a' a b' b) where
    lift = Proxy . lift

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
        Pure           r   -> return r
        Free (Respond c  fc') -> wrap $ Respond c (fc' <-<? p2)
        Free (Request b' fb ) -> FreeT $ do
            x2 <- runFreeT $ p2 b'
            runFreeT $ case x2 of
                Pure           r   -> return r
                Free (Respond b  fb') -> ((\_ -> fb b) <-<? fb') c'
                Free (Request a' fa ) -> do
                    let p1' = \_ -> FreeT $ return x1
                    wrap $ Request a' $ \a -> (p1' <-<? (\_ -> fa a)) c'

instance Request Proxy where
    request a' = Proxy $ liftF $ Request a' id
    p1 /</ p2 = (Proxy .) $ (unProxy . p1) /</? (unProxy . p2)

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

instance Respond Proxy where
    respond a = Proxy $ liftF $ Respond a id
    p1 />/ p2 = (Proxy .) $ (unProxy . p1) />/? (unProxy . p2)

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
type Server req resp = Proxy Void   () req resp

{-| @Client req resp@ sends requests of type @req@ and receives responses of
    type @resp@.

    'Client's only 'request' and never 'respond' to anything. -}
type Client req resp = Proxy  req resp () Void

{-| A self-contained 'Session', ready to be run by 'runSession'

    'Session's never 'request' anything or 'respond' to anything. -}
type Session         = Proxy Void   ()  () Void

{- $run
    'runSession' ensures that the 'Proxy' passed to it does not have any
    open responses or requests.  This restriction makes 'runSession' less
    polymorphic than it could be, and I settled on this restriction for four
    reasons:

    * It prevents against accidental data loss.

    * It protects against silent failures

    * It prevents wastefully draining a scarce resource by gratuitously
      driving it to completion

    * It encourages an idiomatic programming style where unfulfilled requests
      or responses are satisfied in a structured way using composition.

    If you believe that loose requests or responses should be discarded or
    ignored, then you can explicitly ignore them by using 'discard' (which
    discards all responses), and 'ignore' (which ignores all requests):

> runSession $ discard <-< p <-< ignore
-}
-- | Run a self-contained 'Session', converting it back to the base monad
runSession :: (Monad m) => (() -> Session m r) -> m r
runSession p = runSession' $ unProxy $ p ()

runSession' p = do
    x <- runFreeT p
    case x of
        Pure            r    -> return r
        Free (Respond _ fb ) -> runSession' $ fb  ()
        Free (Request _ fa') -> runSession' $ fa' ()

{- $utility
    'discard' provides a fallback 'Client' that gratuitously 'request's input
    from a 'Server', but discards all responses.

    'ignore' provides a fallback 'Server' that trivially 'respond's with output
    to a 'Client', but ignores all request parameters.

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
discard :: (Monad m) => () -> Client () a m r
discard () = forever $ request ()

-- | Ignore all requests
ignore  :: (Monad m) => a -> Server a () m r
ignore  _  = forever $ respond ()

-- | Compose a \'K\'leisli arrow with itself forever
foreverK :: (Monad m) => (a -> m a) -> (a -> m b)
foreverK k = let r = k >=> r in r
{- foreverK uses 'let' to avoid a space leak.
   See: http://hackage.haskell.org/trac/ghc/ticket/5205 -}
