{-| This is an internal module, meaning that it is unsafe to import unless you
    understand the risks.

    This module provides the fast proxy implementation, which achieves its speed
    by weakening the monad transformer laws.  These laws do not hold if you can
    pattern match on the constructors, as the following counter-example
    illustrates:

> lift . return = M . return . Pure
>
> return = Pure
>
> lift . return /= return

    These laws only hold when viewed through certain safe observation functions,
    like 'runProxy' and 'observe'.

    Also, you really should not use the constructors anyway, let alone the
    concrete type and instead you should stick to the 'Proxy' type class API.
    This not only ensures that your code does not violate the monad transformer
    laws, but also guarantees that it works with the other proxy implementations
    and with any proxy transformers. -}

{-# LANGUAGE Trustworthy #-}
{- The rewrite RULES require the 'TrustWorthy' annotation.  I've supplied the
   correctness proof for each rewrite rule immediately below each rule. -}

module Control.Proxy.Core.Fast (
    -- * Types
    ProxyFast(..),

    -- * Run Sessions 
    -- $run
    runProxy,
    runProxyK,
    runPipe,

    -- * Safety
    observe
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(hoist))
import Control.Proxy.Class
import Control.Proxy.Synonym (C)

{-| A 'ProxyFast' communicates with an upstream interface and a downstream
    interface.

    The type variables of @ProxyFast req_a' resp_a req_b' resp_b m r@ signify:

    * @req_a'@ - The request supplied to the upstream interface

    * @resp_a@ - The response provided by the upstream interface

    * @req_b'@ - The request supplied by the downstream interface

    * @resp_b@ - The response provided to the downstream interface

    * @m     @ - The base monad

    * @r     @ - The final return value -}
data ProxyFast a' a b' b m r
  = Request a' (a  -> ProxyFast a' a b' b m r )
  | Respond b  (b' -> ProxyFast a' a b' b m r )
  | M          (m    (ProxyFast a' a b' b m r))
  | Pure    r

instance (Monad m) => Functor (ProxyFast a' a b' b m) where
    fmap f p0 = go p0 where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure       r   -> Pure (f r)

instance (Monad m) => Applicative (ProxyFast a' a b' b m) where
    pure      = Pure
    pf <*> px = go pf where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure       f   -> fmap f px

instance (Monad m) => Monad (ProxyFast a' a b' b m) where
    return = Pure
    (>>=)  = _bind

_bind
 :: (Monad m)
 => ProxyFast a' a b' b m r
 -> (r -> ProxyFast a' a b' b m r')
 -> ProxyFast a' a b' b m r'
p0 `_bind` f = go p0 where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       r   -> f r

{-# RULES
    "_bind (Request a' Pure) f" forall a' f .
        _bind (Request a' Pure) f = Request a' f;
    "_bind (Respond b  Pure) f" forall b  f .
        _bind (Respond b  Pure) f = Respond b  f
  #-}
{- Proof that the rewrite rules are Trustworthy:

  _bind (Request a' Pure) f
= go (Request a' Pure) where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       r   -> f r
= Request a' (\a -> go (Pure a))
= Request a' (\a -> f a)
= Request a' f

  _bind (Respond b Pure) f
= go (Respond b Pure) where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       r   -> f r
= Respond b (\b' -> go (Pure b'))
= Respond b (\b' -> f b')
= Respond b f
-}

instance MonadP ProxyFast where
    return_P = return
    (?>=)   = _bind

-- | Only satisfies laws modulo 'observe'
instance MonadTrans (ProxyFast a' a b' b) where
    lift = _lift

_lift :: (Monad m) => m r -> ProxyFast a' a b' b m r
_lift m = M (m >>= \r -> return (Pure r))
-- _lift = M . liftM Pure

{- These never fire, for some reason, but keep them until I figure out how to
   get them to work. -}
{-# RULES
    "_lift m ?>= f" forall m f .
        _bind (_lift m) f = M (m >>= \r -> return (f r))
  #-}
{- Proof that the rewrite rule is Trustworthy:

  _bind (_lift m) f
= _bind (M (m >>= \r -> return (Pure r))) f
= go (M (m >>= \r -> return (Pure r))) where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       r   -> f r
= M ((m >>= \r -> return (Pure r)) >>= \p' -> return (go p'))
= M (m >>= \r -> (return (Pure r) >>= \p' -> return (go p')))
= M (m >>= \r -> return (go (Pure r)))
= M (m >>= \r -> return (f r))
-}

instance (MonadIO m) => MonadIO (ProxyFast a' a b' b m) where
    liftIO m = M (liftIO (m >>= \r -> return (Pure r)))
 -- liftIO = M . liftIO . liftM Pure

instance MonadIOP ProxyFast where
    liftIO_P = liftIO

instance Proxy ProxyFast where
    fb'_0 >-> fc'_0 = \c' -> fb'_0 >-| fc'_0 c' where
        p1 |-> fb = case p1 of
            Request a' fa  -> Request a' (\a -> fa a |-> fb)
            Respond b  fb' -> fb' >-| fb b
            M          m   -> M (m >>= \p1' -> return (p1' |-> fb))
            Pure       r   -> Pure r
        fb' >-| p2 = case p2 of
            Request b' fb  -> fb' b' |-> fb
            Respond c  fc' -> Respond c (\c' -> fb' >-| fc' c')
            M          m   -> M (m >>= \p2' -> return (fb' >-| p2'))
            Pure       r   -> Pure r

    fa_0 >~> fb_0 = \a -> fa_0 a |-> fb_0 where
        p1 |-> fb = case p1 of
            Request a' fa  -> Request a' (\a -> fa a |-> fb)
            Respond b  fb' -> fb' >-| fb b
            M          m   -> M (m >>= \p1' -> return (p1' |-> fb))
            Pure       r   -> Pure r
        fb' >-| p2 = case p2 of
            Request b' fb  -> fb' b' |-> fb
            Respond c  fc' -> Respond c (\c' -> fb' >-| fc' c')
            M          m   -> M (m >>= \p2' -> return (fb' >-| p2'))
            Pure       r   -> Pure r

    request a' = Request a' Pure
    respond b  = Respond b  Pure

    lift_P = _lift

    hoist_P = hoist

instance Interact ProxyFast where
    k2 \>\ k1 = \a' -> go (k1 a') where
        go p = case p of
            Request b' fb  -> k2 b' >>= \b -> go (fb b)
            Respond x  fx' -> Respond x (\x' -> go (fx' x'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure       a   -> Pure a
    k2 />/ k1 = \a' -> go (k2 a') where
        go p = case p of
            Request x' fx  -> Request x' (\x -> go (fx x))
            Respond b  fb' -> k1 b >>= \b' -> go (fb' b')
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure       a   -> Pure a

instance MFunctor (ProxyFast a' a b' b) where
    hoist nat p0 = go (observe p0) where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (nat (m >>= \p' -> return (go p')))
            Pure       r   -> Pure r

{- $run
    The following commands run self-sufficient proxies, converting them back to
    the base monad.

    These are the only functions specific to the 'ProxyFast' type.  Everything
    else programs generically over the 'Proxy' type class.

    Use 'runProxyK' if you are running proxies nested within proxies.  It
    provides a Kleisli arrow as its result that you can pass to another
    'runProxy' / 'runProxyK' command. -}

{-| Run a self-sufficient 'ProxyFast' Kleisli arrow, converting it back to the
    base monad -}
runProxy :: (Monad m) => (() -> ProxyFast a' () () b m r) -> m r
runProxy k = go (k ()) where
    go p = case p of
        Request _ fa  -> go (fa  ())
        Respond _ fb' -> go (fb' ())
        M         m   -> m >>= go
        Pure      r   -> return r

{-| Run a self-sufficient 'ProxyFast' Kleisli arrow, converting it back to a
    Kleisli arrow in the base monad -}
runProxyK :: (Monad m) => (() -> ProxyFast a' () () b m r) -> (() -> m r)
runProxyK p = \() -> runProxy p

-- | Run the 'Pipe' monad transformer, converting it back to the base monad
runPipe :: (Monad m) => ProxyFast a' () () b m r -> m r
runPipe p = runProxy (\_ -> p)

{-| The monad transformer laws are correct when viewed through the 'observe'
    function:

> observe (lift (return r)) = observe (return r)
>
> observe (lift (m >>= f)) = observe (lift m >>= lift . f)

    This correctness comes at a moderate cost to performance, so use this
    function sparingly or else you would be better off using
    "Control.Proxy.Core.Correct".

    You do not need to use this function if you use the safe API exported from
    "Control.Proxy", which does not export any functions or constructors that
    can violate the monad transformer laws.
-}
observe :: (Monad m) => ProxyFast a' a b' b m r -> ProxyFast a' a b' b m r
observe p = M (go p) where
    go p = case p of
        M          m'  -> m' >>= go
        Pure       r   -> return (Pure r)
        Request a' fa  -> return (Request a' (\a  -> observe (fa  a )))
        Respond b  fb' -> return (Respond b  (\b' -> observe (fb' b')))
