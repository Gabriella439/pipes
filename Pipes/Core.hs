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

    The monad transformer laws do hold when viewed through the safe API exported
    from "Pipes".
-}

{-# LANGUAGE Trustworthy #-}
{- The rewrite RULES require the 'TrustWorthy' annotation.  Their proofs are
   pretty trivial since they are just inlining the definition of their
   respective operators.  GHC doesn't do this inlining automatically for these
   functions because they are recursive.
-}

module Pipes.Core (
    -- * Types
    Proxy(..),

    -- * Operators
    request,
    (\>\),
    (>\\),
    respond,
    (/>/),
    (//>),
    pull,
    (>->),
    (->>),
    push,
    (>~>),
    (>>~),

    -- * Run Sessions 
    -- $run
    runProxy,
    runProxyK,

    -- * Safety
    observe
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))

{-| A 'Proxy' is a monad transformer that receives and sends information on two
    separate interfaces:

    The type variables signify:

    * @a'@ - The request supplied to the upstream interface

    * @a @ - The response provided by the upstream interface

    * @b'@ - The request supplied by the downstream interface

    * @b @ - The response provided to the downstream interface

    * @m @ - The base monad

    * @r @ - The return value

    Diagrammatically:

> Upstream | Downstream
>      +-------+
>      |       |
>  a' <==     <== b'
>      |   m   |
>  a  ==>     ==> b
>      |   |   |
>      +---|---+
>          v
>          r

-}
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r

instance (Monad m) => Functor (Proxy a' a b' b m) where
    fmap f p0 = go p0 where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure       r   -> Pure (f r)

instance (Monad m) => Applicative (Proxy a' a b' b m) where
    pure      = Pure
    pf <*> px = go pf where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure       f   -> fmap f px

instance (Monad m) => Monad (Proxy a' a b' b m) where
    return = Pure
    (>>=)  = _bind

_bind
    :: (Monad m)
    => Proxy a' a b' b m r
    -> (r -> Proxy a' a b' b m r')
    -> Proxy a' a b' b m r'
p0 `_bind` f = go p0 where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       r   -> f r

{-# RULES
    "_bind (Request a' k) f" forall a' k f .
        _bind (Request a' k) f = Request a' (\a  -> _bind (k a)  f);
    "_bind (Respond b  k) f" forall b  k f .
        _bind (Respond b  k) f = Respond b  (\b' -> _bind (k b') f);
    "_bind (M          m) f" forall m    f .
        _bind (M          m) f = M (m >>= \p -> return (_bind p f));
    "_bind (Pure    r   ) f" forall r    f .
        _bind (Pure       r) f = f r;
  #-}

-- | Only satisfies monad transformer laws modulo 'observe'
instance MonadTrans (Proxy a' a b' b) where
    lift m = M (m >>= \r -> return (Pure r))

instance MFunctor (Proxy a' a b' b) where
    hoist nat p0 = go (observe p0) where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (nat (m >>= \p' -> return (go p')))
            Pure       r   -> Pure r

instance (MonadIO m) => MonadIO (Proxy a' a b' b m) where
    liftIO m = M (liftIO (m >>= \r -> return (Pure r)))

request :: (Monad m) => a' -> Proxy a' a b' b m a
request a' = Request a' Pure
{-# INLINABLE request #-}

(\>\)
    :: (Monad m)
    => (b' -> Proxy a' a y' y m b)
    -> (c' -> Proxy b' b y' y m c)
    -> (c' -> Proxy a' a y' y m c)
(fb' \>\ fc') c' = fb' >\\ fc' c'
{-# INLINABLE (\>\) #-}

(>\\)
    :: (Monad m)
    => (b' -> Proxy a' a y' y m b)
    ->        Proxy b' b y' y m c
    ->        Proxy a' a y' y m c
fb' >\\ p0 = go p0
  where
    go p = case p of
        Request b' fb  -> fb' b' >>= \b -> go (fb b)
        Respond x  fx' -> Respond x (\x' -> go (fx' x'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       a   -> Pure a
{-# INLINABLE (>\\) #-}

{-# RULES
    "fb' >\\ (Request b' fb )" forall fb' b' fb  .
        fb' >\\ (Request b' fb ) = _bind (fb' b') (\b -> fb' >\\ fb b);
    "fb' >\\ (Respond x  fx')" forall fb' x  fx' .
        fb' >\\ (Respond x  fx') = Respond x (\x' -> fb' >\\ fx' x');
    "fb' >\\ (M          m  )" forall fb'    m   .
        fb' >\\ (M          m  ) = M (m >>= \p' -> return (fb' >\\ p'));
    "fb' >\\ (Pure    a     )" forall fb' a      .
        fb' >\\ (Pure    a     ) = Pure a;
  #-}

respond :: (Monad m) => b  -> Proxy a' a b' b m b'
respond b  = Respond b  Pure
{-# INLINABLE respond #-}

(/>/)
    :: (Monad m)
    => (a -> Proxy x' x b' b m a')
    -> (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x c' c m a')
(fa />/ fb) a = fa a //> fb
{-# INLINABLE (/>/) #-}

(//>)
    :: (Monad m)
    =>       Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    ->       Proxy x' x c' c m a'
p0 //> fb = go p0
  where
    go p = case p of
        Request x' fx  -> Request x' (\x -> go (fx x))
        Respond b  fb' -> fb b >>= \b' -> go (fb' b')
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       a   -> Pure a
{-# INLINABLE (//>) #-}

{-# RULES
    "(Request x' fx ) //> fb" forall x' fx  fb .
        (Request x' fx ) //> fb = Request x' (\x -> fx x //> fb);
    "(Respond b  fb') //> fb" forall b  fb' fb .
        (Respond b  fb') //> fb = _bind (fb b) (\b' -> fb' b' //> fb);
    "(M          m  ) //> fb" forall    m   fb .
        (M          m  ) //> fb = M (m >>= \p' -> return (p' //> fb));
    "(Pure    a     ) //> fb" forall a      fb .
        (Pure    a     ) //> fb = Pure a;
  #-}

pull :: (Monad m) => a' -> Proxy a' a a' a m r
pull = go
  where
    go a' = Request a' (\a -> Respond a go)
{-# INLINABLE pull #-}

(>->)
    :: (Monad m)
    => ( b' -> Proxy a' a b' b m r)
    -> (_c' -> Proxy b' b c' c m r)
    -> (_c' -> Proxy a' a c' c m r)
(fb' >-> fc') c' = fb' ->> fc' c'
{-# INLINABLE (>->) #-}

(->>)
    :: (Monad m)
    => (b' -> Proxy a' a b' b m r)
    ->        Proxy b' b c' c m r
    ->        Proxy a' a c' c m r
fb' ->> p = case p of
    Request b' fb  -> fb' b' >>~ fb
    Respond c  fc' -> Respond c (\c' -> fb' ->> fc' c')
    M          m   -> M (m >>= \p' -> return (fb' ->> p'))
    Pure       r   -> Pure r
{-# INLINABLE (->>) #-}

push :: (Monad m) => a -> Proxy a' a a' a m r
push = go
  where
    go a = Respond a (\a' -> Request a' go)
{-# INLINABLE push #-}

(>~>)
    :: (Monad m)
    => (_a -> Proxy a' a b' b m r)
    -> ( b -> Proxy b' b c' c m r)
    -> (_a -> Proxy a' a c' c m r)
(fa >~> fb) a = fa a >>~ fb
{-# INLINABLE (>~>) #-}

(>>~)
    :: (Monad m)
    =>       Proxy a' a b' b m r
    -> (b -> Proxy b' b c' c m r)
    ->       Proxy a' a c' c m r
p >>~ fb = case p of
    Request a' fa  -> Request a' (\a -> fa a >>~ fb)
    Respond b  fb' -> fb' ->> fb b
    M          m   -> M (m >>= \p' -> return (p' >>~ fb))
    Pure       r   -> Pure r
{-# INLINABLE (>>~) #-}

reflect :: (Monad m) => Proxy a' a b' b m r -> Proxy b b' a a' m r
reflect = go
  where
    go p = case p of
        Request a' fa  -> Respond a' (\a  -> go (fa  a ))
        Respond b  fb' -> Request b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       r   -> Pure r
{-# INLINABLE reflect #-}

{- $run
    The following commands run self-sufficient proxies, converting them back to
    the base monad.

    These are the only functions specific to the 'Proxy' type.  Everything
    else programs generically over the 'Proxy' type class.

    Use 'runProxyK' if you are running proxies nested within proxies.  It
    provides a Kleisli arrow as its result that you can pass to another
    'runProxy' / 'runProxyK' command.
-}

-- | Run a self-sufficient 'Proxy', converting it back to the base monad
run :: (Monad m) => Proxy a' () () b m r -> m r
run p = case p of
    Request _ fa  -> run (fa  ())
    Respond _ fb' -> run (fb' ())
    M         m   -> m >>= run
    Pure      r   -> return r

{-| Run a self-sufficient 'Proxy' Kleisli arrow, converting it back to the
    base monad
-}
runProxy :: (Monad m) => (() -> Proxy a' () () b m r) -> m r
runProxy k = run (k ())
{-# INLINABLE runProxy #-}

{-| Run a self-sufficient 'Proxy' Kleisli arrow, converting it back to a
    Kleisli arrow in the base monad
-}
runProxyK :: (Monad m) => (q -> Proxy a' () () b m r) -> (q -> m r)
runProxyK k q = run (k q)
{-# INLINABLE runProxyK #-}

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
observe :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m r
observe p0 = M (go p0) where
    go p = case p of
        M          m'  -> m' >>= go
        Pure       r   -> return (Pure r)
        Request a' fa  -> return (Request a' (\a  -> observe (fa  a )))
        Respond b  fb' -> return (Respond b  (\b' -> observe (fb' b')))
{-# INLINABLE observe #-}
