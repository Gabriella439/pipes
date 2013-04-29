-- | This module provides the proxy transformer equivalent of 'StateT'.

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Trans.State (
    -- * StateP
    StateP(..),
    runStateP,
    runStateK,
    evalStateP,
    evalStateK,
    execStateP,
    execStateK,

    -- * State operations
    state,
    stateT,
    get,
    put,
    modify,
    gets
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (
    Proxy(request, respond, (->>), (>>~), (>\\), (//>), turn),
    ProxyInternal(return_P, (?>=), lift_P, liftIO_P, hoist_P, thread_P),
    MonadPlusP(mzero_P, mplus_P) )
import Control.Proxy.Morph (PFunctor(hoistP))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'State' proxy transformer
newtype StateP s p a' a b' b (m :: * -> *) r
    = StateP { unStateP :: s -> p (a', s) (a, s) (b', s) (b, s) m (r, s) }

instance (Monad m, Proxy p) => Functor (StateP s p a' a b' b m) where
       fmap f p = StateP (\s0 ->
           unStateP p s0 ?>= \(x, s1) ->
           return_P (f x, s1) )

instance (Monad m, Proxy p) => Applicative (StateP s p a' a b' b m) where
    pure      = return
    p1 <*> p2 = StateP (\s0 ->
        unStateP p1 s0 ?>= \(f, s1) ->
        unStateP p2 s1 ?>= \(x, s2) ->
        return_P (f x, s2) )

instance (Monad m, Proxy p) => Monad (StateP s p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (Proxy p) => MonadTrans (StateP s p a' a b' b) where
    lift = lift_P

instance (Proxy p) => MFunctor (StateP s p a' a b' b) where
    hoist = hoist_P

instance (MonadIO m, Proxy p) => MonadIO (StateP s p a' a b' b m) where
    liftIO = liftIO_P

instance (Monad m, MonadPlusP p) => Alternative (StateP s p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, MonadPlusP p) => MonadPlus (StateP s p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy p) => ProxyInternal (StateP s p) where
    return_P = \r -> StateP (\s -> return_P (r, s))
    m ?>= f  = StateP (\s ->
        unStateP m s ?>= \(a, s') ->
        unStateP (f a) s' )

    lift_P m = StateP (\s -> lift_P (m >>= \r -> return (r, s)))

    hoist_P nat p = StateP (\s -> hoist_P nat (unStateP p s))

    liftIO_P m = StateP (\s -> liftIO_P (m >>= \r -> return (r, s)))

    thread_P p s = StateP (\s' ->
        ((up ->> thread_P (unStateP p s') s) >>~ dn) ?>= next )
      where
        up ((a', s1), s2) =
            request ((a', s2 ), s1 ) ?>= \((a , s1'), s2') ->
            respond ((a , s2'), s1') ?>= up
        dn ((b , s1), s2) =
            respond ((b , s2 ), s1 ) ?>= \((b', s1'), s2') ->
            request ((b', s2'), s1') ?>= dn
        next ((r, s1), s2) = return_P ((r, s2), s1)

instance (Proxy p) => Proxy (StateP s p) where
    fb' ->> p = StateP (\s ->
        (\(b', s') -> unStateP (fb' b') s') ->> unStateP p s)
    p >>~ fb  = StateP (\s ->
        unStateP p s >>~ (\(b, s') -> unStateP (fb b) s') )
    request = \a' -> StateP (\s -> request (a', s))
    respond = \b  -> StateP (\s -> respond (b , s))

    fb' >\\ p = StateP (\s ->
        (\(b', s') -> unStateP (fb' b') s') >\\ unStateP p s)
    p //> fb  = StateP (\s ->
        unStateP p s //> (\(b, s') -> unStateP (fb b) s') )

    turn p = StateP (\s -> turn (unStateP p s))

instance (MonadPlusP p) => MonadPlusP (StateP s p) where
    mzero_P       = StateP (\_ -> mzero_P)
    mplus_P m1 m2 = StateP (\s -> mplus_P (unStateP m1 s) (unStateP m2 s))

instance ProxyTrans (StateP s) where
    liftP m = StateP (thread_P m)

instance PFunctor (StateP s) where
    hoistP nat p = StateP (\s -> nat (unStateP p s))

-- | Run a 'StateP' computation, producing the final result and state
runStateP
    :: (Monad m, Proxy p)
    => s -> StateP s p a' a b' b m r -> p a' a b' b m (r, s)
runStateP s0 m = up >\\ unStateP m s0 //> dn
  where
    up (a', s) =
        request a' ?>= \a  ->
        return_P (a , s)
    dn (b , s) =
        respond b  ?>= \b' ->
        return_P (b', s)
{-# INLINABLE runStateP #-}

-- | Run a 'StateP' \'@K@\'leisli arrow, procuding the final result and state
runStateK
    :: (Monad m, Proxy p)
    => s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m (r, s))
runStateK s k q = runStateP s (k q)
{-# INLINABLE runStateK #-}

-- | Evaluate a 'StateP' computation, but discard the final state
evalStateP
    :: (Monad m, Proxy p) => s -> StateP s p a' a b' b m r -> p a' a b' b m r
evalStateP s p = runStateP s p ?>= \(r, _) -> return_P r
{-# INLINABLE evalStateP #-}

-- | Evaluate a 'StateP' \'@K@\'leisli arrow, but discard the final state
evalStateK
    :: (Monad m, Proxy p)
    => s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m r)
evalStateK s k q = evalStateP s (k q)
{-# INLINABLE evalStateK #-}

-- | Evaluate a 'StateP' computation, but discard the final result
execStateP
    :: (Monad m, Proxy p) => s -> StateP s p a' a b' b m r -> p a' a b' b m s
execStateP s p = runStateP s p ?>= \(_, s') -> return_P s'
{-# INLINABLE execStateP #-}

-- | Evaluate a 'StateP' \'@K@\'leisli arrow, but discard the final result
execStateK
    :: (Monad m, Proxy p)
    => s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m s)
execStateK s k q = execStateP s (k q)
{-# INLINABLE execStateK #-}

-- | Convert a State to a 'StateP'
state :: (Monad m, Proxy p) => (s -> (r, s)) -> StateP s p a' a b' b m r
state f = StateP (\s -> return_P (f s))

-- | Convert a StateT to a 'StateP'
stateT :: (Monad m, Proxy p) => (s -> m (r, s)) -> StateP s p a' a b' b m r
stateT f = StateP (\s -> lift_P (f s))

-- | Get the current state
get :: (Monad m, Proxy p) => StateP s p a' a b' b m s
get = StateP (\s -> return_P (s, s))
{-# INLINABLE get #-}

-- | Set the current state
put :: (Monad m, Proxy p) => s -> StateP s p a' a b' b m ()
put s = StateP (\_ -> return_P ((), s))
{-# INLINABLE put #-}

-- | Modify the current state using a function
modify :: (Monad m, Proxy p) => (s -> s) -> StateP s p a' a b' b m ()
modify f = StateP (\s -> return_P ((), f s))
{-# INLINABLE modify #-}

-- | Get the state filtered through a function
gets :: (Monad m, Proxy p) => (s -> r) -> StateP s p a' a b' b m r
gets f = StateP (\s -> return_P (f s, s))
{-# INLINABLE gets #-}
