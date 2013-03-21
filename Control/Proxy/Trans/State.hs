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
import Control.Proxy.Class
import Control.Proxy.Morph (PFunctor(hoistP))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'State' proxy transformer
newtype StateP s p a' a b' b (m :: * -> *) r
  = StateP { unStateP :: s -> p a' a b' b m (r, s) }

instance (Proxy p, Monad m) => Functor (StateP s p a' a b' b m) where
       fmap f p = StateP (\s0 ->
           unStateP p s0 ?>= \(x, s1) ->
           return_P (f x, s1) )

instance (Proxy p, Monad m) => Applicative (StateP s p a' a b' b m) where
    pure      = return
    p1 <*> p2 = StateP (\s0 ->
        unStateP p1 s0 ?>= \(f, s1) ->
        unStateP p2 s1 ?>= \(x, s2) ->
        return_P (f x, s2) )

instance (Proxy p, Monad m) => Monad (StateP s p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (Proxy p) => MonadTrans (StateP s p a' a b' b) where
    lift = lift_P

instance (Proxy p) => MFunctor (StateP s p a' a b' b) where
    hoist = hoist_P

instance (Proxy p, MonadIO m) => MonadIO (StateP s p a' a b' b m) where
    liftIO = liftIO_P

instance (MonadPlusP p, Monad m) => Alternative (StateP s p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP p, Monad m) => MonadPlus (StateP s p a' a b' b m) where
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

instance (Proxy p) => Proxy (StateP s p) where
    fb' ->> p = StateP (\s -> (\b' -> unStateP (fb' b') s) ->> unStateP p s)
    p >>~ fb  = StateP (\s -> unStateP p s >>~ (\b -> unStateP (fb b) s))
    request = \a' -> StateP (\s -> request a' ?>= \a  -> return_P (a , s))
    respond = \b  -> StateP (\s -> respond b  ?>= \b' -> return_P (b', s))

instance (MonadPlusP p) => MonadPlusP (StateP s p) where
    mzero_P       = StateP (\_ -> mzero_P)
    mplus_P m1 m2 = StateP (\s -> mplus_P (unStateP m1 s) (unStateP m2 s))

instance ProxyTrans (StateP s) where
    liftP m = StateP (\s -> m ?>= \r -> return_P (r, s))

instance PFunctor (StateP s) where
    hoistP nat p = StateP (\s -> nat (unStateP p s))

-- | Run a 'StateP' computation, producing the final result and state
runStateP :: s -> StateP s p a' a b' b m r -> p a' a b' b m (r, s)
runStateP s m = unStateP m s

-- | Run a 'StateP' \'@K@\'leisli arrow, procuding the final result and state
runStateK :: s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m (r, s))
runStateK s k q = unStateP (k q) s

-- | Evaluate a 'StateP' computation, but discard the final state
evalStateP
 :: (Proxy p, Monad m) => s -> StateP s p a' a b' b m r -> p a' a b' b m r
evalStateP s p = unStateP p s ?>= \x -> return_P (fst x)

-- | Evaluate a 'StateP' \'@K@\'leisli arrow, but discard the final state
evalStateK
 :: (Proxy p, Monad m)
 => s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m r)
evalStateK s k q = evalStateP s (k q)

-- | Evaluate a 'StateP' computation, but discard the final result
execStateP
 :: (Proxy p, Monad m) => s -> StateP s p a' a b' b m r -> p a' a b' b m s
execStateP s p = unStateP p s ?>= \x -> return_P (snd x)

-- | Evaluate a 'StateP' \'@K@\'leisli arrow, but discard the final result
execStateK
 :: (Proxy p, Monad m)
 => s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m s)
execStateK s k q = execStateP s (k q)

-- | Get the current state
get :: (Proxy p, Monad m) => StateP s p a' a b' b m s
get = StateP (\s -> return_P (s, s))

-- | Set the current state
put :: (Proxy p, Monad m) => s -> StateP s p a' a b' b m ()
put s = StateP (\_ -> return_P ((), s))

-- | Modify the current state using a function
modify :: (Proxy p, Monad m) => (s -> s) -> StateP s p a' a b' b m ()
modify f = StateP (\s -> return_P ((), f s))

-- | Get the state filtered through a function
gets :: (Proxy p, Monad m) => (s -> r) -> StateP s p a' a b' b m r
gets f = StateP (\s -> return_P (f s, s))
