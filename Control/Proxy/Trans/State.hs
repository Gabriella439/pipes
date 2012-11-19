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
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'State' proxy transformer
newtype StateP s p a' a b' b (m :: * -> *) r
  = StateP { unStateP :: s -> p a' a b' b m (r, s) }

instance (ProxyP            p, Monad m)
       => Functor (StateP s p a' a b' b m) where
       fmap f p = StateP (\s0 ->
           unStateP p s0 ?>= \(x, s1) ->
           return_P (f x, s1) )

{- As far as I can tell, there is no way to write this using an Applicative
   context -}
instance (ProxyP                p, Monad m)
       => Applicative (StateP s p a' a b' b m) where
    pure = return
    p1 <*> p2 = StateP (\s0 ->
        unStateP p1 s0 ?>= \(f, s1) ->
        unStateP p2 s1 ?>= \(x, s2) ->
        return_P (f x, s2) )

instance (ProxyP          p, Monad m)
       => Monad (StateP s p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (MonadPlusP            p, Monad m)
       => Alternative (StateP s p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP           p )
       => MonadPlusP (StateP s p) where
    mzero_P       = StateP (\_ -> mzero_P)
    mplus_P m1 m2 = StateP (\s -> mplus_P (unStateP m1 s) (unStateP m2 s))

instance (MonadPlusP          p, Monad m)
       => MonadPlus (StateP s p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (ProxyP               p )
       => MonadTrans (StateP s p a' a b' b) where
    lift = lift_P

instance (MonadIOP           p )
       => MonadIOP (StateP s p) where
    liftIO_P m = StateP (\s -> liftIO_P (m >>= \r -> return (r, s)))

instance (MonadIOP          p, MonadIO m)
       => MonadIO (StateP s p a' a b' b m) where
    liftIO = liftIO_P

instance (MFunctorP           p )
       => MFunctorP (StateP s p) where
    mapT_P nat p = StateP (\s -> mapT_P nat (unStateP p s))
 -- mapT nat = StateP . fmap (mapT nat) . unStateP

instance (MFunctorP          p )
       => MFunctor (StateP s p a' a b' b) where
    mapT = mapT_P

instance (ProxyP           p )
       => ProxyP (StateP s p) where
    idT = \a' -> StateP (\_ -> idT a')

    p1 >-> p2 = \c'1 -> StateP (\s ->
        ((\b' -> unStateP (p1 b') s) >-> (\c'2 -> unStateP (p2 c'2) s)) c'1 )
 {- (p1 >-> p2) = \c' -> StateP $ \s ->
        ((`unStateP` s) . p1 >-> (`unStateP` s) . p2) c' -}

    request = \a' -> StateP (\s -> request a' ?>= \a  -> return_P (a , s))
    respond = \b  -> StateP (\s -> respond b  ?>= \b' -> return_P (b', s))

    return_P = \r -> StateP (\s -> return_P (r, s))
    m ?>= f  = StateP (\s ->
        unStateP m s ?>= \(a, s') ->
        unStateP (f a) s' )

    lift_P m = StateP (\s -> lift_P (m >>= \r -> return (r, s)))

instance ProxyTrans (StateP s) where
    liftP m = StateP (\s -> m ?>= \r -> return_P (r, s))

-- | Run a 'StateP' computation, producing the final result and state
runStateP :: s -> StateP s p a' a b' b m r -> p a' a b' b m (r, s)
runStateP s m = unStateP m s

-- | Run a 'StateP' \'@K@\'leisli arrow, procuding the final result and state
runStateK :: s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m (r, s))
runStateK s k q = unStateP (k q) s
-- runStateK s = (runStateP s .)

-- | Evaluate a 'StateP' computation, but discard the final state
evalStateP
 :: (ProxyP p, Monad m) => s -> StateP s p a' a b' b m r -> p a' a b' b m r
evalStateP s p = unStateP p s ?>= \x -> return_P (fst x)
-- evalStateP s = liftM fst . runStateP s

-- | Evaluate a 'StateP' \'@K@\'leisli arrow, but discard the final state
evalStateK
 :: (ProxyP p, Monad m)
 => s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m r)
evalStateK s k q = evalStateP s (k q)
-- evalStateK s = (evalStateP s .)

-- | Evaluate a 'StateP' computation, but discard the final result
execStateP
 :: (ProxyP p, Monad m) => s -> StateP s p a' a b' b m r -> p a' a b' b m s
execStateP s p = unStateP p s ?>= \x -> return_P (snd x)
-- execStateP s = liftM snd . runStateP s

-- | Evaluate a 'StateP' \'@K@\'leisli arrow, but discard the final result
execStateK
 :: (ProxyP p, Monad m)
 => s -> (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m s)
execStateK s k q = execStateP s (k q)
-- execStateK s = (execStateP s .)

-- | Get the current state
get :: (ProxyP p, Monad m) => StateP s p a' a b' b m s
get = StateP (\s -> return_P (s, s))

-- | Set the current state
put :: (ProxyP p, Monad m) => s -> StateP s p a' a b' b m ()
put s = StateP (\_ -> return_P ((), s))

-- | Modify the current state using a function
modify :: (ProxyP p, Monad m) => (s -> s) -> StateP s p a' a b' b m ()
modify f = StateP (\s -> return_P ((), f s))

-- | Get the state filtered through a function
gets :: (ProxyP p, Monad m) => (s -> r) -> StateP s p a' a b' b m r
gets f = StateP (\s -> return_P (f s, s))
