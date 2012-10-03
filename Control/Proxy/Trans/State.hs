-- | This module provides the proxy transformer equivalent of 'StateT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.State (
    -- * StateP
    StateP(..),
    -- * State operations
    get,
    put,
    modify,
    gets
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT    , (>->)), 
    Request(request, (\>\)), 
    Respond(respond, (/>/)))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'State' proxy transformer
newtype StateP s p a' a b' b (m :: * -> *) r
  = StateP { runStateP :: s -> p a' a b' b m (r, s) }

instance (Monad (p a' a b' b m)) => Functor (StateP s p a' a b' b m) where
    fmap = liftM

instance (Monad (p a' a b' b m)) => Applicative (StateP s p a' a b' b m) where
    pure  = return
    (<*>) = ap

instance (Monad (p a' a b' b m)) => Monad (StateP s p a' a b' b m) where
    return a = StateP $ \s -> return (a, s)
    m >>= f = StateP $ \s -> do
        (a, s') <- runStateP m s
        runStateP (f a) s'

instance (MonadTrans (p a' a b' b)) => MonadTrans (StateP s p a' a b' b) where
    lift m = StateP $ \s -> lift $ liftM (\r -> (r, s)) m

instance (MFunctor (p a' a b' b)) => MFunctor (StateP s p a' a b' b) where
    mapT nat = StateP . fmap (mapT nat) . runStateP

instance (Channel p) => Channel (StateP s p) where
    idT a = StateP $ \s -> idT a
    (p1 >-> p2) a = StateP $ \s ->
        ((`runStateP` s) . p1 >-> (`runStateP` s) . p2) a

instance ProxyTrans (StateP s) where
    liftP m = StateP $ \s ->  liftM (\r -> (r, s)) m

-- | Get the current state
get :: (Monad (p a' a b' b m)) => StateP s p a' a b' b m s
get = StateP $ \s -> return (s, s)

-- | Set the current state
put :: (Monad (p a' a b' b m)) => s -> StateP s p a' a b' b m ()
put s = StateP $ \_ -> return ((), s)

-- | Modify the current state using a function
modify :: (Monad (p a' a b' b m)) => (s -> s) -> StateP s p a' a b' b m ()
modify f = StateP $ \s -> return ((), f s)

-- | Get the state filtered through a function
gets :: (Monad (p a' a b' b m)) => (s -> r) -> StateP s p a' a b' b m r
gets f = StateP $ \s -> return (f s, s)
