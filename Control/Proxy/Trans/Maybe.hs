-- | This module provides the proxy transformer equivalent of 'MaybeT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Maybe (
    -- * MaybeP
    MaybeP(..),
    -- * Maybe operations
    nothing,
    just
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT    , (>->)), 
    Request(request, (\>\)), 
    Respond(respond, (/>/)))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Maybe' proxy transformer
newtype MaybeP p a' a b' b (m :: * -> *) r
  = MaybeP { runMaybeP :: p a' a b' b m (Maybe r) }

instance (Monad (p a' a b' b m)) => Functor (MaybeP p a' a b' b m) where
    fmap = liftM

instance (Monad (p a' a b' b m)) => Applicative (MaybeP p a' a b' b m) where
    pure  = return
    (<*>) = ap

instance (Monad (p a' a b' b m)) => Monad (MaybeP p a' a b' b m) where
    return = MaybeP . return . Just
    m >>= f = MaybeP $ do
        ma <- runMaybeP m
        runMaybeP $ case ma of
            Nothing -> nothing
            Just a  -> f a

instance (Monad (p a' a b' b m)) => MonadPlus (MaybeP p a' a b' b m) where
    mzero = nothing
    mplus m1 m2 = MaybeP $ do
        ma <- runMaybeP m1
        runMaybeP $ case ma of
            Nothing -> m2
            Just a  -> just a

instance (Monad (p a' a b' b m)) => Alternative (MaybeP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadTrans (p a' a b' b)) => MonadTrans (MaybeP p a' a b' b) where
    lift = MaybeP . lift . liftM Just

instance (MFunctor (p a' a b' b)) => MFunctor (MaybeP p a' a b' b) where
    mapT nat = MaybeP . mapT nat . runMaybeP

instance (Channel p) => Channel (MaybeP p) where
    idT = MaybeP . idT
    p1 >-> p2 = (MaybeP .) $ runMaybeP . p1 >-> runMaybeP . p2

instance ProxyTrans MaybeP where
    liftP = MaybeP . liftM Just

-- | A synonym for 'mzero'
nothing :: (Monad (p a' a b' b m)) => MaybeP p a' a b' b m r
nothing = MaybeP $ return Nothing

-- | A synonym for 'return'
just :: (Monad (p a' a b' b m)) => r -> MaybeP p a' a b' b m r
just = return
