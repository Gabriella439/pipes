-- | This module provides the proxy transformer equivalent of 'MaybeT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Maybe (
    -- * MaybeP
    MaybeP(..)
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

instance (MonadTrans (p a' a b' b)) => MonadTrans (MaybeP p a' a b' b) where
    lift = MaybeP . lift . liftM Just

instance (MFunctor (p a' a b' b)) => MFunctor (MaybeP p a' a b' b) where
    mapT nat = MaybeP . mapT nat . runMaybeP

instance (Channel p) => Channel (MaybeP p) where
    idT = MaybeP . idT
    p1 >-> p2 = (MaybeP .) $ runMaybeP . p1 >-> runMaybeP . p2

instance ProxyTrans MaybeP where
    liftP = MaybeP . liftM Just

nothing :: (Monad (p a' a b' b m)) => MaybeP p a' a b' b m r
nothing = MaybeP $ return Nothing
