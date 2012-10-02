-- | This module provides the proxy transformer equivalent of 'IdentityT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Identity (
    IdentityP(..)
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

-- | The trivial proxy transformer, which maps a proxy to itself
newtype IdentityP p a' a b' b (m :: * -> *) r
  = IdentityP { runIdentityP :: p a' a b' b m r }

instance (Monad (p a' a b' b m)) => Functor (IdentityP p a' a b' b m) where
    fmap = liftM

instance (Monad (p a' a b' b m)) => Applicative (IdentityP p a' a b' b m) where
    pure  = return
    (<*>) = ap

instance (Monad (p a' a b' b m)) => Monad (IdentityP p a' a b' b m) where
    return = IdentityP . return
    m >>= f = IdentityP $ runIdentityP m >>= runIdentityP . f

instance (MonadTrans (p a' a b' b)) => MonadTrans (IdentityP p a' a b' b) where
    lift = IdentityP . lift

instance (MFunctor (p a' a b' b)) => MFunctor (IdentityP p a' a b' b) where
    mapT nat = IdentityP . mapT nat . runIdentityP

instance (Channel p) => Channel (IdentityP p) where
    idT = IdentityP . idT
    p1 >-> p2 = (IdentityP .) $ runIdentityP . p1 >-> runIdentityP . p2

instance (Request p) => Request (IdentityP p) where
    request = IdentityP . request
    p1 \>\ p2 = (IdentityP .) $ runIdentityP . p1 \>\ runIdentityP . p2

instance (Respond p) => Respond (IdentityP p) where
    respond = IdentityP . respond
    p1 />/ p2 = (IdentityP .) $ runIdentityP . p1 />/ runIdentityP . p2

instance ProxyTrans IdentityP where
    liftP = IdentityP
