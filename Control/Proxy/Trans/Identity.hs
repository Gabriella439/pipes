-- | This module provides the proxy transformer equivalent of 'IdentityT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Identity (
    -- * IdentityP
    IdentityP(..)
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT    , (>->)), 
    Request(request, (\>\)), 
    Respond(respond, (/>/)))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Identity' proxy transformer
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

instance (MonadPlus (p a' a b' b m))
 => Alternative (IdentityP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus (p a' a b' b m))
 => MonadPlus (IdentityP p a' a b' b m) where
    mzero = IdentityP mzero
    mplus m1 m2 = IdentityP $ mplus (runIdentityP m1) (runIdentityP m2)

instance (MonadTrans (p a' a b' b)) => MonadTrans (IdentityP p a' a b' b) where
    lift = IdentityP . lift

instance (MonadIO (p a' a b' b m)) => MonadIO (IdentityP p a' a b' b m) where
    liftIO = IdentityP . liftIO

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
