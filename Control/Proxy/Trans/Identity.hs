-- | This module provides the proxy transformer equivalent of 'IdentityT'.

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Trans.Identity (
    -- * Identity Proxy Transformer
    IdentityP(..),
    identityK,
    runIdentityK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.Morph (PFunctor(hoistP), PMonad(embedP))
import Control.Proxy.ListT (ListT((>\\), (//>)))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Identity' proxy transformer
newtype IdentityP p a' a b' b (m :: * -> *) r =
    IdentityP { runIdentityP :: p a' a b' b m r }

instance (Proxy p, Monad m) => Functor (IdentityP p a' a b' b m) where
    fmap f p = IdentityP (
        runIdentityP p ?>= \x ->
        return_P (f x) )

instance (Proxy p, Monad m) => Applicative (IdentityP p a' a b' b m) where
    pure      = return
    fp <*> xp = IdentityP (
        runIdentityP fp ?>= \f ->
        runIdentityP xp ?>= \x ->
        return_P (f x) )

instance (Proxy p, Monad m) => Monad (IdentityP p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (Proxy p) => MonadTrans (IdentityP p a' a b' b) where
    lift = lift_P

instance (Proxy p) => MFunctor (IdentityP p a' a b' b) where
    hoist = hoist_P

instance (Proxy p, MonadIO m) => MonadIO (IdentityP p a' a b' b m) where
    liftIO = liftIO_P

instance (MonadPlusP p, Monad m) => Alternative (IdentityP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP p, Monad m) => MonadPlus (IdentityP p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy p) => ProxyInternal (IdentityP p) where
    return_P = \r -> IdentityP (return_P r)
    m ?>= f  = IdentityP (
        runIdentityP m ?>= \x ->
        runIdentityP (f x) )

    lift_P m = IdentityP (lift_P m)

    hoist_P nat p = IdentityP (hoist_P nat (runIdentityP p))

    liftIO_P m = IdentityP (liftIO_P m)

instance (Proxy p) => Proxy (IdentityP p) where
    fb' ->> p = IdentityP ((\b' -> runIdentityP (fb' b')) ->> runIdentityP p)
    p >>~ fb  = IdentityP (runIdentityP p >>~ (\b -> runIdentityP (fb b)))

    request = \a' -> IdentityP (request a')
    respond = \b  -> IdentityP (respond b )

instance (ListT p) => ListT (IdentityP p) where
    fb' >\\ p = IdentityP ((\b' -> runIdentityP (fb' b')) >\\ runIdentityP p)
    p //> fb  = IdentityP (runIdentityP p //> (\b -> runIdentityP (fb b)))

instance (MonadPlusP p) => MonadPlusP (IdentityP p) where
    mzero_P       = IdentityP  mzero_P
    mplus_P m1 m2 = IdentityP (mplus_P (runIdentityP m1) (runIdentityP m2))

instance ProxyTrans IdentityP where
    liftP = IdentityP

instance PFunctor IdentityP where
    hoistP nat p = IdentityP (nat (runIdentityP p))

instance PMonad IdentityP where
    embedP nat p = nat (runIdentityP p)

-- | Wrap a \'@K@\'leisli arrow in 'IdentityP'
identityK :: (q -> p a' a b' b m r) -> (q -> IdentityP p a' a b' b m r)
identityK k q = IdentityP (k q)

-- | Run an 'P' \'@K@\'leisli arrow
runIdentityK :: (q -> IdentityP p a' a b' b m r) -> (q -> p a' a b' b m r)
runIdentityK k q = runIdentityP (k q)
