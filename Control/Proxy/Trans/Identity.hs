-- | This module provides the proxy transformer equivalent of 'IdentityT'.

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Trans.Identity (
    -- * Identity Proxy Transformer
    IdentityP(..),
    runIdentityK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Identity' proxy transformer
newtype IdentityP p a' a b' b (m :: * -> *) r =
    IdentityP { runIdentityP :: p a' a b' b m r }

instance (ProxyP             p, Monad m)
       => Functor (IdentityP p a' a b' b m) where
    fmap f p = IdentityP (
        runIdentityP p ?>= \x ->
        return_P (f x) )
 -- fmap = liftM

instance (ProxyP                 p, Monad m)
       => Applicative (IdentityP p a' a b' b m) where
    pure = return

    fp <*> xp = IdentityP (
        runIdentityP fp ?>= \f ->
        runIdentityP xp ?>= \x ->
        return_P (f x) )
 -- fp <*> xp = ap

instance (ProxyP           p, Monad m)
       => Monad (IdentityP p a' a b' b m) where
    return = return_P
    (>>=) = (?>=)

instance (MonadPlusP             p, Monad m)
       => Alternative (IdentityP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP            p )
       => MonadPlusP (IdentityP p) where
    mzero_P = IdentityP mzero_P
    mplus_P m1 m2 = IdentityP (mplus_P (runIdentityP m1) (runIdentityP m2))

instance (MonadPlusP           p, Monad m)
       => MonadPlus (IdentityP p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (ProxyP                p )
       => MonadTrans (IdentityP p a' a b' b) where
    lift = lift_P

instance (MonadIOP            p )
       => MonadIOP (IdentityP p) where
    liftIO_P m = IdentityP (liftIO_P m)
 -- liftIO = IdentityP . liftIO

instance (MonadIOP           p, MonadIO m)
       => MonadIO (IdentityP p a' a b' b m) where
    liftIO = liftIO_P

instance (MFunctorP            p )
       => MFunctorP (IdentityP p) where
    mapT_P nat p = IdentityP (mapT_P nat (runIdentityP p))
 -- mapT nat = IdentityP . mapT nat . runIdentityP

instance (MFunctorP           p )
       => MFunctor (IdentityP p a' a b' b) where
    mapT = mapT_P

instance (ProxyP            p )
       => ProxyP (IdentityP p) where
    idT = \a' -> IdentityP (idT a')
 -- idT = IdentityP . idT

    p1 >-> p2 = \c'1 -> IdentityP (
        ((\c'2 -> runIdentityP (p1 c'2))
     >-> (\b'  -> runIdentityP (p2 b' )) ) c'1 )
 -- p1 >-> p2 = (IdentityP .) $ runIdentityP . p1 >-> runIdentityP . p2

    request = \a' -> IdentityP (request a')
 -- request = P . request

    respond = \b -> IdentityP (respond b)
 -- respond = P . respond

    return_P = \r -> IdentityP (return_P r)
 -- return = P . return

    m ?>= f = IdentityP (
        runIdentityP m ?>= \x ->
        runIdentityP (f x) )

    lift_P m = IdentityP (lift_P m)
 -- lift = P . lift

instance (InteractP            p )
      =>  InteractP (IdentityP p) where
    p1 \>\ p2 = \c'1 -> IdentityP (
        ((\b'  -> runIdentityP (p1 b' ))
     \>\ (\c'2 -> runIdentityP (p2 c'2)) ) c'1 )
 -- p1 \>\ p2 = (IdentityP .) $ runIdentityP . p1 \>\ runIdentityP . p2

    p1 />/ p2 = \a1 -> IdentityP (
        ((\a2 -> runIdentityP (p1 a2))
     />/ (\b  -> runIdentityP (p2 b )) ) a1 )
 -- p1 />/ p2 = (IdentityP .) $ runIdentityP . p1 />/ runIdentityP . p2

instance ProxyTrans IdentityP where
    liftP = IdentityP

-- | Run an 'P' \'@K@\'leisli arrow
runIdentityK :: (q -> IdentityP p a' a b' b m r) -> (q -> p a' a b' b m r)
runIdentityK k q = runIdentityP (k q)
-- runIdentityK = (runIdentityP .)
