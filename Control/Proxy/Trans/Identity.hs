{-| This module provides the proxy transformer equivalent of 'IdentityT', except
    with the shortest name possible since it plays a key role in constraint
    inference. -}

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Trans.Identity (
    -- * Identity Proxy Transformer
    P(..),
    runK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Identity' proxy transformer
newtype P p a' a b' b (m :: * -> *) r =
    P { runP :: p a' a b' b m r }

instance (ProxyP     p, Monad m)
       => Functor (P p a' a b' b m) where
    fmap f p = P (
        runP p ?>= \x ->
        return_P (f x) )
 -- fmap = liftM

instance (ProxyP         p, Monad m)
       => Applicative (P p a' a b' b m) where
    pure = return

    fp <*> xp = P (
        runP fp ?>= \f ->
        runP xp ?>= \x ->
        return_P (f x) )
 -- fp <*> xp = ap

instance (ProxyP   p, Monad m)
       => Monad (P p a' a b' b m) where
    return = return_P
    (>>=) = (?>=)

instance (MonadPlusP             p, Monad m)
       => Alternative (P p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP    p )
       => MonadPlusP (P p) where
    mzero_P = P mzero_P
    mplus_P m1 m2 = P (mplus_P (runP m1) (runP m2))

instance (MonadPlusP   p, Monad m)
       => MonadPlus (P p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (ProxyP                p )
       => MonadTrans (P p a' a b' b) where
    lift = lift_P

instance (MonadIOP    p )
       => MonadIOP (P p) where
    liftIO_P m = P (liftIO_P m)
 -- liftIO = P . liftIO

instance (MonadIOP   p, MonadIO m)
       => MonadIO (P p a' a b' b m) where
    liftIO = liftIO_P

instance (MFunctorP    p )
       => MFunctorP (P p) where
    mapT_P nat p = P (mapT_P nat (runP p))
 -- mapT nat = P . mapT nat . runP

instance (MFunctorP   p )
       => MFunctor (P p a' a b' b) where
    mapT = mapT_P

instance (ProxyP    p )
       => ProxyP (P p) where
    idT = \a' -> P (idT a')
 -- idT = P . idT

    p1 >-> p2 = \c'1 -> P (
        ((\c'2 -> runP (p1 c'2))
     >-> (\b'  -> runP (p2 b' )) ) c'1 )
 -- p1 >-> p2 = (P .) $ runP . p1 >-> runP . p2

    request = \a' -> P (request a')
 -- request = P . request

    respond = \b -> P (respond b)
 -- respond = P . respond

    return_P = \r -> P (return_P r)
 -- return = P . return

    m ?>= f = P (
        runP m ?>= \x ->
        runP (f x) )

    lift_P m = P (lift_P m)
 -- lift = P . lift

instance (InteractP            p )
      =>  InteractP (P p) where
    p1 \>\ p2 = \c'1 -> P (
        ((\b'  -> runP (p1 b' ))
     \>\ (\c'2 -> runP (p2 c'2)) ) c'1 )
 -- p1 \>\ p2 = (P .) $ runP . p1 \>\ runP . p2

    p1 />/ p2 = \a1 -> P (
        ((\a2 -> runP (p1 a2))
     />/ (\b  -> runP (p2 b )) ) a1 )
 -- p1 />/ p2 = (P .) $ runP . p1 />/ runP . p2

instance ProxyTrans P where
    liftP = P

-- | Run an 'P' \'@K@\'leisli arrow
runK :: (q -> P p a' a b' b m r) -> (q -> p a' a b' b m r)
runK k q = runP (k q)
-- runK = (runP .)
