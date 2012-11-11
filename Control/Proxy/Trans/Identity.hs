-- | This module provides the proxy transformer equivalent of 'IdentityT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Identity (
    -- * IdentityP
    IdentityP(..),
    runIdentityK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT, (>->)),
    InteractId(request, respond),
    InteractComp((\>\), (/>/)),
    MonadProxy(returnP, (?>=)) )
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Identity' proxy transformer
newtype IdentityP p a' a b' b (m :: * -> *) r =
    IdentityP { runIdentityP :: p a' a b' b m r }

instance (Monad             (p a' a b' b m))
       => Functor (IdentityP p a' a b' b m) where
    fmap f p = IdentityP (do
        x <- runIdentityP p
        return (f x) )
 -- fmap = liftM

instance (Monad                 (p a' a b' b m))
       => Applicative (IdentityP p a' a b' b m) where
    pure = return

    fp <*> xp = IdentityP (do
        f <- runIdentityP fp
        x <- runIdentityP xp
        return (f x) )
 -- fp <*> xp = ap

instance (Monad           (p a' a b' b m))
       => Monad (IdentityP p a' a b' b m) where
    return r = IdentityP (return r)
 -- return = IdentityP . return

    m >>= f = IdentityP (do
        x <- runIdentityP m
        runIdentityP (f x) )

instance (MonadPlus             (p a' a b' b m))
       => Alternative (IdentityP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus           (p a' a b' b m))
       => MonadPlus (IdentityP p a' a b' b m) where
    mzero = IdentityP mzero
    mplus m1 m2 = IdentityP (mplus (runIdentityP m1) (runIdentityP m2))

instance (MonadTrans           (p a' a b' b))
       => MonadTrans (IdentityP p a' a b' b) where
    lift m = IdentityP (lift m)
 -- lift = IdentityP . lift

instance (MonadIO           (p a' a b' b m))
       => MonadIO (IdentityP p a' a b' b m) where
    liftIO m = IdentityP (liftIO m)
 -- liftIO = IdentityP . liftIO

instance (MFunctor           (p a' a b' b))
       => MFunctor (IdentityP p a' a b' b) where
    mapT nat p = IdentityP (mapT nat (runIdentityP p))
 -- mapT nat = IdentityP . mapT nat . runIdentityP

instance (Channel            p )
       => Channel (IdentityP p) where
    idT = \a' -> IdentityP (idT a')
 -- idT = IdentityP . idT

    p1 >-> p2 = \c'1 -> IdentityP (
        ((\c'2 -> runIdentityP (p1 c'2))
     >-> (\b'  -> runIdentityP (p2 b' )) ) c'1 )
 -- p1 >-> p2 = (IdentityP .) $ runIdentityP . p1 >-> runIdentityP . p2

instance (InteractId            p )
       => InteractId (IdentityP p) where
    request = \a' -> IdentityP (request a')
 -- request = IdentityP . request

    respond = \b -> IdentityP (respond b)
 -- respond = IdentityP . respond

instance (InteractComp            p )
      =>  InteractComp (IdentityP p) where
    p1 \>\ p2 = \c'1 -> IdentityP (
        ((\b'  -> runIdentityP (p1 b' ))
     \>\ (\c'2 -> runIdentityP (p2 c'2)) ) c'1 )
 -- p1 \>\ p2 = (IdentityP .) $ runIdentityP . p1 \>\ runIdentityP . p2

    p1 />/ p2 = \a1 -> IdentityP (
        ((\a2 -> runIdentityP (p1 a2))
     />/ (\b  -> runIdentityP (p2 b )) ) a1 )
 -- p1 />/ p2 = (IdentityP .) $ runIdentityP . p1 />/ runIdentityP . p2

instance (MonadProxy            p )
       => MonadProxy (IdentityP p) where
    returnP = \r -> IdentityP (returnP r)
    m ?>= f = IdentityP (
        runIdentityP m ?>= \r ->
        runIdentityP (f r) )

instance ProxyTrans IdentityP where
    liftP = IdentityP

-- | Run an 'IdentityP' \'@K@\'leisli arrow
runIdentityK :: (q -> IdentityP p a' a b' b m r) -> (q -> p a' a b' b m r)
runIdentityK k q = runIdentityP (k q)
-- runIdentityK = (runIdentityP .)
