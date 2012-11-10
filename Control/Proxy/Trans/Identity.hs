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
    Channel(idT, (>->)), Interact(request, (\>\), respond, (/>/)) )
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Identity' proxy transformer
newtype IdentityP p a' a b' b (m :: * -> *) r =
    IdentityP { runIdentityP :: p a' a b' b m r }

instance (Functor (p a' a b' b m))
 => Functor (IdentityP p a' a b' b m) where
    fmap f p = IdentityP (fmap f (runIdentityP p))
 -- fmap f = IdentityP . fmap f . runIdentityP

instance (Applicative (p a' a b' b m))
 => Applicative (IdentityP p a' a b' b m) where
    pure r = IdentityP (pure r)
 -- pure = IdentityP . pure

    fp <*> xp = IdentityP (runIdentityP fp <*> runIdentityP xp)

instance (Monad (p a' a b' b m))
 => Monad (IdentityP p a' a b' b m) where
    return r = IdentityP (return r)
 -- return = IdentityP . return

    m >>= f = IdentityP (runIdentityP m >>= \x -> runIdentityP (f x))

instance (Alternative (p a' a b' b m))
 => Alternative (IdentityP p a' a b' b m) where
    empty = IdentityP empty
    m1 <|> m2 = IdentityP (runIdentityP m1 <|> runIdentityP m2)

instance (MonadPlus (p a' a b' b m))
 => MonadPlus (IdentityP p a' a b' b m) where
    mzero = IdentityP mzero
    mplus m1 m2 = IdentityP $ mplus (runIdentityP m1) (runIdentityP m2)

instance (MonadTrans (p a' a b' b)) => MonadTrans (IdentityP p a' a b' b) where
    lift m = IdentityP (lift m)
 -- lift = IdentityP . lift

instance (MonadIO (p a' a b' b m)) => MonadIO (IdentityP p a' a b' b m) where
    liftIO m = IdentityP (liftIO m)
 -- liftIO = IdentityP . liftIO

instance (MFunctor (p a' a b' b)) => MFunctor (IdentityP p a' a b' b) where
    mapT nat p = IdentityP (mapT nat (runIdentityP p))
 -- mapT nat = IdentityP . mapT nat . runIdentityP

instance (Channel p) => Channel (IdentityP p) where
    idT = \a' -> IdentityP (idT a')
 -- idT = IdentityP . idT

    p1 >-> p2 = \c'1 -> IdentityP (
        ((\c'2 -> runIdentityP (p1 c'2))
     >-> (\b'  -> runIdentityP (p2 b' )) ) c'1 )
 -- p1 >-> p2 = (IdentityP .) $ runIdentityP . p1 >-> runIdentityP . p2

instance (Interact p) => Interact (IdentityP p) where
    request a' = IdentityP (request a')
 -- request = IdentityP . request

    p1 \>\ p2 = \c'1 -> IdentityP (
        ((\b'  -> runIdentityP (p1 b' ))
     \>\ (\c'2 -> runIdentityP (p2 c'2)) ) c'1 )
 -- p1 \>\ p2 = (IdentityP .) $ runIdentityP . p1 \>\ runIdentityP . p2

    respond b = IdentityP (respond b )
 -- respond = IdentityP . respond

    p1 />/ p2 = \a1 -> IdentityP (
        ((\a2 -> runIdentityP (p1 a2)) />/ (\b -> runIdentityP (p2 b))) a1 )
 -- p1 />/ p2 = (IdentityP .) $ runIdentityP . p1 />/ runIdentityP . p2

instance ProxyTrans IdentityP where
    liftP = IdentityP

-- | Run an 'IdentityP' \'@K@\'leisli arrow
runIdentityK :: (q -> IdentityP p a' a b' b m r) -> (q -> p a' a b' b m r)
runIdentityK k = \q -> runIdentityP (k q)
-- runIdentityK = (runIdentityP .)
