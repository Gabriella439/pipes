-- | This module provides the proxy transformer equivalent of 'MaybeT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Maybe (
    -- * MaybeP
    MaybeP(..),
    runMaybeK,
    -- * Maybe operations
    nothing,
    just
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT, (>->)),
    InteractId(request, respond),
    MonadP(return_P, (?>=)) )
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Maybe' proxy transformer
newtype MaybeP p a' a b' b (m :: * -> *) r
  = MaybeP { runMaybeP :: p a' a b' b m (Maybe r) }

instance (Monad          (p a' a b' b m))
       => Functor (MaybeP p a' a b' b m) where
    fmap f p = MaybeP (do
        m <- runMaybeP p
        return (case m of
            Nothing -> Nothing
            Just x  -> Just (f x) ) )
 -- fmap f = MaybeP . fmap (fmap f) . runMaybeP

instance (Monad              (p a' a b' b m))
       => Applicative (MaybeP p a' a b' b m) where
    pure = return

    fp <*> xp = MaybeP (do
        m1 <- runMaybeP fp
        case m1 of
            Nothing -> return Nothing
            Just f  -> do
                m2 <- runMaybeP xp
                case m2 of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x)) )
 -- fp <*> xp = MaybeP ((<*>) <$> (runMaybeP fp) <*> (runMaybeP xp))

instance (Monad        (p a' a b' b m))
       => Monad (MaybeP p a' a b' b m) where
    return = just
    m >>= f = MaybeP (do
        ma <- runMaybeP m
        runMaybeP $ case ma of
            Nothing -> nothing
            Just a  -> f a )

instance (Monad              (p a' a b' b m))
       => Alternative (MaybeP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Monad            (p a' a b' b m))
       => MonadPlus (MaybeP p a' a b' b m) where
    mzero = nothing
    mplus m1 m2 = MaybeP (do
        ma <- runMaybeP m1
        runMaybeP (case ma of
            Nothing -> m2
            Just a  -> just a) )

instance (MonadTrans        (p a' a b' b))
       => MonadTrans (MaybeP p a' a b' b) where
    lift m = MaybeP (lift (m >>= \x -> return (Just x)))
 -- lift = MaybeP . lift . liftM Just

instance (MonadIO        (p a' a b' b m))
       => MonadIO (MaybeP p a' a b' b m) where
    liftIO m = MaybeP (liftIO (m >>= \x -> return (Just x)))
 -- liftIO = MaybeP . liftIO . liftM Just

instance (MFunctor        (p a' a b' b))
       => MFunctor (MaybeP p a' a b' b) where
    mapT nat p = MaybeP (mapT nat (runMaybeP p))
 -- mapT nat = MaybeP . mapT nat . runMaybeP

instance (Channel         p )
       => Channel (MaybeP p) where
    idT = \a' -> MaybeP (idT a')
 -- idT = MaybeP . idT

    p1 >-> p2 = \c'1 -> MaybeP (
        ((\b' -> runMaybeP (p1 b')) >-> (\c'2 -> runMaybeP (p2 c'2))) c'1 )
 -- p1 >-> p2 = (MaybeP .) $ runMaybeP . p1 >-> runMaybeP . p2

instance (InteractId         p, MonadP p)
       => InteractId (MaybeP p) where
    request = \a' -> MaybeP (request a' ?>= \a  -> return_P (Just a ))
    respond = \b  -> MaybeP (respond b  ?>= \b' -> return_P (Just b'))

instance (MonadP         p )
       => MonadP (MaybeP p) where
    return_P = \r -> MaybeP (return_P (Just r))
    m ?>= f = MaybeP (
        runMaybeP m ?>= \ma ->
        case ma of
            Nothing -> return_P Nothing
            Just a  -> runMaybeP (f a) )

instance ProxyTrans MaybeP where
    liftP p = MaybeP (p ?>= \x -> return_P (Just x))
 -- liftP = MaybeP . liftM Just

-- | Run a 'MaybeP' \'@K@\'leisli arrow, returning the result or 'Nothing'
runMaybeK :: (q -> MaybeP p a' a b' b m r) -> (q -> p a' a b' b m (Maybe r))
runMaybeK p q = runMaybeP (p q)
-- runMaybeK = (runMaybeP .)

-- | A synonym for 'mzero'
nothing :: (Monad (p a' a b' b m)) => MaybeP p a' a b' b m r
nothing = MaybeP (return Nothing)

-- | A synonym for 'return'
just :: (Monad (p a' a b' b m)) => r -> MaybeP p a' a b' b m r
just r = MaybeP (return (Just r))
