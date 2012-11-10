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
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (Channel(idT, (>->)))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Maybe' proxy transformer
newtype MaybeP p a' a b' b (m :: * -> *) r
  = MaybeP { runMaybeP :: p a' a b' b m (Maybe r) }

instance (Functor        (p a' a b' b m))
       => Functor (MaybeP p a' a b' b m) where
    fmap f p = MaybeP (fmap (fmap f) (runMaybeP p))
 -- fmap f = MaybeP . fmap (fmap f) . runMaybeP

instance (Applicative        (p a' a b' b m))
       => Applicative (MaybeP p a' a b' b m) where
    pure r = MaybeP (pure (Just r))
 -- pure = MaybeP . pure . Just

    fp <*> xp = MaybeP (fmap (<*>) (runMaybeP fp) <*> (runMaybeP xp))
 -- fp <*> xp = MaybeP ((<*>) <$> (runMaybeP fp) <*> (runMaybeP xp))

instance (Monad        (p a' a b' b m))
       => Monad (MaybeP p a' a b' b m) where
    return = just
    m >>= f = MaybeP $ do
        ma <- runMaybeP m
        runMaybeP $ case ma of
            Nothing -> nothing
            Just a  -> f a

{- I don't use the weaker Applicative context for the Alternative instance,
   otherwise mplus would need to evaluate both actions. -}
instance (Applicative        (p a' a b' b m),
          Monad              (p a' a b' b m))
       => Alternative (MaybeP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Monad            (p a' a b' b m))
       => MonadPlus (MaybeP p a' a b' b m) where
    mzero = nothing
    mplus m1 m2 = MaybeP (do
        ma <- runMaybeP m1
        runMaybeP $ case ma of
            Nothing -> m2
            Just a  -> just a )

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
    idT a' = MaybeP (idT a')
 -- idT = MaybeP . idT

    p1 >-> p2 = \c'1 -> MaybeP (
        ((\b' -> runMaybeP (p1 b')) >-> (\c'2 -> runMaybeP (p2 c'2))) c'1 )
 -- p1 >-> p2 = (MaybeP .) $ runMaybeP . p1 >-> runMaybeP . p2

instance ProxyTrans MaybeP where
    liftP p = MaybeP (p >>= \x -> return (Just x))
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
