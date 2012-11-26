-- | This module provides the proxy transformer equivalent of 'MaybeT'.

{-# LANGUAGE KindSignatures #-}

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
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Maybe' proxy transformer
newtype MaybeP p a' a b' b (m :: * -> *) r
  = MaybeP { runMaybeP :: p a' a b' b m (Maybe r) }

instance (Proxy           p, Monad m)
       => Functor (MaybeP p a' a b' b m) where
    fmap f p = MaybeP (
        runMaybeP p ?>= \m ->
        return_P (case m of
            Nothing -> Nothing
            Just x  -> Just (f x) ) )
 -- fmap f = MaybeP . fmap (fmap f) . runMaybeP

instance (Proxy               p, Monad m)
       => Applicative (MaybeP p a' a b' b m) where
    pure = return

    fp <*> xp = MaybeP (
        runMaybeP fp ?>= \m1 ->
        case m1 of
            Nothing -> return_P Nothing
            Just f  ->
                runMaybeP xp ?>= \m2 ->
                case m2 of
                    Nothing -> return_P Nothing
                    Just x  -> return_P (Just (f x)) )
 -- fp <*> xp = MaybeP ((<*>) <$> (runMaybeP fp) <*> (runMaybeP xp))

instance (Proxy         p, Monad m)
       => Monad (MaybeP p a' a b' b m) where
    return = return_P
    (>>=) = (?>=)

instance (Proxy               p, Monad m)
       => Alternative (MaybeP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Proxy              p )
       => MonadPlusP (MaybeP p) where
    mzero_P = nothing
    mplus_P m1 m2 = MaybeP (
        runMaybeP m1 ?>= \ma ->
        runMaybeP (case ma of
            Nothing -> m2
            Just a  -> just a) )

instance (Proxy             p, Monad m)
       => MonadPlus (MaybeP p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy              p )
       => MonadTrans (MaybeP p a' a b' b) where
    lift = lift_P

instance (MonadIOP         p )
       => MonadIOP (MaybeP p) where
    liftIO_P m = MaybeP (liftIO_P (m >>= \x -> return (Just x)))
 -- liftIO = MaybeP . liftIO . liftM Just

instance (MonadIOP        p, MonadIO m)
       => MonadIO (MaybeP p a' a b' b m) where
    liftIO = liftIO_P

instance (MFunctorP         p )
       => MFunctorP (MaybeP p) where
    mapT_P nat p = MaybeP (mapT_P nat (runMaybeP p))
 -- mapT nat = MaybeP . mapT nat . runMaybeP

instance (MFunctorP        p )
       => MFunctor (MaybeP p a' a b' b) where
    mapT = mapT_P

instance (Proxy         p )
       => Proxy (MaybeP p) where
    idT = \a' -> MaybeP (idT a')
 -- idT = MaybeP . idT

    p1 >-> p2 = \c'1 -> MaybeP (
        ((\b' -> runMaybeP (p1 b')) >-> (\c'2 -> runMaybeP (p2 c'2))) c'1 )
 -- p1 >-> p2 = (MaybeP .) $ runMaybeP . p1 >-> runMaybeP . p2

    request = \a' -> MaybeP (request a' ?>= \a  -> return_P (Just a ))
    respond = \b  -> MaybeP (respond b  ?>= \b' -> return_P (Just b'))

    return_P = just
    m ?>= f = MaybeP (
        runMaybeP m ?>= \ma ->
        runMaybeP (case ma of
            Nothing -> nothing
            Just a  -> f a ) )

    lift_P m = MaybeP (lift_P (m >>= \x -> return (Just x)))
 -- lift = MaybeP . lift . liftM Just

instance ProxyTrans MaybeP where
    liftP p = MaybeP (p ?>= \x -> return_P (Just x))
 -- liftP = MaybeP . liftM Just

-- | Run a 'MaybeP' \'@K@\'leisli arrow, returning the result or 'Nothing'
runMaybeK :: (q -> MaybeP p a' a b' b m r) -> (q -> p a' a b' b m (Maybe r))
runMaybeK p q = runMaybeP (p q)
-- runMaybeK = (runMaybeP .)

-- | A synonym for 'mzero'
nothing :: (Monad m, Proxy p) => MaybeP p a' a b' b m r
nothing = MaybeP (return_P Nothing)

-- | A synonym for 'return'
just :: (Monad m, Proxy p) => r -> MaybeP p a' a b' b m r
just r = MaybeP (return_P (Just r))
