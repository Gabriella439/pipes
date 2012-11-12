-- | This module provides the proxy transformer equivalent of 'ReaderT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Reader (
    -- * ReaderP
    ReaderP(..),
    runReaderP,
    runReaderK,
    withReaderP,
    -- * Reader operations
    ask,
    local,
    asks,
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
    MonadP(return_P, (?>=)) )
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Reader' proxy transformer
newtype ReaderP i p a' a b' b (m :: * -> *) r
  = ReaderP { unReaderP :: i -> p a' a b' b m r }

instance (Monad             (p a' a b' b m))
       => Functor (ReaderP i p a' a b' b m) where
    fmap f p = ReaderP (\i -> do
        x <- unReaderP p i
        return (f x) )

instance (Monad                 (p a' a b' b m))
       => Applicative (ReaderP i p a' a b' b m) where
    pure = return
    p1 <*> p2 = ReaderP (\i -> do
        f <- unReaderP p1 i
        x <- unReaderP p2 i
        return (f x) )

instance (Monad           (p a' a b' b m))
       => Monad (ReaderP i p a' a b' b m) where
    return = \r -> ReaderP (\_ -> return r)
    m >>= f  = ReaderP (\i -> do
        a <- unReaderP m i
        unReaderP (f a) i )

instance (MonadPlus             (p a' a b' b m))
       => Alternative (ReaderP i p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus           (p a' a b' b m))
       => MonadPlus (ReaderP i p a' a b' b m) where
    mzero = ReaderP (\_ -> mzero)
    mplus m1 m2 = ReaderP (\i -> mplus (unReaderP m1 i) (unReaderP m2 i))

instance (MonadTrans           (p a' a b' b))
       => MonadTrans (ReaderP i p a' a b' b) where
    lift m = ReaderP (\_ -> lift m)

instance (MonadIO           (p a' a b' b m))
       => MonadIO (ReaderP i p a' a b' b m) where
    liftIO m = ReaderP (\_ -> liftIO m)

instance (MFunctor           (p a' a b' b))
       => MFunctor (ReaderP i p a' a b' b) where
    mapT nat p = ReaderP (\i -> mapT nat (unReaderP p i))
 -- mapT nat = ReaderP . fmap (mapT nat) . unReaderP

instance (Channel            p  )
       => Channel (ReaderP i p) where
    idT = \a' -> ReaderP (\_ -> idT a')
    p1 >-> p2 = \c'1 -> ReaderP (\i ->
        ((\b'  -> unReaderP (p1 b' ) i)
     >-> (\c'2 -> unReaderP (p2 c'2) i) ) c'1 )
 {- p1 >-> p2 = \c' -> ReaderP $ \i ->
        ((`unReaderP` i) . p1 >-> (`unReaderP` i) . p2) c' -}

instance (InteractId            p )
       => InteractId (ReaderP i p) where
    request = \a -> ReaderP (\_ -> request a)
    respond = \a -> ReaderP (\_ -> respond a)

instance (InteractComp            p)
       => InteractComp (ReaderP i p) where
    p1 \>\ p2 = \c'1 -> ReaderP (\i ->
        ((\b'  -> unReaderP (p1 b' ) i)
     \>\ (\c'2 -> unReaderP (p2 c'2) i) ) c'1 )
 {- p1 \>\ p2 = \c' -> ReaderP $ \i ->
        ((`unReaderP` i) . p1 \>\ (`unReaderP` i) . p2) c' -}

    p1 />/ p2 = \a1 -> ReaderP (\i ->
        ((\b  -> unReaderP (p1 b ) i)
     />/ (\a2 -> unReaderP (p2 a2) i) ) a1 )
 {- p1 />/ p2 = \a -> ReaderP $ \i ->
        ((`unReaderP` i) . p1 />/ (`unReaderP` i) . p2) a -}

instance (MonadP            p )
       => MonadP (ReaderP i p) where
    return_P = \r -> ReaderP (\_ -> return_P r)
    m ?>= f  = ReaderP (\i ->
        unReaderP m i ?>= \a ->
        unReaderP (f a) i )

instance ProxyTrans (ReaderP i) where
    liftP m = ReaderP (\_ -> m)

-- | Run a 'ReaderP' computation, supplying the environment
runReaderP :: i -> ReaderP i p a' a b' b m r -> p a' a b' b m r
runReaderP i m = unReaderP m i

-- | Run a 'ReaderP' \'@K@\'leisli arrow, supplying the environment
runReaderK :: i -> (q -> ReaderP i p a' a b' b m r) -> (q -> p a' a b' b m r)
runReaderK i p q = runReaderP i (p q)
-- runReaderK i = (runReaderP i .)

-- | Modify a computation's environment (a more general version of 'local')
withReaderP
 :: (Monad (p a' a b' b m))
 => (j -> i) -> ReaderP i p a' a b' b m r -> ReaderP j p a' a b' b m r
withReaderP f p = ReaderP (\i -> unReaderP p (f i))
-- withReaderP f p = ReaderP $ unReaderP p . f

-- | Get the environment
ask :: (Monad (p a' a b' b m)) => ReaderP i p a' a b' b m i
ask = ReaderP return

-- | Get a function of the environment
asks :: (Monad (p a' a b' b m)) => (i -> r) -> ReaderP i p a' a b' b m r
asks f = ReaderP (\i -> return (f i))

-- | Modify a computation's environment (a specialization of 'withReaderP')
local
 :: (Monad (p a' a b' b m))
 => (i -> i) -> ReaderP i p a' a b' b m r -> ReaderP i p a' a b' b m r
local = withReaderP
