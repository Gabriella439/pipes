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
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT    , (>->)), 
    Request(request, (\>\)), 
    Respond(respond, (/>/)))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Reader' proxy transformer
newtype ReaderP i p a' a b' b (m :: * -> *) r
  = ReaderP { unReaderP :: i -> p a' a b' b m r }

instance (Monad (p a' a b' b m)) => Functor (ReaderP i p a' a b' b m) where
    fmap = liftM

instance (Monad (p a' a b' b m)) => Applicative (ReaderP i p a' a b' b m) where
    pure  = return
    (<*>) = ap

instance (Monad (p a' a b' b m)) => Monad (ReaderP i p a' a b' b m) where
    return a = ReaderP $ \_ -> return a
    m >>= f = ReaderP $ \i -> do
        a <- unReaderP m i
        unReaderP (f a) i

instance (MonadPlus (p a' a b' b m))
 => Alternative (ReaderP i p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus (p a' a b' b m))
 => MonadPlus (ReaderP i p a' a b' b m) where
    mzero = ReaderP $ \_ -> mzero
    mplus m1 m2 = ReaderP $ \i -> mplus (unReaderP m1 i) (unReaderP m2 i)

instance (MonadTrans (p a' a b' b)) => MonadTrans (ReaderP i p a' a b' b) where
    lift m = ReaderP $ \_ -> lift m

instance (MonadIO (p a' a b' b m)) => MonadIO (ReaderP i p a' a b' b m) where
    liftIO m = ReaderP $ \_ -> liftIO m

instance (MFunctor (p a' a b' b)) => MFunctor (ReaderP i p a' a b' b) where
    mapT nat = ReaderP . fmap (mapT nat) . unReaderP

instance (Channel p) => Channel (ReaderP i p) where
    idT a = ReaderP $ \_ -> idT a
    (p1 >-> p2) a = ReaderP $ \i ->
        ((`unReaderP` i) . p1 >-> (`unReaderP` i) . p2) a

instance (Request p) => Request (ReaderP i p) where
    request a = ReaderP $ \_ -> request a
    (p1 \>\ p2) a = ReaderP $ \i ->
        ((`unReaderP` i) . p1 \>\ (`unReaderP` i) . p2) a

instance (Respond p) => Respond (ReaderP i p) where
    respond a = ReaderP $ \_ -> respond a
    (p1 />/ p2) a = ReaderP $ \i ->
        ((`unReaderP` i) . p1 />/ (`unReaderP` i) . p2) a

instance ProxyTrans (ReaderP i) where
    liftP m = ReaderP $ \_ -> m

-- | Run a 'ReaderP' computation, supplying the environment
runReaderP :: i -> ReaderP i p a' a b' b m r -> p a' a b' b m r
runReaderP i m = unReaderP m i

-- | Run a 'ReaderP' \'@K@\'leisli arrow, supplying the environment
runReaderK :: i -> (q -> ReaderP i p a' a b' b m r) -> (q -> p a' a b' b m r)
runReaderK i = (runReaderP i .)

-- | Modify a computation's environment (a more general version of 'local')
withReaderP
 :: (Monad (p a' a b' b m))
 => (j -> i) -> ReaderP i p a' a b' b m r -> ReaderP j p a' a b' b m r
withReaderP f r = ReaderP $ unReaderP r . f

-- | Get the environment
ask :: (Monad (p a' a b' b m)) => ReaderP i p a' a b' b m i
ask = ReaderP return

-- | Get a function of the environment
asks :: (Monad (p a' a b' b m)) => (i -> r) -> ReaderP i p a' a b' b m r
asks f = ReaderP (return . f)

-- | Modify a computation's environment (a specialization of 'withReaderP')
local
 :: (Monad (p a' a b' b m))
 => (i -> i) -> ReaderP i p a' a b' b m r -> ReaderP i p a' a b' b m r
local = withReaderP
