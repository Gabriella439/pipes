-- | This module provides the proxy transformer equivalent of 'ReaderT'.

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Trans.Reader (
    -- * ReaderP
    ReaderP(..),
    runReaderP,
    runReaderK,

    -- * Reader operations
    ask,
    asks,
    local,
    withReaderP,
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(hoist))
import Control.PFunctor (PFunctor(hoistP))
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Reader' proxy transformer
newtype ReaderP i p a' a b' b (m :: * -> *) r
  = ReaderP { unReaderP :: i -> p a' a b' b m r }

instance (MonadP            p )
       => MonadP (ReaderP i p) where
    return_P = \r -> ReaderP (\_ -> return_P r)
    m ?>= f  = ReaderP (\i ->
        unReaderP m i ?>= \a -> 
        unReaderP (f a) i )

instance (Proxy              p, Monad m)
       => Functor (ReaderP i p a' a b' b m) where
    fmap f p = ReaderP (\i ->
        unReaderP p i ?>= \x ->
        return_P (f x) )

instance (Proxy                  p, Monad m)
       => Applicative (ReaderP i p a' a b' b m) where
    pure = return
    p1 <*> p2 = ReaderP (\i ->
        unReaderP p1 i ?>= \f -> 
        unReaderP p2 i ?>= \x -> 
        return_P (f x) )

instance (Proxy            p, Monad m)
       => Monad (ReaderP i p a' a b' b m) where
    return = return_P
    (>>=) = (?>=)

instance (MonadPlusP             p, Monad m)
       => Alternative (ReaderP i p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP           p )
       => MonadPlusP (ReaderP i p) where
    mzero_P = ReaderP (\_ -> mzero_P)
    mplus_P m1 m2 = ReaderP (\i -> mplus_P (unReaderP m1 i) (unReaderP m2 i))

instance (MonadPlusP           p, Monad m)
       => MonadPlus (ReaderP i p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (MonadTransP            p )
       => MonadTransP (ReaderP i p) where
    lift_P m = ReaderP (\_ -> lift_P m)

instance (Proxy                 p )
       => MonadTrans (ReaderP i p a' a b' b) where
    lift = lift_P

instance (MonadIOP            p )
       => MonadIOP (ReaderP i p) where
    liftIO_P m = ReaderP (\_ -> liftIO_P m)

instance (MonadIOP           p, MonadIO m)
       => MonadIO (ReaderP i p a' a b' b m) where
    liftIO = liftIO_P

instance (MFunctorP            p )
       => MFunctorP (ReaderP i p) where
    hoist_P nat p = ReaderP (\i -> hoist_P nat (unReaderP p i))
 -- hoist_P nat = ReaderP . fmap (hoist_P nat) . unReaderP

instance (Proxy               p )
       => MFunctor (ReaderP i p a' a b' b) where
    hoist = hoist_P

instance (Proxy            p  )
       => Proxy (ReaderP i p) where
    fb' ->> p = ReaderP (\i -> (\b' -> unReaderP (fb' b') i) ->> unReaderP p i)

    p >>~ fb  = ReaderP (\i -> unReaderP p i >>~ (\b -> unReaderP (fb b) i))

    request = \a -> ReaderP (\_ -> request a)
    respond = \a -> ReaderP (\_ -> respond a)

instance (Interact            p)
       => Interact (ReaderP i p) where
    fb' >\\ p = ReaderP (\i -> (\b' -> unReaderP (fb' b') i) >\\ unReaderP p i)

    p //> fb  = ReaderP (\i -> unReaderP p i //> (\b -> unReaderP (fb b) i))

instance ProxyTrans (ReaderP i) where
    liftP m = ReaderP (\_ -> m)

instance PFunctor (ReaderP i) where
    hoistP nat = ReaderP . (nat .) . unReaderP

-- | Run a 'ReaderP' computation, supplying the environment
runReaderP :: i -> ReaderP i p a' a b' b m r -> p a' a b' b m r
runReaderP i m = unReaderP m i

-- | Run a 'ReaderP' \'@K@\'leisli arrow, supplying the environment
runReaderK :: i -> (q -> ReaderP i p a' a b' b m r) -> (q -> p a' a b' b m r)
runReaderK i p q = runReaderP i (p q)
-- runReaderK i = (runReaderP i .)

-- | Get the environment
ask :: (Proxy p, Monad m) => ReaderP i p a' a b' b m i
ask = ReaderP return_P

-- | Get a function of the environment
asks :: (Proxy p, Monad m) => (i -> r) -> ReaderP i p a' a b' b m r
asks f = ReaderP (\i -> return_P (f i))

-- | Modify a computation's environment (a specialization of 'withReaderP')
local
 :: (i -> i) -> ReaderP i p a' a b' b m r -> ReaderP i p a' a b' b m r
local = withReaderP

-- | Modify a computation's environment (a more general version of 'local')
withReaderP
 :: (j -> i) -> ReaderP i p a' a b' b m r -> ReaderP j p a' a b' b m r
withReaderP f p = ReaderP (\i -> unReaderP p (f i))
-- withReaderP f p = ReaderP $ unReaderP p . f
