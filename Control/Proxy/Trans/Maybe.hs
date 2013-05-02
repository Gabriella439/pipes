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
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (
    Proxy(request, respond, (->>), (>>~), (>\\), (//>), turn),
    ProxyInternal(return_P, (?>=), lift_P, liftIO_P, hoist_P, thread_P),
    MonadPlusP(mzero_P, mplus_P) )
import Control.Proxy.Morph (PFunctor(hoistP), PMonad(embedP))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Maybe' proxy transformer
newtype MaybeP p a' a b' b (m :: * -> *) r
    = MaybeP { runMaybeP :: p a' a b' b m (Maybe r) }

instance (Monad m, Proxy p) => Functor (MaybeP p a' a b' b m) where
    fmap f p = MaybeP (
        runMaybeP p ?>= \m ->
        return_P (case m of
            Nothing -> Nothing
            Just x  -> Just (f x) ) )

instance (Monad m, Proxy p) => Applicative (MaybeP p a' a b' b m) where
    pure      = return
    fp <*> xp = MaybeP (
        runMaybeP fp ?>= \m1 ->
        case m1 of
            Nothing -> return_P Nothing
            Just f  ->
                runMaybeP xp ?>= \m2 ->
                case m2 of
                    Nothing -> return_P  Nothing
                    Just x  -> return_P (Just (f x)) )

instance (Monad m, Proxy p) => Monad (MaybeP p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (Proxy p) => MonadTrans (MaybeP p a' a b' b) where
    lift = lift_P

instance (Proxy p) => MFunctor (MaybeP p a' a b' b) where
    hoist = hoist_P

instance (MonadIO m, Proxy p) => MonadIO (MaybeP p a' a b' b m) where
    liftIO = liftIO_P

instance (Monad m, Proxy p) => Alternative (MaybeP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, Proxy p) => MonadPlus (MaybeP p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy p) => ProxyInternal (MaybeP p) where
    return_P = \r -> MaybeP (return_P (Just r))
    m ?>= f  = MaybeP (
        runMaybeP m ?>= \ma ->
        runMaybeP (case ma of
            Nothing -> MaybeP (return_P Nothing)
            Just a  -> f a ) )

    lift_P m = MaybeP (lift_P (m >>= \x -> return (Just x)))

    hoist_P nat p = MaybeP (hoist_P nat (runMaybeP p))

    liftIO_P m = MaybeP (liftIO_P (m >>= \x -> return (Just x)))

    thread_P p s = MaybeP (
        thread_P (runMaybeP p) s ?>= \(x, s') ->
        return_P (case x of
            Nothing -> Nothing
            Just r  -> Just (r, s') ) )

instance (Proxy p) => Proxy (MaybeP p) where
    fb' ->> p = MaybeP ((\b' -> runMaybeP (fb' b')) ->> runMaybeP p)
    p >>~ fb  = MaybeP (runMaybeP p >>~ (\b -> runMaybeP (fb b)))
    request = \a' -> MaybeP (request a' ?>= \a  -> return_P (Just a ))
    respond = \b  -> MaybeP (respond b  ?>= \b' -> return_P (Just b'))

    p //> fb = MaybeP (
        (runMaybeP p >>~ absorb) //> \b -> runMaybeP (fb b) )
      where
        absorb b =
            respond b ?>= \x ->
            case x of
                Nothing -> return_P Nothing
                Just b' ->
                    request b' ?>= \b2 ->
                    absorb b2
    fb' >\\ p = MaybeP (
        (\b' -> runMaybeP (fb' b')) >\\ (absorb ->> runMaybeP p) )
      where
        absorb b' =
            request b' ?>= \x ->
            case x of
                Nothing -> return_P Nothing
                Just b  ->
                    respond b ?>= \b'2 ->
                    absorb b'2

    turn p = MaybeP (turn (runMaybeP p))

instance (Proxy p) => MonadPlusP (MaybeP p) where
    mzero_P       = MaybeP (return_P Nothing)
    mplus_P m1 m2 = MaybeP (
        runMaybeP m1 ?>= \ma ->
        case ma of
            Nothing -> runMaybeP m2
            Just a  -> return_P (Just a) )

instance ProxyTrans MaybeP where
    liftP p = MaybeP (p ?>= \x -> return_P (Just x))

instance PFunctor MaybeP where
    hoistP nat p = MaybeP (nat (runMaybeP p))

instance PMonad MaybeP where
    embedP nat p = MaybeP (
        runMaybeP (nat (runMaybeP p)) ?>= \x ->
        return_P (case x of
            Nothing       -> Nothing
            Just Nothing  -> Nothing
            Just (Just a) -> Just a ) )

-- | Run a 'MaybeP' \'@K@\'leisli arrow, returning the result or 'Nothing'
runMaybeK :: (q -> MaybeP p a' a b' b m r) -> (q -> p a' a b' b m (Maybe r))
runMaybeK p q = runMaybeP (p q)
{-# INLINABLE runMaybeK #-}

-- | A synonym for 'mzero'
nothing :: (Monad m, Proxy p) => MaybeP p a' a b' b m r
nothing = MaybeP (return_P Nothing)
{-# INLINABLE nothing #-}

-- | A synonym for 'return'
just :: (Monad m, Proxy p) => r -> MaybeP p a' a b' b m r
just r = MaybeP (return_P (Just r))
{-# INLINABLE just #-}
