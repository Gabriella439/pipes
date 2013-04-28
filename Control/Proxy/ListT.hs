{-| This module implements \"ListT done right\" in terms of proxies.

    The 'RespondT' monad transformer is the 'ListT' monad transformer over the
    downstream output type.  Each 'respond' corresponds to an element of the
    list.  The monad bind operation non-deterministically selects one of the
    previous 'respond's as input.  The 'RespondT' Kleisli category corresponds
    to the \"respond\" category of the 'ListT' class.

    Symmetrically, the 'RequestT' monad transformer is the 'ListT' monad
    transformer over the upstream output type.  Each 'request' corresponds to an
    element of the list.  The monad bind operation non-deterministically selects
    one of the previous 'request's as input.  The 'RequestT' Kleisli category
    corresponds to the \"request\" category of the 'ListT' class.

    Unlike 'ListT' from @transformers@, these monad transformers are correct by
    construction and always satisfy the monad and monad transformer laws.
-}

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.ListT (
    -- * Respond Monad Transformer
    RespondT(..),
    runRespondK,
    ProduceT,

    -- * Request Monad Transformer
    RequestT(..),
    runRequestK,
    CoProduceT,

    -- * Utilities
    eachS,
    eachC,
    rangeS,
    rangeC,
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (
    Proxy(request, respond, (//>)), (//<), return_P, (?>=), lift_P )
import Control.Proxy.Prelude.Base (
    fromListS, fromListC, enumFromToS, enumFromToC )
import Control.Proxy.Synonym (C)
import Data.Monoid (Monoid(mempty, mappend))

-- For documentation
import Control.Monad ((>=>), (<=<))

-- | A monad transformer over a proxy's downstream output
newtype RespondT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a b' m b =
    RespondT { runRespondT :: p a' a b' b m b' }

instance (Monad m, Proxy p) => Functor (RespondT p a' a b' m) where
    fmap f p = RespondT (runRespondT p //> \a -> respond (f a))

instance (Monad m, Proxy p) => Applicative (RespondT p a' a b' m) where
    pure a = RespondT (respond a)
    mf <*> mx = RespondT (
        runRespondT mf //> \f ->
        runRespondT mx //> \x ->
        respond (f x) )

instance (Monad m, Proxy p) => Monad (RespondT p a' a b' m) where
    return a = RespondT (respond a)
    m >>= f  = RespondT (runRespondT m //> \a -> runRespondT (f a))

instance (Proxy p) => MonadTrans (RespondT p a' a b') where
    lift m = RespondT (lift_P m ?>= \a -> respond a)

instance (MonadIO m, Proxy p) => MonadIO (RespondT p a' a b' m) where
    liftIO m = lift (liftIO m)

instance (Monad m, Proxy p, Monoid b')
       => Alternative (RespondT p a' a b' m) where
    empty = RespondT (return_P mempty)
    p1 <|> p2 = RespondT (
        runRespondT p1 ?>= \r1 ->
        runRespondT p2 ?>= \r2 ->
        return_P (mappend r1 r2) )

instance (Monad m, Proxy p, Monoid b') => MonadPlus (RespondT p a' a b' m) where
    mzero = empty
    mplus = (<|>)

-- | Convert a 'RespondT' \'@K@\'leisli arrow into a proxy
runRespondK :: (q -> RespondT p a' a b' m b) -> (q -> p a' a b' b m b')
runRespondK k q = runRespondT (k q)
{-# INLINABLE runRespondK #-}

-- | 'ProduceT' is isomorphic to \"ListT done right\"
type ProduceT p = RespondT p C () ()

-- | A monad transformer over a proxy's upstream output
newtype RequestT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b' b m a' =
    RequestT { runRequestT :: p a' a b' b m a }

instance (Monad m, Proxy p) => Functor (RequestT p a b' b m) where
    fmap f p = RequestT (runRequestT p //< \a -> request (f a))

instance (Monad m, Proxy p) => Applicative (RequestT p a b' b m) where
    pure a = RequestT (request a)
    mf <*> mx = RequestT (
        runRequestT mf //< \f ->
        runRequestT mx //< \x ->
        request (f x) )

instance (Monad m, Proxy p) => Monad (RequestT p a b' b m) where
    return a = RequestT (request a)
    m >>= f  = RequestT (runRequestT m //< \a -> runRequestT (f a))

instance (Proxy p) => MonadTrans (RequestT p a' a b') where
    lift m = RequestT (lift_P m ?>= \a -> request a)

instance (MonadIO m, Proxy p) => MonadIO (RequestT p a b' b m) where
    liftIO m = lift (liftIO m)

instance (Monad m, Proxy p, Monoid a)
       => Alternative (RequestT p a b' b m) where
    empty = RequestT (return_P mempty)
    p1 <|> p2 = RequestT (
        runRequestT p1 ?>= \r1 ->
        runRequestT p2 ?>= \r2 ->
        return_P (mappend r1 r2) )

instance (Monad m, Proxy p, Monoid a) => MonadPlus (RequestT p a b' b m) where
    mzero = empty
    mplus = (<|>)

-- | Convert a 'RequestT' \'@K@\'leisli arrow into a proxy
runRequestK :: (q -> RequestT p a b' b m a') -> (q -> p a' a b' b m a)
runRequestK k q = runRequestT (k q)
{-# INLINABLE runRequestK #-}

-- | 'CoProduceT' is isomorphic to \"ListT done right\"
type CoProduceT p = RequestT p () () C

{-| Non-deterministically choose from all values in the given list

> mappend <$> eachS xs <*> eachS ys = eachS (mappend <$> xs <*> ys)
>
> eachS (pure mempty) = pure mempty
-}
eachS :: (Monad m, Proxy p) => [b] -> ProduceT p m b
eachS bs = RespondT (fromListS bs ())
{-# INLINABLE eachS #-}

{-| Non-deterministically choose from all values in the given list

> mappend <$> eachC xs <*> eachC ys = eachC (mappend <$> xs <*> ys)
>
> eachC (pure mempty) = pure mempty
-}
eachC :: (Monad m, Proxy p) => [a'] -> CoProduceT p m a'
eachC a's = RequestT (fromListC a's ())
{-# INLINABLE eachC #-}

-- | Non-deterministically choose from all values in the given range
rangeS :: (Enum b, Ord b, Monad m, Proxy p) => b -> b -> ProduceT p m b
rangeS b1 b2 = RespondT (enumFromToS b1 b2 ())
{-# INLINABLE rangeS #-}

-- | Non-deterministically choose from all values in the given range
rangeC
    :: (Enum a', Ord a', Monad m, Proxy p) => a' -> a' -> CoProduceT p m a'
rangeC a'1 a'2 = RequestT (enumFromToC a'1 a'2 ())
{-# INLINABLE rangeC #-}
