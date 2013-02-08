{-| This module implements \"ListT done right\" in terms of proxies.

    The 'RespondT' monad transformer is the 'ListT' monad transformer over the
    downstream output type.  Each 'respond' corresponds to an element of the
    list.  The monad bind operation non-deterministically selects one of the
    previous 'respond's as input.

    Symmetrically, the 'RequestT' monad transformer is the 'ListT' monad
    transformer over the upstream output type.  Each 'request' corresponds to an
    element of the list.  The monad bind operation non-deterministically selects
    one of the previous 'request's as input.

    The 'RespondT' Kleisli category corresponds to the \"respond category\" of
    the 'Interact' class.  Symmetrically, the 'RequestT' Kleisli category
    corresponds to the \"request category\" of the 'Interact' class.  These two
    monad transformers let you use @do@ notation to sweeten manipulations in
    the two 'Interact' categories.

    Unlike 'ListT' from @transformers@, these monad transformers are correct by
    construction and do not assume the base monad is commutative.
-}

{-# LANGUAGE KindSignatures #-}

module Control.Monad.Trans.Interact (
    -- * Respond Monad Transformer
    RespondT(..),
    ProduceT,
    runProduceS,

    -- * Request Monad Transformer
    RequestT(..),
    CoProduceT,
    runCoProduceC
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.Synonym (C, Producer, CoProducer)

-- | A monad transformer over a proxy's downstream end
newtype RespondT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a b' m b =
    RespondT { runRespondT :: p a' a b' b m b' }

instance (Monad m, Interact p) => Functor (RespondT p a' a b' m) where
    fmap f p = RespondT (runRespondT p //> \a -> respond (f a))

instance (Monad m, Interact p) => Applicative (RespondT p a' a b' m) where
    pure a = RespondT (respond a)

    mf <*> mx = RespondT (
        runRespondT mf //> \f ->
        runRespondT mx //> \x ->
        respond (f x) )

instance (Monad m, Interact p) => Monad (RespondT p a' a b' m) where
    return a = RespondT (respond a)

    m >>= f  = RespondT (runRespondT m //> \a -> runRespondT (f a))

instance (Interact p) => MonadTrans (RespondT p a' a b') where
    lift m = RespondT (lift_P m ?>= \a -> respond a)

instance (MonadIO m, Interact p) => MonadIO (RespondT p a' a b' m) where
    liftIO m = lift (liftIO m)

-- | 'ProduceT' is isomorphic to \"ListT done right\"
type ProduceT p = RespondT p C () ()

-- | Convert a 'ProduceT' into a 'Producer' suitable for composition
runProduceS :: ProduceT p m b -> () -> Producer p b m ()
runProduceS p () = runRespondT p

-- | A monad transformer over a proxy's upstream end
newtype RequestT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b' b m a' =
    RequestT { runRequestT :: p a' a b' b m a }

instance (Monad m, Interact p) => Functor (RequestT p a b' b m) where
    fmap f p = RequestT (runRequestT p //< \a -> request (f a))

instance (Monad m, Interact p) => Applicative (RequestT p a b' b m) where
    pure a = RequestT (request a)

    mf <*> mx = RequestT (
        runRequestT mf //< \f ->
        runRequestT mx //< \x ->
        request (f x) )

instance (Monad m, Interact p) => Monad (RequestT p a b' b m) where
    return a = RequestT (request a)

    m >>= f  = RequestT (runRequestT m //< \a -> runRequestT (f a))

instance (Interact p) => MonadTrans (RequestT p a' a b') where
    lift m = RequestT (lift_P m ?>= \a -> request a)

instance (MonadIO m, Interact p) => MonadIO (RequestT p a b' b m) where
    liftIO m = lift (liftIO m)

-- | 'CoProduceT' is isomorphic to \"ListT done right\"
type CoProduceT p = RequestT p () () C

-- | Convert a 'CoProduceT' into a 'CoProducer' suitable for composition
runCoProduceC :: CoProduceT p m a' -> () -> CoProducer p a' m ()
runCoProduceC p () = runRequestT p
