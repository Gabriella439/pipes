{-| This module provides the proxy transformer equivalent of 'CodensityT'.

    The base 'Proxy' implementations suffer a quadratic time complexity if
    you repeatedly left-associate the monad bind operation.  You can recover
    linear time complexity just by adding 'runCodensityK' right after
    'runProxy', which transforms the base 'Proxy' implementation to use
    continuation-passing style:

> -- Before:
> runProxy $ ...
>
> -- After:
> runProxy $ runCodensityK $ ...

    Everything will still type-check if you you wrote your code to be
    polymorphic over the base 'Proxy'.

    Note that even though 'CodensityP' has better time complexity for
    left-associated binds, it has worse constant factors for everything else
    (about 6x slower on pure benchmarks), because:

    * You cannot optimize it using rewrite rules

    * It has a slower composition operation

    So only use it if you actually need it, which is typically only the case if
    you left associate your monad binds on the order of hundreds of times.  Even
    better: only wrap the problematic portions of the pipeline in
    'runCodensityK' so that the performance of the rest of the pipeline does not
    suffer.
-}

{-# LANGUAGE KindSignatures, PolymorphicComponents #-}

module Control.Proxy.Trans.Codensity (
    -- * Codensity Proxy Transformer
    CodensityP,
    runCodensityP,
    runCodensityK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (
    Proxy(request, respond, (->>), (>>~), (>\\), (//>)),
    ProxyInternal(return_P, (?>=), lift_P, liftIO_P, hoist_P, thread_P),
    MonadPlusP(mzero_P, mplus_P) )
import Control.Proxy.Morph (PFunctor(hoistP))
import Control.Proxy.Trans (ProxyTrans(liftP))

-- | The 'Codensity' proxy transformer
newtype CodensityP p a' a b' b (m :: * -> *) r
    = CodensityP { unCodensityP
        :: forall x . (Monad m, Proxy p)
         => (r -> p a' a b' b m x) -> p a' a b' b m x }
{- The type class instances only satisfy their laws if you hide the constructor
   for 'CodensityP'.

   Normally you would not have to hide it and you could rely on parametricity to
   guarantee that 'CodensityP p' is isomorphic to 'p'.  However, the 'MFunctor'
   and 'PFunctor' type classes require including class constraints within the
   constructor, which breaks parametricity and makes it possible to define
   'CodensityP' values which break the laws for the following type class
   instances.
-}

instance (Monad m, Proxy p) => Functor (CodensityP p a' a b' b m) where
    fmap f p = CodensityP (\k ->
        unCodensityP p    (\a ->
        k (f a)) )

instance (Monad m, Proxy p) => Applicative (CodensityP p a' a b' b m) where
    pure = return
    fp <*> xp = CodensityP (\k ->
        unCodensityP fp    (\f ->
        unCodensityP xp    (\x ->
        k (f x) ) ) )

instance (Monad m, Proxy p) => Monad (CodensityP p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (Proxy p) => MonadTrans (CodensityP p a' a b' b) where
    lift = lift_P

instance (Proxy p) => MFunctor (CodensityP p a' a b' b) where
    hoist = hoist_P

instance (MonadIO m, Proxy p) => MonadIO (CodensityP p a' a b' b m) where
    liftIO = liftIO_P

instance (Monad m, MonadPlusP p) => Alternative (CodensityP p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, MonadPlusP p) => MonadPlus (CodensityP p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy p) => ProxyInternal (CodensityP p) where
    return_P = \r -> CodensityP (\k -> k r)
    m ?>= f  = CodensityP  (\k ->
        unCodensityP  m    (\a ->
        unCodensityP (f a)   k ) )

    lift_P m = CodensityP (\k -> lift_P m ?>= k)

    hoist_P nat p = CodensityP (\k ->
        hoist_P nat (unCodensityP p return_P) ?>= k)

    liftIO_P m = CodensityP (\k -> liftIO_P m ?>= k)

    thread_P p s = CodensityP (\k -> thread_P (unCodensityP p return_P) s ?>= k)

instance (MonadPlusP p) => MonadPlusP (CodensityP p) where
    mzero_P       = CodensityP (\_ -> mzero_P)
    mplus_P m1 m2 = CodensityP (\k ->
        mplus_P (unCodensityP m1 k) (unCodensityP m2 k) )

instance (Proxy p) => Proxy (CodensityP p) where
    fb' ->> p = CodensityP (\k ->
        ((\b' -> unCodensityP (fb' b') return_P) ->> unCodensityP p return_P)
            ?>= k )
    p >>~ fb  = CodensityP (\k ->
        (unCodensityP p return_P >>~ (\b -> unCodensityP (fb b) return_P))
            ?>= k )
    request = \a' -> CodensityP (\k -> request a' ?>= k)
    respond = \b  -> CodensityP (\k -> respond b  ?>= k)

    fb' >\\ p = CodensityP (\k ->
        ((\b' -> unCodensityP (fb' b') return_P) >\\ unCodensityP p return_P)
            ?>= k )
    p //> fb  = CodensityP (\k ->
        (unCodensityP p return_P //> (\b -> unCodensityP (fb b) return_P))
            ?>= k )

instance ProxyTrans CodensityP where
    liftP p = CodensityP (\k -> p ?>= k)

instance PFunctor CodensityP where
    hoistP nat p = CodensityP (\k -> nat (unCodensityP p return_P) ?>= k)

-- | Run a 'CodensityP' proxy, converting, converting it back to the base proxy
runCodensityP
    :: (Monad m, Proxy p) => CodensityP p a' a b' b m r -> p a' a b' b m r
runCodensityP p = unCodensityP p return_P
{-# INLINABLE runCodensityP #-}

{-| Run a 'CodensityP' \'@K@\'leisli arrow, converting it back to the base proxy
-}
runCodensityK
    :: (Monad m, Proxy p)
    => (q -> CodensityP p a' a b' b m r) -> (q -> p a' a b' b m r)
runCodensityK k q = runCodensityP (k q)
{-# INLINABLE runCodensityK #-}
