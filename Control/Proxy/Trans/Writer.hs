{-| This module provides the proxy transformer equivalent of 'WriterT'.

    This module is even stricter than @Control.Monad.Trans.Writer.Strict@ by
    being strict in the accumulated monoid. 

    The underlying implementation uses the state monad to avoid quadratic blowup
    from left-associative binds.
-}

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Trans.Writer (
    -- * WriterP
    WriterP,
    writerP,
    runWriterP,
    runWriterK,
    execWriterP,
    execWriterK,

    -- * Writer operations
    writer,
    writerT,
    tell,
    censor,

    -- * Folds
    foldD,
    allD,
    allD_,
    anyD,
    anyD_,
    sumD,
    productD,
    lengthD,
    headD,
    headD_,
    lastD,
    toListD,
    foldrD,

    -- * Re-exports
    module Data.Monoid
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
import Data.Monoid (
    Monoid(mempty, mappend),
    Endo(Endo, appEndo),
    All(All, getAll),
    Any(Any, getAny),
    Sum(Sum, getSum),
    Product(Product, getProduct),
    First(First, getFirst),
    Last(Last, getLast) )

-- | The strict 'Writer' proxy transformer
newtype WriterP w p a' a b' b (m :: * -> *) r
    = WriterP { unWriterP :: w -> p (a', w) (a, w) (b', w) (b, w) m (r, w) }

instance (Monad m, Proxy p) => Functor (WriterP w p a' a b' b m) where
    fmap f p = WriterP (\w0 ->
        unWriterP p w0 ?>= \(x, w1) ->
        return_P (f x, w1) )

instance (Monad m, Proxy p) => Applicative (WriterP w p a' a b' b m) where
    pure      = return
    fp <*> xp = WriterP (\w0 ->
        unWriterP fp w0 ?>= \(f, w1) ->
        unWriterP xp w1 ?>= \(x, w2) ->
        return_P (f x, w2) )

instance (Monad m, Proxy p) => Monad (WriterP w p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (Proxy p) => MonadTrans (WriterP w p a' a b' b) where
    lift = lift_P

instance (Proxy p) => MFunctor (WriterP w p a' a b' b) where
    hoist = hoist_P

instance (MonadIO m, Proxy p) => MonadIO (WriterP w p a' a b' b m) where
    liftIO = liftIO_P

instance (Monad m, MonadPlusP p) => Alternative (WriterP w p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, MonadPlusP p) => MonadPlus (WriterP w p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy p) => ProxyInternal (WriterP w p) where
    return_P = \r -> WriterP (\w -> return_P (r, w))
    m ?>= f  = WriterP (\w ->
        unWriterP m w ?>= \(a, w') ->
        unWriterP (f a) w' )

    lift_P m = WriterP (\w -> lift_P (m >>= \r -> return (r, w)))

    hoist_P nat p = WriterP (\w -> hoist_P nat (unWriterP p w))

    liftIO_P m = WriterP (\w -> liftIO_P (m >>= \r -> return (r, w)))

    thread_P p w = WriterP (\w' ->
        ((up ->> thread_P (unWriterP p w') w) >>~ dn) ?>= next )
      where
        up ((a', w1), w2) =
            request ((a', w2 ), w1 ) ?>= \((a , w1'), w2') ->
            respond ((a , w2'), w1') ?>= up
        dn ((b , w1), w2) =
            respond ((b , w2 ), w1 ) ?>= \((b', w1'), w2') ->
            request ((b', w2'), w1') ?>= dn
        next ((r, w1), w2) = return_P ((r, w2), w1)

instance (Proxy p) => Proxy (WriterP w p) where
    fb' ->> p = WriterP (\w ->
        (\(b', w') -> unWriterP (fb' b') w') ->> unWriterP p w )
    p >>~ fb  = WriterP (\w ->
        unWriterP p w >>~ (\(b, w') -> unWriterP (fb b) w') )

    request = \a' -> WriterP (\w -> request (a', w))
    respond = \b  -> WriterP (\w -> respond (b , w))

    fb' >\\ p = WriterP (\w ->
        (\(b', w') -> unWriterP (fb' b') w') >\\ unWriterP p w )
    p //> fb  = WriterP (\w ->
        unWriterP p w //> (\(b, w') -> unWriterP (fb b) w') )

instance (MonadPlusP p) => MonadPlusP (WriterP w p) where
    mzero_P       = WriterP (\_ -> mzero_P)
    mplus_P m1 m2 = WriterP (\w -> mplus_P (unWriterP m1 w) (unWriterP m2 w))

instance ProxyTrans (WriterP w) where
    liftP m = WriterP (thread_P m)

instance PFunctor (WriterP w) where
    hoistP nat p = WriterP (\s -> nat (unWriterP p s))

-- | Create a 'WriterP' from a proxy that generates a result and a monoid
writerP
    :: (Monad m, Proxy p, Monoid w)
    => p a' a b' b m (r, w) -> WriterP w p a' a b' b m r
writerP p = WriterP (\w ->
    thread_P p w ?>= \((r, w2), w1) ->
    let w' = mappend w1 w2
    in  w' `seq` return_P (r, w') )
{-# INLINABLE writerP #-}

-- | Run a 'WriterP' computation, producing the final result and monoid
runWriterP
    :: (Monad m, Proxy p, Monoid w)
    => WriterP w p a' a b' b m r -> p a' a b' b m (r, w)
runWriterP p = up >\\ unWriterP p mempty //> dn
  where
    up (a', w) =
        request a' ?>= \a  ->
        return_P (a , w)
    dn (b , w) =
        respond b  ?>= \b' ->
        return_P (b', w) 
{-# INLINABLE runWriterP #-}

-- | Run a 'WriterP' \'@K@\'leisli arrow, producing the final result and monoid
runWriterK
    :: (Monad m, Proxy p, Monoid w)
    => (q -> WriterP w p a' a b' b m r) -> (q -> p a' a b' b m (r, w))
runWriterK k q = runWriterP (k q)
{-# INLINABLE runWriterK #-}

-- | Evaluate a 'WriterP' computation, but discard the final result
execWriterP
    :: (Monad m, Proxy p, Monoid w)
    => WriterP w p a' a b' b m r -> p a' a b' b m w
execWriterP m = runWriterP m ?>= \(_, w) -> return_P w
{-# INLINABLE execWriterP #-}

-- | Evaluate a 'WriterP' \'@K@\'leisli arrow, but discard the final result
execWriterK
    :: (Monad m, Proxy p, Monoid w)
    => (q -> WriterP w p a' a b' b m r) -> (q -> p a' a b' b m w)
execWriterK k q= execWriterP (k q)
{-# INLINABLE execWriterK #-}

-- | Convert a Writer to a 'WriterP'
writer :: (Monad m, Proxy p, Monoid w) => (r, w) -> WriterP w p a' a b' b m r
writer x = writerP (return_P x)

-- | Convert a WriterT to a 'WriterP'
writerT :: (Monad m, Proxy p, Monoid w) => m (r, w) -> WriterP w p a' a b' b m r
writerT m = writerP (lift_P m)

-- | Add a value to the monoid
tell :: (Monad m, Proxy p, Monoid w) => w -> WriterP w p a' a b' b m ()
tell w' = WriterP (\w ->
    let w'' = mappend w w' in w'' `seq` return_P ((), w''))
{-# INLINABLE tell #-}

-- | Modify the result of a writer computation
censor
    :: (Monad m, Proxy p, Monoid w)
    => (w -> w) -> WriterP w p a' a b' b m r -> WriterP w p a' a b' b m r
censor f p = WriterP (\w0 ->
    unWriterP p w0 ?>= \(r, w1) ->
    return_P (r, f w1) )
{-# INLINABLE censor #-}

{-| Strict fold over values flowing \'@D@\'ownstream.

> foldD f >-> foldD g = foldD (f <> g)
>
> foldD mempty = idPull
-}
foldD
    :: (Monad m, Proxy p, Monoid w) => (a -> w) -> x -> WriterP w p x a x a m r
foldD f = go where
    go x = do
        a <- request x
        tell (f a)
        x2 <- respond a
        go x2
{-# INLINABLE foldD #-}

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate
-}
allD :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP All p x a x a m r
allD predicate = foldD (All . predicate)
{-# INLINABLE allD #-}

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate

    'allD_' terminates on the first value that fails the predicate
-}
allD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP All p x a x a m ()
allD_ predicate = go where
    go x = do
        a <- request x
        if (predicate a)
            then do
                x2 <- respond a
                go x2
            else tell (All False)
{-# INLINABLE allD_ #-}

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies the
    predicate
-}
anyD :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP Any p x a x a m r
anyD predicate = foldD (Any . predicate)
{-# INLINABLE anyD #-}

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies the
    predicate

    'anyD_' terminates on the first value that satisfies the predicate
-}
anyD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP Any p x a x a m ()
anyD_ predicate = go where
    go x = do
        a <- request x
        if (predicate a)
            then tell (Any True)
            else do
                x2 <- respond a
                go x2
{-# INLINABLE anyD_ #-}

-- | Compute the 'Sum' of all values that flow \'@D@\'ownstream
sumD :: (Monad m, Proxy p, Num a) => x -> WriterP (Sum a) p x a x a m r
sumD = foldD Sum
{-# INLINABLE sumD #-}

-- | Compute the 'Product' of all values that flow \'@D@\'ownstream
productD :: (Monad m, Proxy p, Num a) => x -> WriterP (Product a) p x a x a m r
productD = foldD Product
{-# INLINABLE productD #-}

-- | Count how many values flow \'@D@\'ownstream
lengthD :: (Monad m, Proxy p) => x -> WriterP (Sum Int) p x a x a m r
lengthD = foldD (\_ -> Sum 1)
{-# INLINABLE lengthD #-}

-- | Retrieve the first value going \'@D@\'ownstream
headD :: (Monad m, Proxy p) => x -> WriterP (First a) p x a x a m r
headD = foldD (First . Just)
{-# INLINABLE headD #-}

{-| Retrieve the first value going \'@D@\'ownstream

    'headD_' terminates on the first value it receives
-}
headD_ :: (Monad m, Proxy p) => x -> WriterP (First a) p x a x a m ()
headD_ x = do
    a <- request x
    tell $ First (Just a)
{-# INLINABLE headD_ #-}

-- | Retrieve the last value going \'@D@\'ownstream
lastD :: (Monad m, Proxy p) => x -> WriterP (Last a) p x a x a m r
lastD = foldD (Last . Just)
{-# INLINABLE lastD #-}

-- | Fold the values flowing \'@D@\'ownstream into a list
toListD :: (Monad m, Proxy p) => x -> WriterP [a] p x a x a m r
toListD = foldD (\x -> [x])
{-# INLINABLE toListD #-}

{-| Fold equivalent to 'foldr'

    To see why, consider this isomorphic type for 'foldr':

> foldr :: (a -> b -> b) -> [a] -> Endo b
-}
foldrD
    :: (Monad m, Proxy p)
    => (a -> b -> b) -> x -> WriterP (Endo b) p x a x a m r
foldrD step = foldD (Endo . step)
{-# INLINABLE foldrD #-}
