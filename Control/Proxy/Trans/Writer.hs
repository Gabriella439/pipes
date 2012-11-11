{-| This module provides the proxy transformer equivalent of 'WriterT'.

    This module is even stricter than @Control.Monad.Trans.Writer.Strict@ by
    being strict in the accumulated monoid. 

    The underlying implementation uses the state monad to avoid quadratic blowup
    from left-associative binds. -}

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Writer (
    -- * WriterP
    WriterP(..),
    runWriterP,
    runWriterK,
    execWriterP,
    execWriterK,
    -- * Writer operations
    tell,
    censor
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT, (>->)),
    InteractId(request, respond),
    MonadProxy(returnP, (?>=)) )
import Control.Proxy.Trans (ProxyTrans(liftP))
import Data.Monoid (Monoid(mempty, mappend))

-- | The strict 'Writer' proxy transformer
newtype WriterP w p a' a b' b (m :: * -> *) r
  = WriterP { unWriterP :: w -> p a' a b' b m (r, w) }

instance (Monad             (p a' a b' b m))
       => Functor (WriterP w p a' a b' b m) where
    fmap f p = WriterP (\w0 -> do
        (x, w1) <- unWriterP p w0
        return (f x, w1) )

instance (Monad                 (p a' a b' b m))
       => Applicative (WriterP w p a' a b' b m) where
    pure = return
    fp <*> xp = WriterP (\w0 -> do
        (f, w1) <- unWriterP fp w0
        (x, w2) <- unWriterP xp w1
        return (f x, w2) )
 -- (<*>) = ap

instance (Monad           (p a' a b' b m))
       => Monad (WriterP w p a' a b' b m) where
    return  = \r -> WriterP (\w -> return (r, w))
    m >>= f = WriterP (\w -> do
        (a, w') <- unWriterP m w
        unWriterP (f a) w' )

instance (MonadPlus             (p a' a b' b m))
       => Alternative (WriterP w p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus           (p a' a b' b m))
       => MonadPlus (WriterP w p a' a b' b m) where
    mzero = WriterP (\_ -> mzero)
    mplus m1 m2 = WriterP (\w -> mplus (unWriterP m1 w) (unWriterP m2 w))

instance (MonadTrans           (p a' a b' b))
       => MonadTrans (WriterP w p a' a b' b) where
    lift m = WriterP (\w -> lift (m >>= \r -> return (r, w)))

instance (MonadIO           (p a' a b' b m))
       => MonadIO (WriterP w p a' a b' b m) where
    liftIO m = WriterP (\w -> liftIO (m >>= \r -> return (r, w)))

instance (MFunctor           (p a' a b' b))
       => MFunctor (WriterP w p a' a b' b) where
    mapT nat p = WriterP (\w -> mapT nat (unWriterP p w))
 -- mapT nat = WriterP . fmap (mapT nat) . unWriterP

instance (Channel            p )
       => Channel (WriterP w p) where
    idT = \a' -> WriterP (\_ -> idT a')
    p1 >-> p2 = \c'1 -> WriterP (\w ->
        ((\b' -> unWriterP (p1 b') w) >-> (\c'2 -> unWriterP (p2 c'2) w)) c'1 )
 {- p1 >-> p2 = \c' -> WriterP $ \w ->
        ((`unWriterP` w) . p1 >-> (`unWriterP` w) . p2) c' -}

instance (InteractId            p, MonadProxy p)
       => InteractId (WriterP w p) where
    request = \a' -> WriterP (\w -> request a' ?>= \a  -> returnP (a,  w))
    respond = \b  -> WriterP (\w -> respond b  ?>= \b' -> returnP (b', w))

instance (MonadProxy            p )
       => MonadProxy (WriterP w p) where
    returnP = \r -> WriterP (\w -> returnP (r, w))
    m ?>= f = WriterP (\w ->
        unWriterP m w ?>= \(a, w') -> 
        unWriterP (f a) w' )

instance ProxyTrans (WriterP w) where
    liftP m = WriterP (\w -> m >>= \r -> return (r, w))

-- | Run a 'WriterP' computation, producing the final result and monoid
runWriterP :: (Monoid w) => WriterP w p a' a b' b m r -> p a' a b' b m (r, w)
runWriterP p = unWriterP p mempty

-- | Run a 'WriterP' \'@K@\'leisli arrow, producing the final result and monoid
runWriterK
 :: (Monoid w)
 => (q -> WriterP w p a' a b' b m r) -> (q -> p a' a b' b m (r, w))
runWriterK k q = runWriterP (k q)
-- runWriterK = (runWriterP . )

-- | Evaluate a 'WriterP' computation, but discard the final result
execWriterP
 :: (Monad (p a' a b' b m), Monoid w)
 => WriterP w p a' a b' b m r -> p a' a b' b m w
execWriterP m = runWriterP m >>= \(_, w) -> return w
-- execWriterP m = liftM snd $ runWriterP m

-- | Evaluate a 'WriterP' \'@K@\'leisli arrow, but discard the final result
execWriterK
 :: (Monad (p a' a b' b m), Monoid w)
 => (q -> WriterP w p a' a b' b m r) -> (q -> p a' a b' b m w)
execWriterK k q= execWriterP (k q)

-- | Add a value to the monoid
tell :: (Monad (p a' a b' b m), Monoid w) => w -> WriterP w p a' a b' b m ()
tell w' = WriterP (\w -> let w'' = mappend w w' in w'' `seq` return ((), w''))

-- | Modify the result of a writer computation
censor
 :: (Monad (p a' a b' b m), Monoid w)
 => (w -> w) -> WriterP w p a' a b' b m r -> WriterP w p a' a b' b m r
censor f p = WriterP (\w0 -> do
    (r, w1) <- unWriterP p w0
    return (r, f w1) )
-- censor f = WriterP . fmap (liftM (\(r, w) -> (r, f w))) . unWriterP
