{-| This module provides the proxy transformer equivalent of 'WriterT'.

    This module is even stricter than @Control.Monad.Trans.Writer.Strict@ by
    being strict in the accumulated monoid. 

    The underlying implementation uses the state monad to avoid quadratic blowup
    from left-associative binds. -}

{-# LANGUAGE KindSignatures #-}

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
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))
import Data.Monoid (Monoid(mempty, mappend))

-- | The strict 'Writer' proxy transformer
newtype WriterP w p a' a b' b (m :: * -> *) r
  = WriterP { unWriterP :: w -> p a' a b' b m (r, w) }

instance (Proxy              p, Monad m)
       => Functor (WriterP w p a' a b' b m) where
    fmap f p = WriterP (\w0 ->
        unWriterP p w0 ?>= \(x, w1) ->
        return_P (f x, w1) )

instance (Proxy                  p, Monad m)
       => Applicative (WriterP w p a' a b' b m) where
    pure = return
    fp <*> xp = WriterP (\w0 ->
        unWriterP fp w0 ?>= \(f, w1) ->
        unWriterP xp w1 ?>= \(x, w2) ->
        return_P (f x, w2) )
 -- (<*>) = ap

instance (Proxy            p, Monad m)
       => Monad (WriterP w p a' a b' b m) where
    return = return_P
    (>>=) = (?>=)

instance (MonadPlusP             p, Monad m)
       => Alternative (WriterP w p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP            p )
       => MonadPlusP (WriterP w p) where
    mzero_P       = WriterP (\_ -> mzero_P)
    mplus_P m1 m2 = WriterP (\w -> mplus_P (unWriterP m1 w) (unWriterP m2 w))

instance (MonadPlusP           p, Monad m)
       => MonadPlus (WriterP w p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy                 p )
       => MonadTrans (WriterP w p a' a b' b) where
    lift = lift_P

instance (MonadIOP            p )
       => MonadIOP (WriterP w p) where
    liftIO_P m = WriterP (\w -> liftIO_P (m >>= \r -> return (r, w)))

instance (MonadIOP           p, MonadIO m)
       => MonadIO (WriterP w p a' a b' b m) where
    liftIO = liftIO_P

instance (MFunctorP            p )
       => MFunctorP (WriterP w p) where
    mapT_P nat p = WriterP (\w -> mapT_P nat (unWriterP p w))
 -- mapT nat = WriterP . fmap (mapT nat) . unWriterP

instance (MFunctorP           p )
       => MFunctor (WriterP w p a' a b' b) where
    mapT = mapT_P

instance (Proxy            p )
       => Proxy (WriterP w p) where
    idT = \a' -> WriterP (\_ -> idT a')
    p1 >-> p2 = \c'1 -> WriterP (\w ->
        ((\b' -> unWriterP (p1 b') w) >-> (\c'2 -> unWriterP (p2 c'2) w)) c'1 )
 {- p1 >-> p2 = \c' -> WriterP $ \w ->
        ((`unWriterP` w) . p1 >-> (`unWriterP` w) . p2) c' -}

    request = \a' -> WriterP (\w -> request a' ?>= \a  -> return_P (a,  w))
    respond = \b  -> WriterP (\w -> respond b  ?>= \b' -> return_P (b', w))

    return_P = \r -> WriterP (\w -> return_P (r, w))
    m ?>= f  = WriterP (\w ->
        unWriterP m w ?>= \(a, w') ->
        unWriterP (f a) w' )

    lift_P m = WriterP (\w -> lift_P (m >>= \r -> return (r, w)))

instance ProxyTrans (WriterP w) where
    liftP m = WriterP (\w -> m ?>= \r -> return_P (r, w))

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
 :: (Proxy p, Monad m, Monoid w)
 => WriterP w p a' a b' b m r -> p a' a b' b m w
execWriterP m = runWriterP m ?>= \(_, w) -> return_P w
-- execWriterP m = liftM snd $ runWriterP m

-- | Evaluate a 'WriterP' \'@K@\'leisli arrow, but discard the final result
execWriterK
 :: (Proxy p, Monad m, Monoid w)
 => (q -> WriterP w p a' a b' b m r) -> (q -> p a' a b' b m w)
execWriterK k q= execWriterP (k q)

-- | Add a value to the monoid
tell :: (Proxy p, Monad m, Monoid w) => w -> WriterP w p a' a b' b m ()
tell w' = WriterP (\w -> let w'' = mappend w w' in w'' `seq` return_P ((), w''))

-- | Modify the result of a writer computation
censor
 :: (Proxy p, Monad m, Monoid w)
 => (w -> w) -> WriterP w p a' a b' b m r -> WriterP w p a' a b' b m r
censor f p = WriterP (\w0 ->
    unWriterP p w0 ?>= \(r, w1) ->
    return_P (r, f w1) )
-- censor f = WriterP . fmap (liftM (\(r, w) -> (r, f w))) . unWriterP
