-- | This module provides the proxy transformer equivalent of 'EitherT'.

{-# LANGUAGE KindSignatures, CPP #-}

module Control.Proxy.Trans.Either (
    -- * EitherP
    EitherP(..),
    runEitherK,

    -- * Either operations
    left,
    right,

    -- * Symmetric monad
    -- $symmetry
    throw,
    catch,
    handle,
    fmapL
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(hoist))
import Control.PFunctor (PFunctor(hoistP))
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif
import Data.Monoid (Monoid(mempty, mappend))

-- | The 'Either' proxy transformer
newtype EitherP e p a' a b' b (m :: * -> *) r
  = EitherP { runEitherP :: p a' a b' b m (Either e r) }

instance (MonadP            p )
       => MonadP (EitherP e p) where
    return_P = \r -> EitherP (return_P (Right r))   -- right
    m ?>= f = EitherP (
        runEitherP m ?>= \e ->
        runEitherP (case e of
            Left  l -> EitherP (return_P (Left l))  -- left l
            Right r -> f    r ) )

instance (Proxy              p, Monad m)
       => Functor (EitherP e p a' a b' b m) where
    fmap f p = EitherP (
        runEitherP p ?>= \e ->
        return_P (case e of
            Left  l -> Left l
            Right r -> Right (f r) ) )
 -- fmap f = EitherP . liftM (fmap f) . runEitherP

instance (Proxy                  p, Monad m)
       => Applicative (EitherP e p a' a b' b m) where
    pure = return
    fp <*> xp = EitherP (
        runEitherP fp ?>= \e1 ->
        case e1 of
            Left  l -> return_P (Left l)
            Right f ->
                 runEitherP xp ?>= \e2 ->
                 return_P (case e2 of
                      Left l  -> Left  l
                      Right x -> Right (f x) ) )
 -- fp <*> xp = EitherP ((<*>) <$> (runEitherP fp) <*> (runEitherP xp))

instance (Proxy            p, Monad m)
       => Monad (EitherP e p a' a b' b m) where
    return = return_P
    (>>=) = (?>=)

instance (Proxy                  p, Monad m, Monoid e)
       => Alternative (EitherP e p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Proxy                 p, Monoid e)
       => MonadPlusP (EitherP e p) where
    mzero_P = EitherP (return_P (Left mempty))
    mplus_P p1 p2 = EitherP (
        runEitherP p1 ?>= \e1 ->
        case e1 of
            Right r  -> return_P (Right r)
            Left  l1 ->
                runEitherP p2 ?>= \e2 ->
                case e2 of
                    Right r  -> return_P (Right r)
                    Left  l2 -> return_P (Left (mappend l1 l2)) )

instance (Proxy                p, Monad m, Monoid e)
       => MonadPlus (EitherP e p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (MonadTransP            p )
       => MonadTransP (EitherP e p) where
    lift_P m = EitherP (lift_P (m >>= \x -> return (Right x)))
 -- lift = EitherP . lift . liftM Right

instance (Proxy                 p )
       => MonadTrans (EitherP e p a' a b' b) where
    lift = lift_P

instance (MonadIOP            p )
       => MonadIOP (EitherP e p) where
    liftIO_P m = EitherP (liftIO_P (m >>= \x -> return (Right x)))
 -- liftIO = EitherP . liftIO . liftM Right

instance (MonadIOP           p, MonadIO m)
       => MonadIO (EitherP e p a' a b' b m) where
    liftIO = liftIO_P

instance (MFunctorP            p )
       => MFunctorP (EitherP e p) where
    hoist_P nat p = EitherP (hoist_P nat (runEitherP p))
 -- hoist nat = EitherP . hoist nat . runEitherP

instance (Proxy               p )
       => MFunctor (EitherP e p a' a b' b) where
    hoist = hoist_P

instance (Proxy            p )
       => Proxy (EitherP e p) where
    fb' ->> p = EitherP ((\b' -> runEitherP (fb' b')) ->> runEitherP p)

    p >>~ fb  = EitherP (runEitherP p >>~ (\b -> runEitherP (fb b)))

    request = \a' -> EitherP (request a' ?>= \a  -> return_P (Right a ))
    respond = \b  -> EitherP (respond b  ?>= \b' -> return_P (Right b'))

instance ProxyTrans (EitherP e) where
    liftP p = EitherP (p ?>= \x -> return_P (Right x))
 -- liftP = EitherP . liftM Right

instance PFunctor (EitherP e) where
    hoistP nat = EitherP . nat . runEitherP

-- | Run an 'EitherP' \'@K@\'leisi arrow, returning either a 'Left' or 'Right'
runEitherK
 :: (q -> EitherP e p a' a b' b m r) -> (q -> p a' a b' b m (Either e r))
runEitherK p q = runEitherP (p q)
-- runEitherK = (runEitherP .)

-- | Abort the computation and return a 'Left' result
left :: (Monad m, Proxy p) => e -> EitherP e p a' a b' b m r
left e = EitherP (return_P (Left e))
-- left = EitherP . return . Left

-- | Synonym for 'return'
right :: (Monad m, Proxy p) => r -> EitherP e p a' a b' b m r
right r = EitherP (return_P (Right r))
-- right = EitherP . return . Right

{- $symmetry
    'EitherP' forms a second symmetric monad over the left type variable.

    'throw' is symmetric to 'return'

    'catch' is symmetric to ('>>=')

    These two functions obey the monad laws:

> catch m throw = m
>
> catch (throw e) f = f e
>
> catch (catch m f) g = catch m (\e -> catch (f e) g)
-}

-- | Synonym for 'left'
throw :: (Monad m, Proxy p) => e -> EitherP e p a' a b' b m r
throw = left

-- | Resume from an aborted operation
catch
 :: (Monad m, Proxy p)
 => EitherP e p a' a b' b m r        -- ^ Original computation
 -> (e -> EitherP f p a' a b' b m r) -- ^ Handler
 -> EitherP f p a' a b' b m r        -- ^ Handled computation
catch m f = EitherP (
    runEitherP m ?>= \e ->
    runEitherP (case e of
        Left  l -> f     l
        Right r -> right r ))

-- | 'catch' with the arguments flipped
handle
 :: (Monad m, Proxy p)
 => (e -> EitherP f p a' a b' b m r) -- ^ Handler
 -> EitherP e p a' a b' b m r        -- ^ Original computation
 -> EitherP f p a' a b' b m r        -- ^ Handled computation
handle f m = catch m f
-- handle = flip catch

-- | 'fmap' over the \'@L@\' variable
fmapL
 :: (Monad m, Proxy p)
 => (e -> f) -> EitherP e p a' a b' b m r -> EitherP f p a' a b' b m r
fmapL f p = catch p (\e -> throw (f e))
