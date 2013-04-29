-- | This module provides the proxy transformer equivalent of 'EitherT'.

{-# LANGUAGE KindSignatures, CPP #-}

module Control.Proxy.Trans.Either (
    -- * EitherP
    EitherP(..),

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
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class (
    Proxy(request, respond, (->>), (>>~), (>\\), (//>), turn),
    ProxyInternal(return_P, (?>=), lift_P, liftIO_P, hoist_P, thread_P),
    MonadPlusP(mzero_P, mplus_P) )
import Control.Proxy.Morph (PFunctor(hoistP), PMonad(embedP))
import Control.Proxy.Trans (ProxyTrans(liftP))
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif
import Data.Monoid (Monoid(mempty, mappend))

-- | The 'Either' proxy transformer
newtype EitherP e p a' a b' b (m :: * -> *) r
    = EitherP { runEitherP :: p a' a b' b m (Either e r) }

instance (Monad m, Proxy p) => Functor (EitherP e p a' a b' b m) where
    fmap f p = EitherP (
        runEitherP p ?>= \e ->
        return_P (case e of
            Left  l -> Left l
            Right r -> Right (f r) ) )

instance (Monad m, Proxy p) => Applicative (EitherP e p a' a b' b m) where
    pure      = return
    fp <*> xp = EitherP (
        runEitherP fp ?>= \e1 ->
        case e1 of
            Left  l -> return_P (Left l)
            Right f ->
                 runEitherP xp ?>= \e2 ->
                 return_P (case e2 of
                      Left l  -> Left  l
                      Right x -> Right (f x) ) )

instance (Monad m, Proxy p) => Monad (EitherP e p a' a b' b m) where
    return = return_P
    (>>=)  = (?>=)

instance (Proxy p) => MonadTrans (EitherP e p a' a b' b) where
    lift = lift_P

instance (Proxy p) => MFunctor (EitherP e p a' a b' b) where
    hoist = hoist_P

instance (MonadIO m, Proxy p) => MonadIO (EitherP e p a' a b' b m) where
    liftIO = liftIO_P

instance (Monad m, Proxy p, Monoid e)
       => Alternative (EitherP e p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, Proxy p, Monoid e)
       => MonadPlus (EitherP e p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

instance (Proxy p) => ProxyInternal (EitherP e p) where
    return_P = \r -> EitherP (return_P (Right r))
    m ?>= f = EitherP (
        runEitherP m ?>= \e ->
        case e of
            Left  l -> return_P (Left l)
            Right r -> runEitherP (f r) )

    lift_P m = EitherP (lift_P (m >>= \x -> return (Right x)))

    hoist_P nat p = EitherP (hoist_P nat (runEitherP p))

    liftIO_P m = EitherP (liftIO_P (m >>= \x -> return (Right x)))

    thread_P p s = EitherP (
        thread_P (runEitherP p) s ?>= \(x, s') ->
        return_P (case x of
            Left  e -> Left e
            Right r -> Right (r, s') ) )

instance (Proxy p) => Proxy (EitherP e p) where
    fb' ->> p = EitherP ((\b' -> runEitherP (fb' b')) ->> runEitherP p)
    p >>~ fb  = EitherP (runEitherP p >>~ (\b -> runEitherP (fb b)))
    request = \a' -> EitherP (request a' ?>= \a  -> return_P (Right a ))
    respond = \b  -> EitherP (respond b  ?>= \b' -> return_P (Right b'))

    p //> fb = EitherP (
        (runEitherP p >>~ absorb) //> \b -> runEitherP (fb b) )
      where
        absorb b =
            respond b ?>= \x ->
            case x of
                Left  e  -> return_P (Left e)
                Right b' ->
                    request b' ?>= \b2 ->
                    absorb b2
    fb' >\\ p = EitherP (
        (\b' -> runEitherP (fb' b')) >\\ (absorb ->> runEitherP p) )
      where
        absorb b' =
            request b' ?>= \x ->
            case x of
                Left  e -> return_P (Left e)
                Right b ->
                    respond b ?>= \b'2 ->
                    absorb b'2

    turn p = EitherP (turn (runEitherP p))

instance (Proxy p, Monoid e) => MonadPlusP (EitherP e p) where
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

instance ProxyTrans (EitherP e) where
    liftP p = EitherP (p ?>= \x -> return_P (Right x))

instance PFunctor (EitherP e) where
    hoistP nat p = EitherP (nat (runEitherP p))

instance PMonad (EitherP e) where
    embedP nat p = EitherP (
        runEitherP (nat (runEitherP p)) ?>= \x ->
        return_P (case x of
            Left         e  -> Left e
            Right (Left  e) -> Left e
            Right (Right a) -> Right a ) )

-- | Abort the computation and return a 'Left' result
left :: (Monad m, Proxy p) => e -> EitherP e p a' a b' b m r
left e = EitherP (return_P (Left e))
{-# INLINABLE left #-}

-- | Synonym for 'return'
right :: (Monad m, Proxy p) => r -> EitherP e p a' a b' b m r
right r = EitherP (return_P (Right r))
{-# INLINABLE right #-}

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
{-# INLINABLE throw #-}

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
{-# INLINABLE catch #-}

-- | 'catch' with the arguments flipped
handle
    :: (Monad m, Proxy p)
    => (e -> EitherP f p a' a b' b m r) -- ^ Handler
    -> EitherP e p a' a b' b m r        -- ^ Original computation
    -> EitherP f p a' a b' b m r        -- ^ Handled computation
handle f m = catch m f
{-# INLINABLE handle #-}

-- | 'fmap' over the \'@L@\'eft variable
fmapL
    :: (Monad m, Proxy p)
    => (e -> f) -> EitherP e p a' a b' b m r -> EitherP f p a' a b' b m r
fmapL f p = catch p (\e -> throw (f e))
{-# INLINABLE fmapL #-}
