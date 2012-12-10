-- | This module provides the proxy transformer equivalent of 'EitherT'.

{-# LANGUAGE KindSignatures #-}

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
    handle
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(hoist))
import Control.PFunctor (PFunctor(hoistP))
import Control.Proxy.Class
import Control.Proxy.Trans (ProxyTrans(liftP))
import Prelude hiding (catch)

-- | The 'Either' proxy transformer
newtype EitherP e p a' a b' b (m :: * -> *) r
  = EitherP { runEitherP :: p a' a b' b m (Either e r) }

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

instance (MonadPlusP             p, Monad m)
       => Alternative (EitherP e p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlusP            p )
       => MonadPlusP (EitherP e p) where
    mzero_P = EitherP mzero_P
    mplus_P m1 m2 = EitherP (mplus_P (runEitherP m1) (runEitherP m2))

instance (MonadPlusP           p, Monad m)
       => MonadPlus (EitherP e p a' a b' b m) where
    mzero = mzero_P
    mplus = mplus_P

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

instance (Proxy               p )
       => MFunctor (EitherP e p a' a b' b) where
    hoist = hoist_P

instance (Proxy            p )
       => Proxy (EitherP e p) where
    p1 >-> p2 = \c'1 -> EitherP (
        ((\b' -> runEitherP (p1 b')) >-> (\c'2 -> runEitherP (p2 c'2))) c'1 )
 -- p1 >-> p2 = (EitherP .) $ runEitherP . p1 >-> runEitherP . p2

    p1 >~> p2 = \c'1 -> EitherP (
        ((\b' -> runEitherP (p1 b')) >~> (\c'2 -> runEitherP (p2 c'2))) c'1 )
 -- p1 >~> p2 = (EitherP .) $ runEitherP . p1 >~> runEitherP . p2

    request = \a' -> EitherP (request a' ?>= \a  -> return_P (Right a ))
    respond = \b  -> EitherP (respond b  ?>= \b' -> return_P (Right b'))

    return_P = right
    m ?>= f = EitherP (
        runEitherP m ?>= \e ->
        runEitherP (case e of
            Left  l -> left l
            Right r -> f    r ) )

    lift_P m = EitherP (lift_P (m >>= \x -> return (Right x)))
 -- lift = EitherP . lift . liftM Right

    hoist_P nat p = EitherP (hoist_P nat (runEitherP p))
 -- hoist nat = EitherP . hoist nat . runEitherP

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
