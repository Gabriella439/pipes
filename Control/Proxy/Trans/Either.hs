-- | This module provides the proxy transformer equivalent of 'EitherT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

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
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT    , (>->)), 
    Request(request, (\>\)), 
    Respond(respond, (/>/)))
import Control.Proxy.Trans (ProxyTrans(liftP))
import Prelude hiding (catch)

-- | The 'Either' proxy transformer
newtype EitherP e p a' a b' b (m :: * -> *) r
  = EitherP { runEitherP :: p a' a b' b m (Either e r) }

instance (Monad (p a' a b' b m)) => Functor (EitherP e p a' a b' b m) where
    fmap = liftM

instance (Monad (p a' a b' b m)) => Applicative (EitherP e p a' a b' b m) where
    pure  = return
    (<*>) = ap

instance (Monad (p a' a b' b m)) => Monad (EitherP e p a' a b' b m) where
    return = right
    m >>= f = EitherP $ do
        e <- runEitherP m
        runEitherP $ case e of
            Left  e -> left e
            Right r -> f    r

instance (MonadPlus (p a' a b' b m))
 => Alternative (EitherP e p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus (p a' a b' b m))
 => MonadPlus (EitherP e p a' a b' b m) where
    mzero = EitherP mzero
    mplus m1 m2 = EitherP $ mplus (runEitherP m1) (runEitherP m2)

instance (MonadTrans (p a' a b' b)) => MonadTrans (EitherP e p a' a b' b) where
    lift = EitherP . lift . liftM Right

instance (MonadIO (p a' a b' b m)) => MonadIO (EitherP e p a' a b' b m) where
    liftIO = EitherP . liftIO . liftM Right

instance (MFunctor (p a' a b' b)) => MFunctor (EitherP e p a' a b' b) where
    mapT nat = EitherP . mapT nat . runEitherP

instance (Channel p) => Channel (EitherP e p) where
    idT = EitherP . idT
    p1 >-> p2 = (EitherP .) $ runEitherP . p1 >-> runEitherP . p2

instance ProxyTrans (EitherP e) where
    liftP = EitherP . liftM Right

-- | Run an 'EitherP' \'@K@\'leisi arrow, returning either a 'Left' or 'Right'
runEitherK
 :: (q -> EitherP e p a' a b' b m r) -> (q -> p a' a b' b m (Either e r))
runEitherK = (runEitherP .)

-- | Abort the computation and return a 'Left' result
left :: (Monad (p a' a b' b m)) => e -> EitherP e p a' a b' b m r
left = EitherP . return . Left

-- | Synonym for 'return'
right :: (Monad (p a' a b' b m)) => r -> EitherP e p a' a b' b m r
right = EitherP . return . Right

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
throw :: (Monad (p a' a b' b m)) => e -> EitherP e p a' a b' b m r
throw = left

-- | Resume from an aborted operation
catch
 :: (Monad (p a' a b' b m))
 => EitherP e p a' a b' b m r        -- ^ Original computation
 -> (e -> EitherP f p a' a b' b m r) -- ^ Handler
 -> EitherP f p a' a b' b m r        -- ^ Handled computation
catch m f = EitherP $ do
    e <- runEitherP m
    runEitherP $ case e of
        Left  e -> f     e
        Right r -> right r

-- | 'catch' with the arguments flipped
handle
 :: (Monad (p a' a b' b m))
 => (e -> EitherP f p a' a b' b m r) -- ^ Handler
 -> EitherP e p a' a b' b m r        -- ^ Original computation
 -> EitherP f p a' a b' b m r        -- ^ Handled computation
handle f m = EitherP $ do
    e <- runEitherP m
    runEitherP $ case e of
        Left  e -> f     e
        Right r -> right r
