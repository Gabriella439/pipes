-- | This module provides the proxy transformer equivalent of 'EitherT'.

{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Control.Proxy.Trans.Either (
    -- * EitherP
    EitherP(..),
    left,
    right,
    -- * Symmetric monad
    -- $symmetry
    throw,
    catch,
    handle
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.MFunctor (MFunctor(mapT))
import Control.Proxy.Class (
    Channel(idT    , (>->)), 
    Request(request, (\>\)), 
    Respond(respond, (/>/)))
import Control.Proxy.Trans (ProxyTrans(liftP))
import Prelude hiding (catch)

-- | The 'Either' proxy transformer, which allows
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

instance (MonadTrans (p a' a b' b)) => MonadTrans (EitherP e p a' a b' b) where
    lift = EitherP . lift . liftM Right

instance (MFunctor (p a' a b' b)) => MFunctor (EitherP e p a' a b' b) where
    mapT nat = EitherP . mapT nat . runEitherP

instance (Channel p) => Channel (EitherP e p) where
    idT = EitherP . idT
    p1 >-> p2 = (EitherP .) $ runEitherP . p1 >-> runEitherP . p2

instance ProxyTrans (EitherP e) where
    liftP = EitherP . liftM Right

-- | Abort the monad and return a 'Left' result
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
 => EitherP e p a' a b' b m r
 -> (e -> EitherP f p a' a b' b m r)
 -> EitherP f p a' a b' b m r
catch m f = EitherP $ do
    e <- runEitherP m
    runEitherP $ case e of
        Left  e -> f     e
        Right r -> right r

-- | 'catch' with the arguments flipped
handle
 :: (Monad (p a' a b' b m))
 => (e -> EitherP f p a' a b' b m r)
 -> EitherP e p a' a b' b m r
 -> EitherP f p a' a b' b m r
handle f m = EitherP $ do
    e <- runEitherP m
    runEitherP $ case e of
        Left  e -> f     e
        Right r -> right r
