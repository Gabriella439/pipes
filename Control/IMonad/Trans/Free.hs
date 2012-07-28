-- | This module is the indexed version of "Control.Monad.Trans.Free"

{-# LANGUAGE KindSignatures, TypeOperators #-}

module Control.IMonad.Trans.Free (
    -- * Free monad transformers
    -- $freet
    IFreeF(..),
    IFreeT(..),
    wrap,
    liftF
    ) where

import Control.Category.Index
import Control.IMonad
import Control.IMonad.Trans

{- $freet
    Indexed free monad transformers lift the constructor signatures to
    the category of indexed Haskell functions: (':->').

> Return ::   r :-> IFreeF f r x
> Wrap   :: f x :-> IFreeF f r x
>
> IFreeT :: m (IFreeF f r (IFreeT f m r)) :-> IFreeT f m r
-}

-- | Indexed equivalent to @FreeF@
data IFreeF f r (x :: * -> *) i = Return (r i) | Wrap (f x i)

-- | Indexed equivalent to @FreeT@
newtype IFreeT f m r i = IFreeT { runIFreeT :: m (IFreeF f r (IFreeT f m r)) i }

instance (IFunctor f, IMonad m) => IFunctor (IFreeT f m) where
    fmapI f x = x ?>= returnI . f

instance (IFunctor f, IMonad m) => IMonad (IFreeT f m) where
    returnI = IFreeT . returnI . Return
    bindI f m = IFreeT $
        runIFreeT m ?>= \x ->
        runIFreeT $ case x of
            Return r -> f r
            Wrap   w -> wrap $ fmapI (bindI f) w

instance (IFunctor f) => IMonadTrans (IFreeT f) where
    liftI = IFreeT . fmapI Return

-- | Indexed equivalent to @wrap@
wrap :: (IMonad m) => f (IFreeT f m r) :-> IFreeT f m r
wrap = IFreeT . returnI . Wrap

-- | Indexed equivalent to @liftF@
liftF :: (IFunctor f, IMonad m) => f r :-> IFreeT f m r
liftF x = wrap $ fmapI returnI x

-- FIXME: Add IIdentity so that IFree can be defined in terms of IFreeT
