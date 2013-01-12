-- | This module temporarily holds this class until it can find a better home.

{-# LANGUAGE Rank2Types, Safe #-}

module Control.MFunctor (
    -- * Functors over Monads
    MFunctor(..),
    hoistK,
    raise,
    raiseK
    ) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT)
import Control.Monad.Trans.RWS (RWST, mapRWST)
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.State.Lazy   as StateLazy 
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import qualified Control.Monad.Trans.Writer.Lazy   as WriterLazy

{- | A functor in the category of monads

> hoist f . hoist g = hoist (f . g)
>
> hoist id = id

    If @f@ is a monad morphism, then @hoist f@ is a monad morphism, meaning that
    @hoistK f = (hoist f .)@ defines a functor between Kleisli categories:

> hoistK f k1 >=> hoistK f k2 = hoistK f (k1 >=> k2)
>
> hoistK f return = return
-}
class MFunctor t where
    {-| Lift a monad morphism from @m@ to @n@ into a monad morphism from
        @(t m)@ to @(t n)@ -}
    hoist
     :: (Monad m)
     => (forall a . m a -> n a)  -- ^ Monad morphism
     -> t m b -> t n b

instance MFunctor IdentityT where
    hoist nat = mapIdentityT nat

instance MFunctor MaybeT where
    hoist nat = mapMaybeT nat

instance MFunctor (ReaderT r) where
    hoist nat = mapReaderT nat

instance MFunctor (RWST r w s) where
    hoist nat = mapRWST nat

instance MFunctor (StateStrict.StateT s) where
    hoist nat = StateStrict.mapStateT nat

instance MFunctor (StateLazy.StateT s) where
    hoist nat = StateLazy.mapStateT nat

instance MFunctor (WriterStrict.WriterT w) where
    hoist nat = WriterStrict.mapWriterT nat

instance MFunctor (WriterLazy.WriterT w) where
    hoist nat = WriterLazy.mapWriterT nat

-- | Convenience function equivalent to @(hoist f .)@
hoistK
 :: (Monad m, MFunctor t)
 => (forall a . m a -> n a)  -- ^ Monad morphism
 -> (b' -> t m b)            -- ^ Kleisli arrow
 -> (b' -> t n b)
hoistK k p a' = hoist k (p a')
-- hoistK k = (hoist k .)

{-| Lift the base monad

> raise = hoist lift
-}
raise :: (Monad m, MFunctor t1, MonadTrans t2) => t1 m r -> t1 (t2 m) r
raise = hoist lift

{-| Lift the base monad of a \'@K@\'leisli arrow

> raiseK = hoistK lift
-}
raiseK
 :: (Monad m, MFunctor t1, MonadTrans t2) => (q -> t1 m r) -> (q -> t1 (t2 m) r)
raiseK = (hoist lift .)
