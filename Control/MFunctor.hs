{-| This module temporarily holds this class until it can find a better home.

    Note: When combining two monad transformers with two different base monads,
    use @(hoist lift)@ to lift one, not @(hoist runXXX)@ to lower one.  The
    former approach not only works better but also provides 'hoist' with a monad
    morphism, which is what it expects in theory.
-}

{-# LANGUAGE Rank2Types #-}

module Control.MFunctor (
    -- * Monads over functors
    MFunctor(..)
    ) where

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.State.Lazy   as StateLazy 
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import qualified Control.Monad.Trans.Writer.Lazy   as WriterLazy

-- | A functor in the category of monads
class MFunctor t where
    {-| Lift a monad morphism from @m@ to @n@ into a monad morphism from
        @(t m)@ to @(t n)@ -}
    hoist :: (Monad m) => (forall a . m a -> n a) -> t m b -> t n b

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
