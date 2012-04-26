{-| Every functor @f@ gives rise to a corresponding free monad: @Free f@.

    A free monad over a functor resembles a \"list\" of that functor:

    * 'return' behaves like @[]@ by not applying the functor at all

    * 'free' behaves like @(:)@ by prepending another layer of the functor
-}
module Control.Monad.Trans.Free (
    -- * The Free monad
    Free(..),
    free,
    runFree,
    -- * The FreeT monad transformer
    FreeF(..),
    FreeT(..),
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity

-- | The underlying functor for the 'Free' monad:
data FreeF f r x = Return r | Free (f x)

{-|
    A free monad transformer layers a free monad on top of an existing monad

    * @f@ - The functor that generates the free monad

    * @m@ - The base monad

    * @r@ - The type of the return value
-}
data FreeT f m r = FreeT { runFreeT :: m (FreeF f r (FreeT f m r)) }

type Free f = FreeT f Identity

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return = FreeT . return . Return
    m >>= f = FreeT $ do
        x <- runFreeT m
        runFreeT $ case x of
            Return r -> f r
            Free a   -> free $ fmap (>>= f) a

instance (Functor f, Monad m) => Functor (FreeT f m) where fmap = liftM

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure = return
    (<*>) = ap

instance MonadTrans (FreeT f) where lift = FreeT . liftM Return

free :: (Monad m) => f (FreeT f m r) -> FreeT f m r
free = FreeT . return . Free

runFree :: Free f r -> FreeF f r (Free f r)
runFree = runIdentity . runFreeT
