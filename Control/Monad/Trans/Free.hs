{-| Every functor @f@ gives rise to a corresponding free monad: @Free f@.

    A free monad over a functor resembles a \"list\" of that functor:

    * 'pure' behaves like @[]@ by not using the functor at all

    * 'wrap' behaves like @(:)@ by prepending another layer of the functor
-}
module Control.Monad.Trans.Free (
    -- * The Free monad
    FreeF(..),
    Free(..),
    wrap,
    runFree,
    -- * The FreeT monad transformer
    FreeT(..),
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity

data FreeF f r x = Pure r | Wrap (f x)

{-|
    The 'Free' type is isomorphic to:

> data Free f r = Pure r | Wrap (f (Free f r))

    ... except that if you want to pattern match against those constructors, you
    must first use 'runFree' to unwrap the value first.

    Similarly, you don't use the raw constructors to build a value of type
    'Free'.  You instead use the smart constructors 'pure' (from
    @Control.Applicative@) and 'wrap'.
-}
type Free f = FreeT f Identity

wrap :: (Monad m) => f (FreeT f m r) -> FreeT f m r
wrap = FreeT . return . Wrap

runFree :: Free f r -> FreeF f r (Free f r)
runFree = runIdentity . runFreeT

{-|
    A free monad transformer alternates nesting the base functor @f@ and the
    base monad @m@.

    * @f@ - The functor that generates the free monad

    * @m@ - The base monad

    * @r@ - The type of the return value

    This type commonly arises in coroutine/iteratee libraries under various
    names.
-}
data FreeT f m r = FreeT { runFreeT :: m (FreeF f r (FreeT f m r)) }

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return = FreeT . return . Pure
    m >>= f = FreeT $ do
        x <- runFreeT m
        runFreeT $ case x of
            Pure r -> f r
            Wrap a -> wrap $ fmap (>>= f) a

instance (Functor f, Monad m) => Functor (FreeT f m) where
    fmap = liftM

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure = return
    (<*>) = ap

instance MonadTrans (FreeT f) where
    lift = FreeT . liftM Pure
