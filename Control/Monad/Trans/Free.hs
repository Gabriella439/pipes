{-|
    People commonly misconstrue 'Free' as defining a monad transformer with
    'liftF' behaving like 'lift', however that approach violates the monad
    transformer laws.  Another common mistake is to include the base monad as a
    term in the functor, which also gives rise to an incorrect monad
    transformer.

    To solve this, this module provides 'FreeT', which properly generalizes the
    free monad to a free monad transformer which is correct by construction.

    The 'FreeT' type commonly arises in coroutine and iteratee libraries that
    wish to provide a monad transformer that correctly obeys the monad
    transformer laws.
-}

module Control.Monad.Trans.Free (
    -- * Free monad transformer
    -- $freet
    FreeF(..),
    FreeT(..),
    wrap,
    liftF,
    -- * Free monad
    -- $free
    Free,
    runFree
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

{- $freet
    This differs substantially from the non-monad-transformer version because
    of the requirement to nest the constructors within the base monad.

    To deconstruct a free monad transformer, use 'runFreeT' to unwrap it and
    bind the result in the base monad.  You can then pattern match against the
    bound value to obtain the next constructor:

> do x <- runFreeT f
>    case x of
>        Return r -> ...
>        Wrap   w -> ...

    Because of this, you cannot create free monad transformers using the raw
    constructors from 'FreeF'.  Instead you use the smart constructors 'return'
    (from @Control.Monad@) and 'wrap'.
-}

-- | The signature for 'Free'
data FreeF f r x = Return r | Wrap (f x)

{-|
    A free monad transformer alternates nesting the base monad @m@ and the base
    functor @f@.

    * @f@ - The functor that generates the free monad transformer

    * @m@ - The base monad

    * @r@ - The type of the return value
-}
data FreeT f m r = FreeT { runFreeT :: m (FreeF f r (FreeT f m r)) }

instance (Functor f, Monad m) => Functor (FreeT f m) where
    fmap = liftM

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure  = return
    (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return  = FreeT . return . Return
    m >>= f = FreeT $ do
        x <- runFreeT m
        runFreeT $ case x of
            Return r -> f r
            Wrap   w -> wrap $ fmap (>>= f) w

instance (Functor f, MonadIO m) => MonadIO (FreeT f m) where
    liftIO = lift . liftIO

instance MonadTrans (FreeT f) where
    lift = FreeT . liftM Return

-- | Smart constructor for 'Wrap'
wrap :: (Monad m) => f (FreeT f m r) -> FreeT f m r
wrap = FreeT . return . Wrap

-- | Equivalent to @liftF@ from "Control.Monad.Free"
liftF :: (Functor f, Monad m) => f r -> FreeT f m r
liftF x = wrap $ fmap return x

{- $free
    The 'Free' type is isomorphic to the following simple implementation:

> data Free f r = Return r | Wrap (f (Free f r))

    ... except that if you want to pattern match against those constructors, you
    must first use 'runFree' to unwrap the value first.

> case (runFreeT f) of
>     Return r -> ...
>     Wrap   w -> ...

    Similarly, you use the smart constructors 'return' and 'wrap' to build a
    value of type 'Free'.
-}

-- | 'FreeT' reduces to 'Free' when specialized to the 'Identity' monad.
type Free f = FreeT f Identity

-- | Observation function that exposes the next 'FreeF' constructor
runFree :: Free f r -> FreeF f r (Free f r)
runFree = runIdentity . runFreeT
