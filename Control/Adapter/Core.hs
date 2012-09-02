module Control.Adapter.Core where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Void

-- | The base functor for the 'Adapter' type
data AdapterF a' a b' b x = Give b (b' -> x) | Need a' (a -> x)

instance Functor (AdapterF a' a b' b) where
    fmap f (Give b  fb') = Give b  (f . fb')
    fmap f (Need a' fa ) = Need a' (f . fa )

type Adapter a' a b' b = FreeT (AdapterF a' a b' b)

type Closure arg ret = Adapter Void () arg ret

type Opening arg ret = Adapter arg ret () Void

need :: (Monad m) => a' -> Adapter a' a b' b m a
need a' = liftF $ Need a' id

give :: (Monad m) => b  -> Adapter a' a b' b m b'
give b  = liftF $ Give b  id

(<-<) :: (Monad m)
 => (c' -> Adapter b' b c' c m r)
 -> (b' -> Adapter a' a b' b m r)
 -> (c' -> Adapter a' a c' c m r)
p1 <-< p2 = \c' -> FreeT $ do
    x1 <- runFreeT $ p1 c'
    runFreeT $ case x1 of
        Pure           r    -> return r
        Free (Give c  fc') -> wrap $ Give c (fc' <-< p2)
        Free (Need b' fb ) -> FreeT $ do
            x2 <- runFreeT $ p2 b'
            runFreeT $ case x2 of
                Pure           r    -> return r
                Free (Give b  fb') -> ((\_ -> fb b) <-< fb') c'
                Free (Need a' fa ) -> do
                    let p1' = \_ -> FreeT $ return x1
                    wrap $ Need a' $ \a -> (p1' <-< (\_ -> fa a)) c'

{- foreverK uses 'let' to avoid a space leak.
   See: http://hackage.haskell.org/trac/ghc/ticket/5205 -}
foreverK :: (Monad m) => (a -> m a) -> (a -> m b)
foreverK k = let r = k >=> r in r

idA :: (Monad m) => a' -> Adapter a' a a' a m r
idA = \a' -> wrap $ Need a' $ \a -> wrap $ Give a idA
-- i.e. idA = foreverK $ need >=> give

runAdapter :: (Monad m) => (() -> Adapter Void () () Void m r) -> m r
runAdapter p = runAdapter' $ p ()

runAdapter' p = do
    x <- runFreeT p
    case x of
        Pure          r    -> return r
        Free (Give _ fb ) -> runAdapter' $ fb  ()
        Free (Need _ fa') -> runAdapter' $ fa' ()
