module Control.Pipe.Final where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Pipe.Common
import Data.Void

type PipeD a b m r = Pipe a b m (Producer b m r)

(<~<) :: (Monad m) => PipeD b c m r -> PipeD a b m r -> PipeD a c m r
p1 <~< p2 = FreeT $ do
    x1 <- runFreeT p1
    runFreeT $ case x1 of
        Pure p1' -> pure p1'
        Wrap (Yield y) -> wrap $ Yield $ fmap (<~< p2) y
        Wrap (Await f) -> FreeT $ do
            let p1 = FreeT $ return x1
            x2 <- runFreeT p2
            runFreeT $ case x2 of
                Pure p2' -> pure $ p1 <~| p2'
                Wrap (Yield (b, p2')) -> f b <~< p2'
                Wrap (Await a) -> wrap $ Await $ fmap (p1 <~<) a

(<~|) :: (Monad m) => PipeD b c m r -> Producer b m r -> Producer c m r
p1 <~| p2 = FreeT $ do
    x1 <- runFreeT p1
    runFreeT $ case x1 of
        Pure p' -> p'
        Wrap (Yield y) -> wrap $ Yield $ fmap (<~| p2) y
        Wrap (Await f) -> FreeT $ do
            let p1 = FreeT $ return x1
            x2 <- runFreeT p2
            runFreeT $ case x2 of
                Pure r -> pure r
                Wrap (Yield (b, p2')) -> f b <~| p2'
                Wrap (Await a) -> wrap $ Await $ fmap (p1 <~|) a

type Frame a b m r = PipeD (Either r a) (m (), b) m r

unit :: (Monad m) => m ()
unit = return ()

mult :: (Monad m)
 => m ()
 -> PipeD (Either r        b ) (m (), c) m r
 -> PipeD (Either r (m (), b)) (m (), c) m r
mult m p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ lift m >> p'
        Wrap (Yield ((m', c), p')) -> wrap $ Yield ((m >> m', c), mult m p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Left r -> mult unit (f $ Left r)
            Right (m', b) -> mult m' (f $ Right b)

comult :: (Monad m) =>
    PipeD (Either r a)           (m (), b)  m r
 -> PipeD (Either r a) (Either r (m (), b)) m r
comult p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ warn p'
        Wrap (Yield (b', p')) -> wrap $ Yield (Right b', comult p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Left  r -> schedule r $ comult (f e)
            Right _ ->              comult (f e)

warn :: (Monad m) =>
    Producer           (m (), b)  m r
 -> Producer (Either r (m (), b)) m r
warn p = do
    r <- pipe Right <+< p
    yield (Left r)
    return r

schedule :: (Monad m) =>
    r
 -> PipeD (Either r a) (Either r (m (), b)) m r
 -> PipeD (Either r a) (Either r (m (), b)) m r
schedule r p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure p'
        Wrap (Await f) -> wrap $ Yield (Left r, wrap $ Await f)
        Wrap (Yield y) -> wrap $ Yield $ fmap (schedule r) y

yieldH :: (Monad m) => b -> Pipe (Either r a) (m (), b) m ()
yieldH x = yield (unit, x)

awaitS :: (Monad m) => Pipe (Either r a) (m (), b) m a
awaitS = await >>= either (\_ -> awaitS) return

idF :: (Monad m) => Frame a a m r
idF = forever $ awaitS >>= yieldH

(<-<) :: (Monad m) => Frame b c m r -> Frame a b m r -> Frame a c m r
p1 <-< p2 = mult unit p1 <~< comult p2

runFrame :: (Monad m) => Frame () Void m r -> m r
runFrame = runPipe . join
