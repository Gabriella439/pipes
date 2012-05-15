module Control.Pipe.Final where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Pipe.Common
import Data.Void

-- Pipe with a 'D'owngraded stage
type PipeD a b m r = Pipe a b m (Producer b m r)

-- 'S'afe pipe that finalizes resources promptly and deterministically
type PipeS a b m r = Pipe (Maybe a) (m (), b) m r

-- Safe pipe with a downgrade stage
type Frame a b m r = PipeD (Maybe a) (m (), b) m r

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

unit :: (Monad m) => m ()
unit = return ()

mult :: (Monad m)
 => m ()
 -> PipeD (Maybe        b ) (m (), c) m r
 -> PipeD (Maybe (m (), b)) (m (), c) m r
mult m p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ lift m >> p'
        Wrap (Yield ((m', c), p')) -> wrap $ Yield ((m >> m', c), mult m p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Nothing      -> mult unit (f   Nothing)
            Just (m', b) -> mult m'   (f $ Just b )

comult :: (Monad m)
 => PipeD (Maybe a)        (m (), b)  m r
 -> PipeD (Maybe a) (Maybe (m (), b)) m r
comult p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ warn p'
        Wrap (Yield (b', p')) -> wrap $ Yield (Just b', comult p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Nothing -> schedule $ comult (f e)
            Just _  ->            comult (f e)

warn :: (Monad m) =>
    Producer        (m (), b)  m r
 -> Producer (Maybe (m (), b)) m r
warn p = do
    r <- pipe Just <+< p
    yield Nothing
    return r

schedule :: (Monad m)
 => PipeD (Maybe a) (Maybe (m (), b)) m r
 -> PipeD (Maybe a) (Maybe (m (), b)) m r
schedule p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure p'
        Wrap (Await f) -> wrap $ Yield (Nothing, wrap $ Await f)
        Wrap (Yield y) -> wrap $ Yield $ fmap schedule y

-- The API exposed to users

yieldH :: (Monad m) => b -> Pipe a (m (), b) m ()
yieldH x = yield (unit, x)

awaitS :: (Monad m) => Pipe (Maybe a) b m a
awaitS = await >>= maybe awaitS return

idF :: (Monad m) => Frame a a m r
idF = forever $ awaitS >>= yieldH

(<-<) :: (Monad m) => Frame b c m r -> Frame a b m r -> Frame a c m r
p1 <-< p2 = mult unit p1 <~< comult p2

produce :: (Monad m) => Producer (m (), b) m r -> Frame a b m r
produce = pure

upgrade :: (Monad m) => Frame a b m r -> PipeS a b m r
upgrade p = join $ fmap (<+< (forever $ yield ())) p

-- catchH unit = id
catchH :: (Monad m) => m () -> Frame a b m r -> Frame a b m r
catchH m p = (forever $ awaitS >>= \x -> yield (m, x)) <-< p

-- catchS :: (Monad m) => 
-- catchS f p = p <-< (forever $ await >>= either f yieldH)

runFrame :: (Monad m) => Frame () Void m r -> m r
runFrame p = go (upgrade p) where
    go p = do
        x <- runFreeT p
        case x of
            Pure r -> return r
            Wrap (Await f) -> go $ f (Just ())
            Wrap (Yield y) -> go $ snd y
