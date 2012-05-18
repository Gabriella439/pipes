module Control.Pipe.Final where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Pipe.Common
import Data.Void

-- TODO: Newtype Frame
-- Define functor instance for Frame
-- Define transformation from Producer to Frame
-- Make types of comult functions more general

-- Pipe with a 'D'owngraded stage
type PipeD a b m r = Pipe a b m (Producer b m r)

-- Frame that forms a 'M'onad but lacks a downgrade stage
type FrameM a b m r = Pipe (Maybe a) (m (), b) m r

-- Frame with a downgrade stage, but does not form a monad
type Frame a b m r = PipeD (Maybe a) (m (), b) m r

(<~<) :: (Monad m) => PipeD b c m r -> PipeD a b m r -> PipeD a c m r
p1 <~< p2 = FreeT $ do
    x1 <- runFreeT p1
    runFreeT $ case x1 of
        Pure p1'       -> pure p1'
        Wrap (Yield y) -> wrap $ Yield $ fmap (<~< p2) y
        Wrap (Await f1) -> FreeT $ do
            let p1 = FreeT $ return x1
            x2 <- runFreeT p2
            runFreeT $ case x2 of
                Pure p2'              -> pure $ p1 <~| p2'
                Wrap (Yield (b2, p2')) -> f1 b2 <~< p2'
                Wrap (Await f2      ) -> wrap $ Await $ fmap (p1 <~<) f2

(<~|) :: (Monad m) => PipeD b c m r -> Producer b m r -> Producer c m r
p1 <~| p2 = FreeT $ do
    x1 <- runFreeT p1
    runFreeT $ case x1 of
        Pure p1'        -> p1'
        Wrap (Yield y) -> wrap $ Yield $ fmap (<~| p2) y
        Wrap (Await f) -> FreeT $ do
            let p1 = FreeT $ return x1
            x2 <- runFreeT p2
            runFreeT $ case x2 of
                Pure r                -> pure r
                Wrap (Yield (b2, p2')) -> f b2 <~| p2'
                Wrap (Await f2      ) -> wrap $ Await $ fmap (p1 <~|) f2

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
 => PipeD (Maybe a)        b  m r
 -> PipeD (Maybe a) (Maybe b) m r
comult p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ warn p'
        Wrap (Yield (b, p')) -> wrap $ Yield (Just b, comult p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Nothing -> schedule $ comult (f e)
            Just _  ->            comult (f e)

warn :: (Monad m) =>
    Producer        b  m r
 -> Producer (Maybe b) m r
warn p = do
    r <- pipe Just <+< p
    yield Nothing
    return r

schedule :: (Monad m)
 => PipeD (Maybe a) (Maybe b) m r
 -> PipeD (Maybe a) (Maybe b) m r
schedule p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure p'
        Wrap (Await f) -> wrap $ Yield (Nothing, wrap $ Await f)
        Wrap (Yield y) -> wrap $ Yield $ fmap schedule y

awaitF' :: (Monad m) => m () -> Pipe (Maybe a) b m a
awaitF' m = await >>= maybe (lift m >> awaitF' m) return

yieldF' :: (Monad m) => m () -> b -> Pipe a (m (), b) m ()
yieldF' m x = yield (m, x)

-- The API intended for library users

idF :: (Monad m) => Frame a a m r
idF = forever $ awaitF >>= yieldF

(<-<) :: (Monad m) => Frame b c m r -> Frame a b m r -> Frame a c m r
p1 <-< p2 = mult unit p1 <~< comult p2

yieldF :: (Monad m) => b -> Pipe a (m (), b) m ()
yieldF x = yield (unit, x)

awaitF :: (Monad m) => Pipe (Maybe a) b m a
awaitF = await >>= maybe awaitF return

downgrade :: (Monad m) => Producer (m (), b) m r -> Frame a b m r
downgrade = pure

upgrade :: (Monad m) => Frame a b m r -> FrameM a b m r
upgrade p = join $ fmap (<+< (forever $ yield ())) p

catchP :: (Monad m) => m () -> Frame a b m r -> Frame a b m r
catchP m p =
    (forever $ awaitF >>= yieldF' m) <-< p <-< (forever $ awaitF' m >>= yieldF)

finallyP :: (Monad m) => m () -> Frame a b m r -> Frame a b m r
finallyP m p = do
    r <- catchP m p
    lift m
    return r

runFrame :: (Monad m) => Frame () Void m r -> m r
runFrame p = go (upgrade p) where
    go p = do
        x <- runFreeT p
        case x of
            Pure r -> return r
            Wrap (Await f) -> go $ f (Just ())
            Wrap (Yield y) -> go $ snd y
