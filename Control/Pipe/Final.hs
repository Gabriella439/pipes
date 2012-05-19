module Control.Pipe.Final where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Pipe.Common
import Data.Void
import Prelude hiding ((.), id)

-- TODO: Newtype Frame
-- Define functor instance for Frame
-- Define transformation from Producer to Frame
-- Make types of comult functions more general

{-|
    A pipe type that enables 'Prompt' finalization 

    This type of pipe returns a 'Producer' when it no longer needs input.  The
    finalization machinery uses this information to safely finalize upstream the
    moment you downgrade to a 'Producer', so the earlier you downgrade, the more
    promptly upstream gets finalized.

    The finalization machinery also finalizes downstream pipes when the
    'Producer' terminates.  This permits a strict ordering of finalizers from
    upstream to downstream.

    Note that this type does not form a 'Monad'.
-}
newtype Prompt a b m r = Prompt { unPrompt :: Pipe a b m (Producer b m r) }

instance (Monad m) => Functor (Prompt a b m) where
    fmap f (Prompt p) = Prompt $ fmap (fmap f) p

{-|
    A pipe type that 'Ensure's deterministic finalization

    The finalization machinery uses the input and output ends in different ways
    to finalize the pipe when another pipe terminates first.

    If an upstream pipe terminates first, the current pipe will receive a
    'Nothing' once.  This allows it to finalize itself and if it terminates then
    its return value takes precedence over upstream's return value.  However, if
    it 'await's again, it defers to upstream's return value and never regains
    control.

    On the output end, the pipe must supply its most up-to-date finalizer
    alongside every value it 'yield's downstream.  This finalizer is guaranteed
    to be called if downstream terminates first.

    This type is called a 'Ensure' because it is like a 'Frame', except without
    the downgrade information and therefore it preserves the \'M\'onad instance.
-}
type Ensure a b m r = Pipe (Maybe a) (m (), b) m r

{-|
    A pipe type that combines 'Prompt' and 'Ensure' to achieve both prompt and
    guaranteed finalization.

    The name connotes a stack frame, since finalized pipes can be thought of as
    forming the 'Category' of stack frames, where upstream finalization is
    equivalent to finalizing the heap, and downstream finalization is equivalent
    to throwing an exception up the stack.
-}
type Frame a b m r = Prompt (Maybe a) (m (), b) m r

-- Convenience type just to annotate internal functions without using newtypes
type Prompt' a b m r = Pipe a b m (Producer b m r)

(<~<) :: (Monad m) => Prompt' b c m r -> Prompt' a b m r -> Prompt' a c m r
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

(<~|) :: (Monad m) => Prompt' b c m r -> Producer b m r -> Producer c m r
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
 -> Prompt' (Maybe        b ) (m (), c) m r
 -> Prompt' (Maybe (m (), b)) (m (), c) m r
mult m p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ lift m >> p'
        Wrap (Yield ((m', c), p')) -> wrap $ Yield ((m >> m', c), mult m p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Nothing      -> mult unit (f   Nothing)
            Just (m', b) -> mult m'   (f $ Just b )

comult :: (Monad m)
 => Prompt' (Maybe a)        b  m r
 -> Prompt' (Maybe a) (Maybe b) m r
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
 => Prompt' (Maybe a) (Maybe b) m r
 -> Prompt' (Maybe a) (Maybe b) m r
schedule p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure p'
        Wrap (Await f) -> wrap $ Yield (Nothing, wrap $ Await f)
        Wrap (Yield y) -> wrap $ Yield $ fmap schedule y

idF :: (Monad m) => Frame a a m r
idF = Prompt $ forever $ awaitF >>= yieldF

(<-<) :: (Monad m) => Frame b c m r -> Frame a b m r -> Frame a c m r
(Prompt p1) <-< (Prompt p2) = Prompt $ mult unit p1 <~< comult p2

newtype FrameC m r a b = FrameC { unFrameC :: Frame a b m r }

instance (Monad m) => Category (FrameC m r) where
    (FrameC p1) . (FrameC p2) = FrameC $ p1 <-< p2
    id = FrameC idF

yieldF :: (Monad m) => b -> Pipe a (m (), b) m ()
yieldF x = yield (unit, x)

awaitF :: (Monad m) => Pipe (Maybe a) b m a
awaitF = await >>= maybe awaitF return

close :: (Monad m) => Producer (m (), b) m r -> Frame a b m r
close = Prompt . pure

reopen :: (Monad m) => Frame a b m r -> Ensure a b m r
reopen (Prompt p) = join $ fmap (<+< (forever $ yield ())) p

catchP :: (Monad m) => m () -> Ensure a b m r -> Ensure a b m r
catchP m p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure r -> pure r
        Wrap (Yield ((m', b), p')) -> wrap $ Yield ((m' >> m, b), catchP m p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Nothing -> lift m >> catchP m (f e)
            Just _  ->           catchP m (f e)
{- Equivalent to:

awaitF' m = await >>= maybe (lift m >> awaitF' m) return

yieldF' m x = yield (m, x)

catchP m p =  reopen $
     (forever $ awaitF >>= yieldF' m)
 <-< Prompt (fmap downgrade p)
 <-< (forever $ awaitF' m >>= yieldF) -}

finallyP :: (Monad m) => m () -> Ensure a b m r -> Ensure a b m r
finallyP m p = do
    r <- catchP m p
    lift m
    return r

runFrame :: (Monad m) => Frame () Void m r -> m r
runFrame p = go (reopen p) where
    go p = do
        x <- runFreeT p
        case x of
            Pure r -> return r
            Wrap (Await f) -> go $ f (Just ())
            Wrap (Yield y) -> go $ snd y
