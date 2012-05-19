module Control.Pipe.Final (
    -- * Types
    Prompt(..),
    Ensure,
    Frame,
    Stack,
    -- * Create Frames
    -- $create
    yieldF,
    awaitF,
    -- * Prompt Finalization
    -- $prompt
    close,
    bindClosed,
    reopen,
    -- * Ensure Finalization
    -- $ensure
    catchP,
    finallyP,
    -- * Compose Frames
    idF,
    (<-<),
    (>->),
    FrameC(..),
    -- * Run Frames
    runFrame
    ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Pipe.Common
import Data.Void
import Prelude hiding ((.), id)

{-|
    A pipe type that enables 'Prompt' finalization 

    This type of pipe returns a 'Producer' when it no longer needs input.  The
    finalization machinery uses this information to safely finalize upstream the
    moment you downgrade to a 'Producer', so the earlier you downgrade, the more
    promptly upstream gets finalized.

    The finalization machinery also finalizes downstream pipes when the
    'Producer' terminates.  This permits a strict ordering of finalizers from
    upstream to downstream.

    Note that this type does not form a 'Monad', however with extensions it
    can be rewritten as a parametrized monad.
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

-- | A 'Stack' is a 'Frame' that doesn't need input and doesn't generate output
type Stack m r = Frame () Void m r

{- $create
    The first step to convert 'Pipe' code to 'Frame' code is to replace all
    'yield's with 'yieldF's and all 'await's with 'awaitF's.

> contrived = do   -->  contrived = do
>     x1 <- await  -->      x1 <- awaitF
>     yield x1     -->      yieldF x1
>     x2 <- await  -->      x2 <- awaitF
>     yield x2     -->      yieldF x2
-}

-- | Like 'yield', but also yields an empty finalizer alongside the value
yieldF :: (Monad m) => b -> Pipe a (m (), b) m ()
yieldF x = yield (unit, x)

-- | Like 'await', but ignores 'Nothing' and just awaits again
awaitF :: (Monad m) => Pipe (Maybe a) b m a
awaitF = await >>= maybe awaitF return

{- $prompt
    The second step to convert 'Pipe' code to 'Frame' code is to mark the point
    where your 'Pipe' no longer 'await's by wrapping it in the 'close' function
    and then wrapping the 'Pipe' in a 'Prompt' constructor:

> contrived :: (Monad m) => Frame a a m ()
> contrived = Prompt $ do
>     x1 <- awaitF
>     yieldF x1
>     x2 <- awaitF
>     close $ yieldF x2
-}

{-|
    Use this to mark when a 'Frame' no longer requires input.  The earlier the
    better!
-}
close :: (Monad m) => Producer b m r -> Pipe a b m (Producer b m r)
close = pure

{-|
    Used to bind to the 'Producer' if you want to continue where it left off but
    you still don't require input.

    This function would not be necessary if 'Prompt' were implemented as a
    parametrized monad, so if it seems ugly, that's because it is.
-}
bindClosed :: (Monad m) =>
    Prompt a b m r1 -> (r1 -> Producer b m r2) -> Prompt a b m r2
bindClosed (Prompt p) f = Prompt $ fmap (>>= f) p

{-|
    Use this to 'reopen' a 'Frame' if you change your mind and decide you want
    to continue to 'await' input after all.

    This postpones finalization of upstream until you 'close' the input end
    again.
-}
reopen :: (Monad m) => Frame a b m r -> Ensure a b m r
reopen (Prompt p) = join $ fmap (<+< (forever $ yield ())) p

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

{- Internal type used to annotate internal functions without descending into
   newtype hell. -}
type Prompt' a b m r = Pipe a b m (Producer b m r)

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

(>->) :: (Monad m) => Frame a b m r -> Frame b c m r -> Frame a c m r
(>->) = flip (<-<)

newtype FrameC m r a b = FrameC { unFrameC :: Frame a b m r }

instance (Monad m) => Category (FrameC m r) where
    (FrameC p1) . (FrameC p2) = FrameC $ p1 <-< p2
    id = FrameC idF

{- $ensure
    The third (optional) step to convert 'Pipe' code to 'Frame' code is to use
    'catchP' or 'finallyP' to register finalizers for blocks of code.

> contrived :: Frame a a IO ()
> contrived = Prompt $ do
>     catchP (putStrLn "Stage 1 interrupted") $ do
>         x1 <- awaitF
>         catchP (putStrLn "Stage 1(b) interrupted") $ yieldF x1
>     catchP (putStrLn "Stage 2 interrupted") $ do
>         x2 <- awaitF
>         close $ yieldF x2
-}

{-|
    @catchP m p@ registers @m@ to be called only if another composed
    pipe terminates before @p@ is done.
-}
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
 <-< Prompt (fmap close p)
 <-< (forever $ awaitF' m >>= yieldF) -}

{-|
    'finallyP' is like 'catchP' except that it also calls the finalizer if @p@
    completes normally.
-}
finallyP :: (Monad m) => m () -> Ensure a b m r -> Ensure a b m r
finallyP m p = do
    r <- catchP m p
    lift m
    return r

runFrame :: (Monad m) => Stack m r -> m r
runFrame p = go (reopen p) where
    go p = do
        x <- runFreeT p
        case x of
            Pure r -> return r
            Wrap (Await f) -> go $ f (Just ())
            Wrap (Yield y) -> go $ snd y
