module Control.Pipe.Final (
    -- * Introduction
    -- $intro
    -- * Types
    Prompt,
    Ensure,
    Frame(..),
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
    -- $compose
    (<-<),
    (>->),
    idF,
    FrameC(..),
    -- * Run Frames
    -- $run
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

{- $intro
    A 'Frame' is a higher-order type built on top of 'Pipe'.  It enables a
    richer composition with the ability to finalize resources:

    * Promptly: You can close resources when you no longer need input from them

    * Deterministically: It ensures that every 'Frame' is finalized no matter
      which frame terminates

    'Frame's differ from 'Pipe's in that they do not form monads, but instead
    form parametrized monads.  Unfortunately, parametrized monads are not
    mainstream in Haskell and require a ton of extensions along with a modified
    Prelude in order to recover @do@ notation, so this first release of the
    'Frame' implementation essentially \"in-lines\" the parametrized monad by
    splitting it into two monads.  Future releases will split off a version that
    takes advantage of parametrized monads for a much simpler underlying type
    and a significantly cleaner implementation.

    The section on \"Types\" is an in-depth explanation of the underlying type,
    which is unfortunately complicated because of in-lining the parametrized
    monad.  I tried to strike a balance between using newtypes to improve type
    errors and abstract over the internals and using type synonyms to avoid
    newtype hell.

    Ordinary users should start at the section \"Create Frames\", but if you
    encounter weird type errors and want to understand them, then consult the
    \"Types\" section.
-}

{-|
    An illustrative type synonym that demonstrates how 'Prompt' finalization
    works

    This type simulates a parametrized monad by breaking it up into two monads
    where the first monad returns the second one.  The first monad permits any
    pipe code and the second monad only permits pipe code that doesn't need
    input.

    This allows the finalization machinery to safely and promptly finalize
    upstream before beginning the second block, so the earlier the code
    transitions to the second monad (using the 'close' function), the more
    promptly upstream gets finalized.

    For example if @p = Pipe@, the first monad is an ordinary 'Pipe' and the
    second monad is a 'Producer':

> Prompt Pipe a b m r = Pipe a b m (Pipe () b m r)

    The finalization machinery also finalizes downstream pipes when the
    second monad terminates.  I use this trick to ensure a strict ordering of
    finalizers from upstream to downstream.

    I don't actually use the 'Prompt' type synonym, since that requires
    newtyping everything, but I will reference it in documentation to clarify
    type signatures.
-}
type Prompt p a b m r = p a b m (p () b m r)

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

    The combination of these two tricks allows a bidirectional guarantee of
    deterministic finalization that satisfies the 'Category' laws.
-}
type Ensure a b m r = Pipe (Maybe a) (m (), b) m r

{-|
    A pipe type that combines 'Prompt' and 'Ensure' to enable both prompt and
    deterministic finalization.

    The name connotes a stack frame, since finalized pipes can be thought of as
    forming the 'Category' of stack frames, where upstream finalization is
    equivalent to finalizing the heap, and downstream finalization is equivalent
    to throwing an exception up the stack.

    The type is equivalent to:

> type Frame a b m r = Prompt Ensure a b m r
-}
newtype Frame a b m r = Frame { unFrame ::
    Pipe (Maybe a) (m (), b) m (Pipe (Maybe ()) (m (), b) m r) }

instance (Monad m) => Functor (Frame a b m) where
    fmap f (Frame p) = Frame $ fmap (fmap f) p

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
yieldF :: (Monad m) => b -> Ensure a b m ()
yieldF x = yield (unit, x)

-- | Like 'await', but ignores all 'Nothing's and just awaits again
awaitF :: (Monad m) => Ensure a b m a
awaitF = await >>= maybe awaitF return

{- $prompt
    The second step to convert 'Pipe' code to 'Frame' code is to mark the point
    where your 'Pipe' no longer 'await's by wrapping it in the 'close' function
    and then wrapping the 'Pipe' in a 'Frame' newtype:

> contrived :: (Monad m) => Frame a a m ()
> contrived = Frame $ do
>     x1 <- awaitF
>     yieldF x1
>     x2 <- awaitF
>     close $ yieldF x2

    If a non-terminating pipe demands input indefinitely, there is no need to
    'close' it.  It will type-check if the return value is polymorphic as a 
    result of non-termination.
-}

{-|
    Use this to mark when a 'Frame' no longer requires input.  The earlier the
    better!
-}
close :: (Monad m) => Ensure () b m r -> Ensure a b m (Ensure () b m r)
close = pure

{-|
    Use this to bind to the 'close'd half of the frame if you want to continue
    where it left off but you still don't require input.

    This function would not be necessary if 'Prompt' were implemented as a
    parametrized monad, so if it seems ugly, that's because it is.
-}
bindClosed :: (Monad m) =>
    Frame a b m r1 -> (r1 -> Ensure () b m r2) -> Frame a b m r2
bindClosed (Frame p) f = Frame $ fmap (>>= f) p

{-|
    Use this to 'reopen' a 'Frame' if you change your mind and decide you want
    to continue to 'await' input after all.

    This postpones finalization of upstream until you 'close' the input end
    again.
-}
reopen :: (Monad m) => Frame a b m r -> Ensure a b m r
reopen (Frame p) = join $ fmap (<+< (forever $ yield $ Just ())) p

{- $ensure
    The third (optional) step to convert 'Pipe' code to 'Frame' code is to use
    'catchP' or 'finallyP' to register finalizers for blocks of code.

> contrived :: Frame a a IO ()
> contrived = Frame $ do
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
{- catchP is equivalent to:

awaitF' m = await >>= maybe (lift m >> awaitF' m) return

yieldF' m x = yield (m, x)

catchP m p =  reopen $
     (forever $ awaitF >>= yieldF' m)
 <-< Frame (fmap close p)
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

(<~<) :: (Monad m)
 => Pipe b c m (Pipe x c m r)
 -> Pipe a b m (Pipe x b m r)
 -> Pipe a c m (Pipe x c m r)
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

(<~|) :: (Monad m)
 => Pipe b c m (Pipe x c m r)
 -> Pipe x b m r
 -> Pipe x c m r
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
 -> Pipe (Maybe        b ) (m (), c) m (Pipe x (m (), c) m r)
 -> Pipe (Maybe (m (), b)) (m (), c) m (Pipe x (m (), c) m r)
mult m p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ lift m >> p'
        Wrap (Yield ((m', c), p')) -> wrap $ Yield ((m >> m', c), mult m p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Nothing      -> mult unit (f   Nothing)
            Just (m', b) -> mult m'   (f $ Just b )

comult :: (Monad m)
 => Pipe (Maybe a)        b  m (Pipe x        b  m r)
 -> Pipe (Maybe a) (Maybe b) m (Pipe x (Maybe b) m r)
comult p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure $ warn p'
        Wrap (Yield (b, p')) -> wrap $ Yield (Just b, comult p')
        Wrap (Await f) -> wrap $ Await $ \e -> case e of
            Nothing -> schedule $ comult (f e)
            Just _  ->            comult (f e)

warn :: (Monad m)
 => Pipe x        b  m r
 -> Pipe x (Maybe b) m r
warn p = do
    r <- pipe Just <+< p
    yield Nothing
    return r

schedule :: (Monad m)
 => Pipe (Maybe a) (Maybe b) m (Pipe x (Maybe b) m r)
 -> Pipe (Maybe a) (Maybe b) m (Pipe x (Maybe b) m r)
schedule p = FreeT $ do
    x <- runFreeT p
    runFreeT $ case x of
        Pure p' -> pure p'
        Wrap (Await f) -> wrap $ Yield (Nothing, wrap $ Await f)
        Wrap (Yield y) -> wrap $ Yield $ fmap schedule y

{- $compose
    The fourth step to convert 'Pipe' code to 'Frame' code is to use ('<-<') to
    compose 'Frame's instead of ('<+<').

> printer  :: Frame a Void IO r
> fromList :: (Monad m) => [a] -> Frame () a m ()
>
> p :: Frame () Void IO ()
> p = printer <-< contrived <-< fromList [1..]

    Similarly, 'idF' replaces 'idP'.

    When a 'Frame' terminates, the 'FrameC' category strictly orders the
    finalizers from upstream to downstream.  Specifically

    * When any 'Frame' 'close's its input end, it finalizes all frames upstream
      of it.  These finalizers are ordered from upstream to downstream.

    * A 'Frame' is responsible for finalizing its own resources under ordinary
      operation (either manually, or using 'finallyP').

    * When a 'Frame' terminates, everything downstream of it is finalized.
      These finalizers are ordered from upstream to downstream.

    The 'Category' instance for 'FrameC' provides the same strong guarantees as
    the 'Lazy' category.  This confers many practical advantages:

    * Registered finalizers are guaranteed to be called exactly once.
      Finalizers are never duplicated or dropped in corner cases.

    * The grouping of composition will never affect the ordering or behavior of
      finalizers.

    * Finalization does not grow more complex the more 'Frame's you add in your
      'Stack'.

    * You can reason about the finalization behavior of each 'Frame'
      independently of other 'Frame's it is composed with.
-}

-- | Corresponds to 'id' from @Control.Category@
idF :: (Monad m) => Frame a a m r
idF = Frame $ forever $ awaitF >>= yieldF

-- | Corresponds to ('<<<')/('.') from @Control.Category@
(<-<) :: (Monad m) => Frame b c m r -> Frame a b m r -> Frame a c m r
(Frame p1) <-< (Frame p2) = Frame $ mult unit p1 <~< comult p2

-- | Corresponds to ('>>>') from @Control.Category@
(>->) :: (Monad m) => Frame a b m r -> Frame b c m r -> Frame a c m r
(>->) = flip (<-<)

newtype FrameC m r a b = FrameC { unFrameC :: Frame a b m r }

instance (Monad m) => Category (FrameC m r) where
    (FrameC p1) . (FrameC p2) = FrameC $ p1 <-< p2
    id = FrameC idF

{- $run
    The final step to convert 'Pipe' code to 'Frame' code is to replace
    'runPipe' with 'runFrame'.

> printer  :: Frame a Void IO r
> take     :: (Monad m) => Int -> Frame a a m ()
> fromList :: (Monad m) => [a] -> Frame () a m ()

>>> runFrame $ printer <-< contrived <-< fromList [1..]
1
2

>>> runFrame $ printer <-< contrived <-< fromList [1]
1
Stage 2 interrupted

>>> runFrame $ printer <-< take 1 <-< contrived <-< fromList [1..]
Stage 1(b) interrupted
Stage 1 interrupted
1

For the last example, remember that 'take' is written to 'close' its input end
before yielding its final value, which is why the finalizers run before
@printer@ receives the 1.

-}

-- | Convert a 'Frame' back to the base monad.
runFrame :: (Monad m) => Stack m r -> m r
runFrame p = go (reopen p) where
    go p = do
        x <- runFreeT p
        case x of
            Pure r -> return r
            Wrap (Await f) -> go $ f (Just ())
            Wrap (Yield y) -> go $ snd y
