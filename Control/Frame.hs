{-|
    'Frame's extend 'Pipe's with:

    * The ability to fold input

    * Prompt and deterministic finalization

    'Frame's differ from 'Pipe's because they form indexed monads rather than
    forming ordinary monads.  This means you must use rebind @do@ notation to
    use restricted monads from the @index-core@ package.  See the
    \"Create Frames\" section for details.  For even more details, consult the
    @index-core@ package.
-}

{-# LANGUAGE GADTs, TypeOperators #-}

module Control.Frame (
    -- * Types
    -- $types
    C,
    O,
    M,
    FrameF(..),
    Frame,
    Stack,
    -- * Create Frames
    -- $create

    -- ** Primitives
    -- $primitives
    yieldF,
    awaitF,
    close,
    -- ** Pipe-like primitives
    -- $pipeprims
    yield,
    await,
    -- * Finalize Frames
    -- $finalization
    catchD,
    catchF,
    finallyD,
    finallyF,
    -- * Compose Frames
    -- $compose
    (<-<),
    (>->),
    idF,
    -- * Run Frames
    -- $run
    FrameC(..),
    runFrame
    ) where

import Control.Category
import Control.IMonad
import Control.IMonad.Trans
import Control.IMonad.Trans.Free
import Control.Monad.Instances ()
import Data.Maybe
import Data.Void
import Prelude hiding ((.), id)

-- For documentation
import Control.Pipe.Common hiding (await, yield, Await, Yield)

{- $types
    The first step to convert 'Pipe' code to 'Frame' code is to translate the
    types.  All types of the form \"@Pipe a b m r@\" become
    \"@Frame b m (M a) C r@\".  For example, given the following type signatures
    from the tutorial:

> printer  :: (Show a) => Pipe b Void IO r
> take'    :: Int -> Pipe b b IO ()
> fromList :: (Monad m) => [b] -> Pipe () b m ()

    ... you would replace them with:

> printer  :: (Show a) => Frame Void IO (M a) C r
> take'    :: Int -> Frame a IO (M a) C ()
> fromList :: (Monad m) => [a] -> Frame a m (M ()) C ()
> -- To use the finalization example, change fromList's base monad to 'IO'
> fromList :: [a] -> Frame a IO (M ()) C ()
-}

-- | Index representing an open input end, receiving values of type @a@
data O a

-- | Index representing a closed input end
data C

-- | Index representing an open input end, receiving values of type @Maybe a@
type M a = O (Maybe a)

{-|
    Base functor for a pipe that can close its input end

    * @b@ - Output type

    * @x@ - Next step

    * @i@ - Current step's index
-}
data FrameF b x i where
    Yield ::  b -> x    i   -> FrameF b x    i
    Await :: (a -> x (O a)) -> FrameF b x (O a)
    Close ::       x    C   -> FrameF b x (O a)

instance IFunctor (FrameF b) where
    fmapI f p = case p of
        Yield b y -> Yield b (f y)
        Await a   -> Await (f . a)
        Close c   -> Close (f c)

{-|
    A 'Frame' is like a 'Pipe' with an indexed input end:

    * @b@ - The type of the 'Frame's output

    * @m@ - The base monad

    * @i@ - The initial index of the input end ('O'pen or 'C'losed)

    * @j@ - The final index of the input end ('O'pen or 'C'losed)

    * @r@ - The return value
-}
type Frame b m i j r = IFreeT (FrameF (m (), b)) (U m) (r := j) i

-- | A self-contained 'Frame' that is ready to be run
type Stack m r = Frame Void m (M ()) C r

-- $create
-- The second step to convert 'Pipe' code to 'Frame' code is to change your
-- module header to:
--
-- > {-# LANGUAGE RebindableSyntax #-}
-- >
-- > import Control.IMonad.Do
-- > import Control.Frame
-- > import Prelude hiding (Monad(..))
--
-- "Control.Frame" replaces all 'Pipe' 'await's and 'yield's with their
-- corresponding 'Frame' counterparts.  @Control.IMonad.Do@ rebinds @do@
-- notation to work with indexed monads, which also requires using the
-- @RebindableSyntax@ extension and hiding the 'Monad' class from the @Prelude@.
--
-- You also must use the restricted monad utility functions, which have the
-- same name as their ordinary monad counterparts except with an \'@R@\' suffix,
-- such as 'foreverR' instead of 'forever'.  Finally, you must use 'liftU'
-- instead of 'lift' to invoke operations in the base monad.
--
-- Finally, every terminating 'Frame' must be 'close'd exactly once before being
-- passed to composition.
--
-- > printer = foreverR $ do
-- >     a <- await
-- >     liftU $ print a
-- >
-- > take' n = do
-- >     replicateMR_ n $ do
-- >         a <- await
-- >         yield a
-- >     close
-- >     liftU $ putStrLn "You shall not pass!"
-- >
-- > fromList xs = do
-- >     close
-- >     mapMR_ yield xs

{- $primitives
    'yieldF' guards against downstream termination by yielding the most
    up-to-date finalization alongside each value, so that downstream can call
    that finalizer if it terminates before requesting another value.

    'awaitF' intercepts upstream termination by returning a 'Nothing' if
    upstream terminates before providing a value.  Further attempts to request
    input from upstream will terminate the current 'Frame' using the
    return value provided from upstream.

    While 'awaitF' is useful for folds, 'yieldF' is less useful for end-users of
    this library and the higher-order 'catchF' / 'finallyF' finalization
    functions are much more user-friendly.

    Composing two 'Frame's requires that each 'Frame' invokes 'close' exactly
    once.  Anything else will not type-check.  Leave out the 'close' statement
    when writing library components and let the person assembling the components
    for composition specify where the 'close' goes.

    The earlier you 'close' the upstream 'Frame', the earlier it is finalized.
    However, once you 'close' it you may no longer 'await'.
-}

-- | 'Yield' the most current finalizer for this 'Frame' alongside the value
yieldF :: (Monad m) => m () -> b -> Frame b m i i ()
yieldF m x = liftF $ Yield (m, x) (V ())

-- | 'Await' a value from upstream, returning 'Nothing' if upstream terminates
awaitF :: (Monad m) => Frame b m (M a) (M a) (Maybe a)
awaitF = liftF $ Await V

-- | 'Close' the input end, calling the finalizers of every upstream 'Frame'
close :: (Monad m) => Frame b m (M a) C ()
close = liftF $ Close (V ())

{- $pipeprims
    The following 'Pipe'-like primitives are built on top of the 'Frame'
    primitives.  They behave identically to their 'Pipe' counterparts and can
    be used as drop-in replacements for them.
-}

-- | 'yield' a value upstream alongside an empty finalizer
yield :: (Monad m) => b -> Frame b m i i ()
yield = yieldF (return ())

-- | 'await' a value from upstream and terminate if upstream terminates
await :: (Monad m) => Frame b m (M a) (M a) a
await = awaitF !>= maybe await returnR

{- $finalization
    The third (and optional) step to convert 'Pipe' code to 'Frame' code is to
    register finalizers for your 'Frame'.  These finalizers may be arbitrarily
    nested:

> printer = foreverR $ catchF (putStrLn "printer interrupted") $ do
>     a <- await
>     liftU $ print a
>
> take' n = finallyF (putStrLn "You shall not pass!") $ do
>     replicateMR_ n $ do
>         a <- catchF (putStrLn "take' interrupted") await
>         yield a
>     close
>
> fromList xs = catchF (putStrLn "fromList interrupted") $ do
>     close
>     mapMR_ yield xs

    These convenience functions register block-level finalizers to be called if
    another 'Frame' terminates first.  The naming conventions are:

    * \"catch\" functions (i.e. 'catchD' / 'catchF') call the finalizer only if
      another 'Frame' terminates before the block completes, but will not call
      the finalizer if the block terminates normally.

    * \"finally\" functions (i.e. 'finallyD' / 'finallyF') are like \"catch\"
      functions except that they also call the finalizer if the block terminates
      normally.

    * Functions that end in a \'@D@\' suffix (i.e. 'catchD' / 'finallyD') only
      guard against downstream termination.

    * Functions that end in a \'@F@\' suffix (i.e. 'catchF' / 'finallyF') guard
      against termination in both directions.  You usually want these ones.

    Note that finalization blocks that /begin/ after the 'close' statement may
    only use the \'@D@\'-suffixed version as upstream has been closed off.  This
    is a consequence of a deficiency in Haskell's type system that will take
    time to work around.  However an \'@F@\'-suffixed block that begins before a
    'close' statement may continue through it normally.  So, for code intended
    only for producers, use 'catchD' \/ 'finallyD', otherwise use 'catchF' \/
    'finallyF'.  In future releases, the \'@D@\'-suffixed versions will be
    removed and merged into the \'@F@\'-suffixed versions.
-}

{-|
    @catchD m p@ calls the finalizer @m@ if a downstream 'Frame' terminates
    before @p@ finishes.
-}
catchD :: (Monad m) => m () -> Frame b m i j r -> Frame b m i j r
catchD m p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Return r                -> returnI r
        Wrap (Close         p') -> wrap $ Close (catchD m p')
        Wrap (Yield (m', b) p') -> wrap $ Yield (m' >> m, b) (catchD m p')
        Wrap (Await         f ) -> wrap $ Await $ fmap (catchD m) f

{-|
    @catchF m p@ calls the finalizer @m@ if any 'Frame' terminates before @p@
    finishes.
-}
catchF :: (Monad m) => m () -> Frame b m (M a) j r -> Frame b m (M a) j r
catchF m p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Return r                -> returnI r
        Wrap (Close         p') -> wrap $ Close $ catchD m p'
        Wrap (Yield (m', b) p') -> wrap $ Yield (m' >> m, b) (catchF m p')
        Wrap (Await         f ) -> wrap $ Await $ \e -> case e of
            Nothing -> liftU m !> catchF m (f e)
            Just _  ->            catchF m (f e)

{-|
    @finallyD m p@ calls the finalizer @m@ if a downstream 'Frame' terminates
    before @p@ finishes or if @p@ completes normally.
-}
finallyD :: (Monad m) => m () -> Frame b m i j r -> Frame b m i j r
finallyD m p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Return r                -> liftU m !> returnI r
        Wrap (Close         p') -> wrap $ Close (finallyD m p')
        Wrap (Yield (m', b) p') -> wrap $ Yield (m' >> m, b) (finallyD m p')
        Wrap (Await         f ) -> wrap $ Await $ fmap (finallyD m) f

{-|
    @finallyF m p@ calls the finalizer @m@ if any 'Frame' terminates before @p@
    finishes or if @p@ completes normally.
-}
finallyF :: (Monad m) => m () -> Frame b m (M a) j r -> Frame b m (M a) j r
finallyF m p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Return r                -> liftU m !> returnI r
        Wrap (Close         p') -> wrap $ Close $ finallyD m p'
        Wrap (Yield (m', b) p') -> wrap $ Yield (m' >> m, b) (finallyF m p')
        Wrap (Await         f ) -> wrap $ Await $ \e -> case e of
            Nothing -> liftU m !> finallyF m (f e)
            Just _  ->            finallyF m (f e)

(<~<) :: (Monad m)
 => IFreeT (FrameF c) (U m) (r := C) (O b)
 -> IFreeT (FrameF b) (U m) (r := C) (O a)
 -> IFreeT (FrameF c) (U m) (r := C) (O a)
p1 <~< p2 = IFreeT $ U $ do
    x1 <- unU $ runIFreeT p1
    unU $ runIFreeT $ case x1 of
        Wrap (Close   p1') -> wrap $ Close p1'
        Wrap (Yield c p1') -> wrap $ Yield c (p1' <~< p2)
        Wrap (Await   f1 ) -> IFreeT $ U $ do
            x2 <- unU $ runIFreeT p2
            let p1' = IFreeT $ returnI x1
            unU $ runIFreeT $ case x2 of
                Wrap (Close p2')   -> wrap $ Close $ p1' <~| p2'
                Wrap (Yield b p2') -> f1 b <~< p2'
                Wrap (Await f2) -> wrap $ Await $ fmap (\p2'-> p1' <~< p2') f2

(<~|) :: (Monad m)
 => IFreeT (FrameF c) (U m) (r := C) (O b)
 -> IFreeT (FrameF b) (U m) (r := C)  C
 -> IFreeT (FrameF c) (U m) (r := C)  C
p1 <~| p2 = IFreeT $ U $ do
    x1 <- unU $ runIFreeT p1
    unU $ runIFreeT $ case x1 of
        Wrap (Close   p1') -> p1'
        Wrap (Yield c p1') -> wrap $ Yield c (p1' <~| p2)
        Wrap (Await   f1 ) -> IFreeT $ U $ do
            x2 <- unU $ runIFreeT p2
            let p1' = IFreeT $ returnI x1
            unU $ runIFreeT $ case x2 of
                Return r           -> returnI r
                Wrap (Yield b p2') -> f1 b <~| p2' 

heap :: (Monad m)
 => m ()
 -> IFreeT (FrameF (m (), c)) (U m) (r := C) (M        b )
 -> IFreeT (FrameF (m (), c)) (U m) (r := C) (M (m (), b))
heap m p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Wrap (Close         p') -> wrap $ Close $ liftU m !> p'
        Wrap (Yield (m', c) p') -> wrap $ Yield (m >> m', c) (heap m p')
        Wrap (Await         f ) -> wrap $ Await $ \e -> case e of
            Nothing      -> heap (return ()) (f  Nothing)
            Just (m', b) -> heap m'          (f $ Just b)

stack :: (Monad m)
 => Bool
 -> IFreeT (FrameF        b ) (U m) (r := C) (M a)
 -> IFreeT (FrameF (Maybe b)) (U m) (r := C) (M a)
stack t p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Wrap (Close   p') -> wrap $ Close $ warn p'
        Wrap (Yield b p') -> wrap $ Yield (Just b) (stack t p')
        Wrap (Await   f ) ->
            let p' = wrap $ Await $ \e -> stack (isNothing e) (f e)
             in case t of
                    False -> p'
                    True  -> wrap $ Yield Nothing p'

warn :: (Monad m)
 => IFreeT (FrameF        b ) (U m) (r := C) C
 -> IFreeT (FrameF (Maybe b)) (U m) (r := C) C
warn p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Return r -> wrap $ Yield Nothing (returnI r)
        Wrap (Yield b p') -> wrap $ Yield (Just b) (warn p')

{- $compose
    The fourth step to convert 'Pipe' code to 'Frame' code is to replace ('<+<')
    with ('<-<'):

> printer <-< take' 3 <-< fromList [1..]

    Like 'Pipe's, Frames form a 'Category' where composition pipes the output
    from the upstream 'Frame' to the input of the downstream 'Frame'.
    Additionally, composition guarantees the following behaviors:

    * 'Frame's receive exactly one 'Nothing' if an upstream 'Frame' terminates.

    * Registered finalizers get called exactly once if a downstream 'Frame'
      terminates.

    * Finalizers are always ordered from upstream to downstream.

    As with 'Pipe's, the 'Category' laws are correct-by-construction and cannot
    be broken.

    Note that you may only compose 'Frame's that begin open and end closed.
-}

-- | Corresponds to ('<<<')/('.') from @Control.Category@
(<-<) :: Monad m
 => Frame c m (M b) C r -> Frame b m (M a) C r -> Frame c m (M a) C r
p1 <-< p2 = heap (return ()) p1 <~< stack False p2

-- | Corresponds to ('>>>') from @Control.Category@
(>->) :: Monad m
 => Frame b m (M a) C r -> Frame c m (M b) C r -> Frame c m (M a) C r
(>->) = flip (<-<)

-- | Corresponds to 'id' from @Control.Category@
idF :: (Monad m) => Frame a m (M a) C r
idF = foreverR $ await !>= yield

-- | 'Frame's form a 'Category' instance when you rearrange the type variables
newtype FrameC m r a b = FrameC { unFrameC :: Frame b m (M a) C r }

instance (Monad m) => Category (FrameC m r) where
    id = FrameC idF
    (FrameC p1) . (FrameC p2) = FrameC (p1 <-< p2)

{- $run
    The fifth step to convert 'Pipe' code to 'Frame' code is to use 'runFrame'
    instead of 'runPipe':

>>> runFrame $ printer <-< take' 3 <-< fromList [1..]
1
2
3
fromList interrupted
You shall not pass!
printer interrupted
>>> runFrame $ printer <-< take' 3 <-< fromList [1]
1
You shall not pass!
take' interrupted
printer interrupted

-}

{-|
    Run the 'Frame' monad transformer, converting it back to the base monad.

    'runFrame' is the 'Frame' equivalent to 'runPipe' and requires a
    self-contained 'Stack'.
-}
runFrame :: (Monad m) => Stack m r -> m r
runFrame p = do
    x <- unU $ runIFreeT p
    case x of
        Wrap (Close   p') -> runFrame' p'
        Wrap (Yield _ p') -> runFrame  p'
        Wrap (Await   f ) -> runFrame (f $ Just ())

runFrame' :: (Monad m) => Frame Void m C C r -> m r
runFrame' p = do
    x <- unU $ runIFreeT p
    case x of
        Return (V r)      -> return r
        Wrap (Yield _ p') -> runFrame'  p'
