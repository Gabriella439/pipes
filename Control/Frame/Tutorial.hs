{-|
    This module provides the tutorial for "Control.Frame".
-}

module Control.Frame.Tutorial (
    -- * Restricted Monads
    -- $restrict1

    -- $extension

    -- $restrict2

    -- * Type Signatures
    -- $types

    -- * Prompt Finalization
    -- $prompt

    -- * Composition
    -- $compose

    -- * Finalization
    -- $ensure

    -- * Frame Composition
    -- $framecompose

    -- * Frame vs. Ensure
    -- $frameensure

    -- * Folds
    -- $fold

    -- * Strictness
    -- $strict
    ) where

import Control.Frame
import Control.IMonad
import Control.IMonad.Trans
import Control.Monad.Trans.Class

{- $restrict1
    'Frame's extend 'Pipe's with two important features:

    * Folding input and intercepting upstream termination

    * Guaranteeing prompt and deterministic finalization

    However, these extra features comes with some added complexity: restricted
    monads.  Restricted monads sound scarier than they actually are, so I'll
    demonstrate that if you are comfortable with monads, then restricted monads
    are not a big leap.

    Let's translate the @take'@ function from the 'Pipe's tutorial into a
    'Frame' to see what changes when we start using restricted monads:

-}
-- $extension
-- > {-# LANGUAGE RebindableSyntax #-}
-- >
-- > import Control.Frame
-- > import Control.IMonad.Do
-- > import Control.IMonad.Trans
-- > import Prelude hiding (Monad(..))
-- >
-- > take' :: Int -> Frame a IO (M a) C ()
-- > take' n = do
-- >     replicateMR_ n $ do
-- >         x <- await
-- >         yield x
-- >     close
-- >     liftU $ putStrLn "You shall not pass!"
{- $restrict2
    I've included all imports this time and highlighted the new
    @RebindableSyntax@ extension.  The imports from the @Control.IMonad@
    hierarchy belong to the @index-core@ package, which provides the core
    restricted monad functionality.

    Yet, you almost wouldn't even know you were using an restricted monad just
    by looking at the code.  This is because @index-core@ can rebind @do@
    notation to use restricted monads instead of ordinary extensions.  Three
    things make this possible:

    * The @RebindableSyntax@ extension, which allows libraries to override
      @do@ syntax (among other things)

    * The @Control.IMonad.Do@ module which exports the new bindings for @do@
      notation

    * Hiding 'Monad' from the Prelude so that it does not conflict with the
      bindings from @index-core@

    However, you are not obligated to rebind @do@ notation to use 'Frame's.  You
    can choose to keep ordinary @do@ notation and desugar the restricted monad
    by hand.  Just import @Control.IMonad@ instead, drop the
    @RebindableSyntax@ extension, and don't hide 'Monad'.  Then you can desugar
    @take'@ manually using the restricted monad operators:

> import Control.Frame
> import Control.IMonad
> import Control.IMonad.Trans
>
> take' :: Int -> Frame a IO (M a) C ()
> take' n =
>     (replicateMR_ n $
>         await !>= \x -> 
>         yield x) !>= \_ ->
>     close        !>= \_ ->
>     liftU $ putStrLn "You shall not pass!"

    However, for this tutorial I will use the rebound @do@ notation, since it's
    prettier and easier to use.

    You'll also notice operators that resemble the ones in @Control.Monad@,
    except with an \'@R@\' suffix on the end of them, like 'replicateMR_'.
    Most functions in @Control.Monad@ have a restricted counterpart provided by
    @Control.IMonad.Restrict@ (which is in turn re-exported by
    @Control.IMonad@).

    Also, every time you lift an operation from the base monad, you must use
    'liftU' instead of 'lift'.  'Frame's are \"restricted monad transformers\",
    and they would normally lift the base restricted monad using 'liftI', but
    they can also lift ordinary monads, too, using 'liftU' (mnemonic: \"lift\"
    and \'U\'pgrade to a restricted monad).
-}

{- $types
    The 'Frame' type constructor also looks a bit different, too:

> Frame a IO (M a) C ()

    Let's dissect that to understand how 'Frame's work:

>       | Output | Base monad | Initial Input | Final Input | Return Value
> Frame   a        IO           (M a)           C             ()

    'Frame's differ from 'Pipe's in that their input end indexes the beginning
    and end of the operation.  Our @take'@ function starts off with an open
    input end (@M a@), and ends with a closed input end (@C@).

    @take'@ finishes with a closed input end because it called the 'close'
    function, which seals off and finalizes upstream.  You can see that 'close'
    changes the index just by looking at its type:

> close :: Monad m => Frame b m (M a) C ()

    The 'close' instruction begins with an open input end (@M a@) and finishes
    with a closed input end.  If you tried to call 'close' twice, you'd get a
    type error:

> -- wrong!
> do close
>    close

    This prevents you from accidentally finalizing upstream twice.

    'close' also forbids you from 'await'ing input from upstream after you have
    already closed it.  If you try, you will get a type error

> -- wrong!
> do close
>    await

    This prevents you from requesting input from a finalized pipe.  In fact,
    once you 'close' upstream, it disappears from the 'Pipeline' completely.
    You couldn't get input from upstream even if you somehow allowed 'await'
    statements after 'close'.

    You can check out 'await''s type signature to see why it won't type-check
    after 'close':

> await :: Monad m => Frame b m (M a) (M a) a

    'await' must begin with the input end open (@M a@) and it leaves the input
    end open when done (@M a@).  However, you can use a 'yield' anywhere:

> yield :: Monad m => b -> Frame b m i i ()

    'yield' will work whether or not the input end is open, and it leaves the
    input end in the same state it found it in.

    Also, if a 'Frame' never terminates, there is no need to explicitly close
    it, especially if it indefinitely requires input:

> printer :: (Show b) => Frame Void IO (M b) C r
> printer = foreverR $ do
>     a <- await
>     liftU $ print a
-}

{- $prompt
    However, when a 'Frame' no longer needs input then you should 'close' it as
    early as possible.  The earlier you 'close' upstream, the more promptly
    upstream gets finalized.

    For example, if you write a stand-alone producer from start to finish, you
    can be sure it will never need upstream, so you can close it immediately:

> fromList :: (M.Monad m) => [b] -> Frame b m (M ()) C ()
> fromList xs = do
>     close
>     mapMR_ yield xs

    However, if you wanted to provide @fromList@ as a library function, you
    would remove the 'close' statement as you cannot guarantee that your user
    won't want to 'await' after @fromList@.  Therefore, a good rule of thumb is
    to always let the user decide when to 'close' the 'Frame' unless you intend
    you component to be a stand-alone 'Frame'.

    For simplicity, I will treat @fromList@ as a stand-alone 'Frame' and include
    'close' for the purposes of this tutorial.
-}

{- $compose
    Composition works just like 'Pipe's, except you use the ('<-<') composition
    operator instead of ('<+<'):

stack :: Stack IO ()
stack = printer <-< take' 3 <-< fromList [1..]

    The 'Frame' equivalent to 'Pipeline' is a 'Stack' (mnemonic: call stack;
    also the name 'Frame' refers to a frame on a call stack):

> type Stack m r = Frame Void m (M ()) C r

    Similarly, you use 'runFrame' instead of 'runPipe':

>>> runFrame stack
1
2
3

    However, carefully check out the type of composition:

> (<-<) :: Monad m
>  => Frame c m (M b) C r
>  -> Frame b m (M a) C r
>  -> Frame c m (M a) C r

    Each argument 'Frame' must begin in an open state and end in a closed state.
    This means that each 'Frame' in a 'Stack' must call 'close' exactly once
    before it may be used.  'runFrame' has the exact same restriction:

> runFrame :: Monad m => Stack m r -> m r
>          ~  Monad m => Frame Void m (M ()) C r -> m r

    Composition specifically requires the user to define when to finalize
    upstream and does not assume this occurs at the end of the 'Frame', since
    this would violate either the 'Monad' or 'Category' laws.  For stand-alone
    'Frame's this is not a problem, since they will know when they no longer
    need input, but smaller library components designed to be assembled into
    complete 'Frame's should let the user decide at the last moment where to
    'close' the 'Pipe' since there is no way to know ahead of time when the
    user wants to stop 'await'ing input.
-}

{- $ensure
    With 'Frame's in hand, we can now write a safe @read'@ function:

> readFile' :: Handle -> Frame Text IO C C ()
> readFile' h = do
>     eof <- liftU $ hIsEOF h
>     whenR (not eof) $ do
>         s <- liftU $ hGetLine h
>         yield s
>         readFile' h
> 
> read' :: FilePath -> Frame Text IO C C ()
> read' file = do
>     liftU $ putStrLn "Opening file..."
>     h <- liftU $ openFile file ReadMode
>     -- The following requires "import qualified Prelude as M"
>     finallyD (putStrLn "Closing file ..." M.>> hClose h) $ readFile' h

    The 'finallyD' function registers a block-level finalizer that is guaranteed
    to execute if a downstream 'Pipe' terminates or if the block completes
    normally.  The more general 'finallyF' function will call the finalizer if
    any 
-}

{- $framecompose

    Just like with 'Pipe's, we can compose 'Frame's, except now we use ('<-<'):

> stack :: Frame Void () IO ()
> stack = printer <-< take' 1 <-< read' "test.txt"

    I call a complete set of 'Frame's a 'Stack', to reflect the fact that
    'Frame' composition uses the exact same tricks stack-based programming uses
    to guarantee deterministic finalization.  When a 'Frame' terminates it
    finalizes upstream 'Frame's as if they were a heap and it propagates an
    exceptional value ('Nothing' in this case) for downstream 'Frame's to
    intercept.  I provide a type synonym to reflect this:

> type Stack m r = Frame Void () IO r

    So we can rewrite the type of @stack@ to be:

> stack :: Stack IO ()

    To run a 'Stack', we use 'runFrame', which is the 'Frame'-based analog to
    'runPipe':

>>> runFrame stack
Opening file ...
"Line 1"
Closing file ...
"Line 2"
You shall not pass!

    Not only did it correctly finalize the file this time, but it did so as
    promptly as possible!  I programmed @take'@ so that it knew it would not
    need @read'@ any longer before it 'yield'ed the second value, so it
    finalized the file before 'yield'ing the second value for @printer@.
    @take'@ did this without knowing anything about the 'Frame' upstream of it.
    One of the big advantages of 'Frame's is that you can reason about the
    finalization behavior of each 'Frame' in complete isolation from other
    'Frame's, allowing you to completely decouple their finalization
    behavior.
-}

{- $frameensure
    Unfortunately, in the absence of extensions I have to split the 'Monad' and
    'Category' into two separate types.  'Ensure' is the 'Monad', 'Frame' is the
    'Category'.

    However, you can achieve the best of both worlds by programming all your
    'Pipe's in the 'Ensure' monad, and then only adding 'close' at the last
    minute when you are building your 'Stack'.  For example, if we wanted to
    read from multiple files, it would be much better to just remove the 'close'
    function from the @read'@ implementation, so it operates in the 'Ensure'
    monad:

> read' :: FilePath -> Ensure () Text IO ()

    Then use 'close' only after we've already concatenated our files:

> files :: Frame () Text IO ()
> files = close $ do
>     read' "test.txt"
>     read' "dictionary.txt"
>     read' "poem.txt"

    This is a more idiomatic 'Frame' programming style that lets you take
    advantage of both the 'Monad' and 'Category' instances.

    The beauty of compositional finalization is we can decompose complicated
    problems into smaller ones.  Imagine that we have a resource that needs a
    fine-grained finalization behavior like in our @take'@ function which does
    a cute little optimization to finalize early.  We can always decompose our
    frame into one that does the straight-forward thing (like @read'@) and then
    just compose it with @take'@ to get the cute optimization for free.  In this
    way we've decomposed the problem into two separate problems: generating the
    resource and doing the cute optimization.
-}

{- $fold
    'Frame's can actually do much more than manage finalization!  Using
    'Frame's, we can now correctly implement folds like @toList@ in a way that
    is truly compositional:

> toList :: (Monad m) => Frame a Void m [a]
> toList = Frame go where
>     go = do
>         x <- await
>         case x of
>             Nothing -> close $ pure []
>             Just a  -> fmap (fmap (a:)) go
>             -- the extra fmap is an unfortunate extra detail

    This time I used an ordinary 'await', instead of 'awaitF', so I could access
    the underlying 'Maybe' values that these 'Frame's are passing around.
    Similarly, you could use 'yield' instead of 'yieldF' if you wanted to
    manually manage the finalizers passed downstream at each 'yield' statement
    instead of using the 'catchP' or 'finallyP' convenience functions.  Using
    these advanced features does not break any of the 'Category' laws.  I could
    expose every single internal of the library and you would not be able to
    break the 'Category' laws because the 'Frame's generated are still
    indistinguishable at the value level and fuse into the hand-written
    implementation.  The compositionality of 'Frame's is just as strong as the
    compositionality of 'Pipe's.

    Now let's use our @toList@ function:

>>> runFrame $ (Just <$> toList) <-< (Nothing <$ fromList [1..3])
Just [1,2,3]

    I still had to provide a return value for @fromList@ ('Nothing' in this
    case), because when @fromList@ terminates, it cannot guarantee that its
    return value will come from itself or from @toList@.  When @toList@ receives
    a 'Nothing' from upstream, it can choose to terminate and over-ride the
    return value from upstream or 'await' again and defer to the upstream return
    value (@fromList@ in this case).  It doesn't even have to immediately
    decide.  It could just 'yield' more values downstream and forget it had even
    received a 'Nothing' and if downstream terminates then composition will
    still ensure that everything \"just works\" the way you would expect and no
    finalizers are missed or duplicated.

    Composition handles every single corner case of finalization.  This directly
    follows from enforcing the 'Category' laws, because categories have no
    corners!
-}

{- $strict
    We can go a step further and modify @toList@ into something even cooler:

> strict :: (Monad m) => Frame a a m ()
> strict = Frame $ do
>     xs <- go
>     close $ mapM_ yieldF xs
>   where
>     go = do
>         x <- await
>         case x of
>             Nothing -> pure []
>             Just a  -> fmap (a:) go

    As the name suggests, @strict@ is strict in its input.  We can use @strict@
    to load the entire resource into memory immediately, allowing us to finalize
    it early.  Let's use this to create a strict version of our file reader:

>>> runFrame $ printer <-< take' 2 <-< strict <-< read' "test.txt"
Opening file ...
Closing file ...
"Line 1"
"Line 2"
You shall not pass!

    Now we have a way to seamlessly switch from laziness to strictness all
    implemented entirely within Haskell without the use of artificial 'seq'
    annotations.
-}
