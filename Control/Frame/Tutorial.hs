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

    -- * Folds
    -- $fold

    -- * Strictness
    -- $strict

    -- * Robustness
    -- $robust
    ) where

-- For documentation
import Control.Category
import Control.Frame
import Control.IMonad
import Control.IMonad.Trans
import Control.Monad.Trans.Class
import Control.Pipe hiding (await, yield, Await, Yield)

{- $restrict1
    'Frame's extend 'Pipe's with two new features:

    * Folding input and intercepting upstream termination

    * Guaranteeing prompt and deterministic finalization

    However, these extra features comes with some added complexity: restricted
    monads, also known as indexed monads.  Restricted monads sound scarier than
    they are, so I'll demonstrate that if you are comfortable using monads, then
    you'll be comfortable using restricted monads.

    Let's translate the @take'@ function from the 'Pipe's tutorial into a
    'Frame' to see what changes when we use restricted monads:

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
    This time I included all imports and highlighted the new @RebindableSyntax@
    extension.  The new imports belong to the @Control.IMonad@ hierarchy from
    the @index-core@ package, which provides the core restricted monad
    functionality.

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
    by hand.  Just import @Control.IMonad@ instead, drop the @RebindableSyntax@
    extension, and don't hide 'Monad'.  Then you can desugar @take'@ manually
    using the restricted monad operators:

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

    However, for this tutorial I will use the @do@ notation, since it's prettier
    and easier to use.

    You'll also notice functions that resemble the ones in @Control.Monad@,
    except with an \'@R@\' suffix on the end of them, like 'replicateMR_'.
    Most functions in @Control.Monad@ have a restricted counterpart provided by
    @Control.IMonad.Restrict@ (which is in turn re-exported by
    @Control.IMonad@), such as 'whenR', 'foreverR', and 'mapMR'.

    Also, every time you lift an operation from the base monad, you must use
    'liftU' instead of 'lift'.  'Frame's are \"restricted monad transformers\",
    and they would normally lift a base restricted monad using 'liftI', but
    they can also lift ordinary monads, too, using 'liftU' (mnemonic: \"lift\"
    an ordinary monad and \'U\'pgrade it to a restricted monad).
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
    function, which seals off and finalizes upstream.  You can see that the
    'close' primitive changes the index just by looking at its type:

> close :: Monad m => Frame b m (M a) C ()

    The 'close' instruction begins with an open input end (@M a@) and finishes
    with a closed input end (@C@).  If you tried to call 'close' twice, you'd
    get a type error:

> -- wrong!
> do close
>    close

    This prevents you from accidentally finalizing upstream twice.

    'close' is the only primitive that changes the index, and there is no way to
    reopen the input once you have closed it.  'close' also forbids you from
    'await'ing input from upstream after you have already closed it.  If you
    try, you will get a type error

> -- wrong!
> do close
>    await

    This prevents you from requesting input from a finalized pipe.  In fact,
    once you 'close' your input end, every upstream 'Frame' disappears
    completely.  You couldn't get input from upstream anyway, even if you
    somehow allowed 'await' statements after 'close'.

    You can check out 'await''s type signature to see why it won't type-check
    after 'close':

> await :: Monad m => Frame b m (M a) (M a) a

    'await' must begin with the input end open (@M a@) and it leaves the input
    end open when done (@M a@).  However, you can still use a 'yield' anywhere:

> yield :: Monad m => b -> Frame b m i i ()

    'yield' will work whether or not the input end is open, and it leaves the
    input end in the same state once 'yield' is done.
-}

{- $prompt
    Every 'Frame' must close its input end /exactly/ one time before you can
    compose it with other 'Frame's.  The only exception is if a 'Frame' never
    terminates:

> -- This type-checks because foreverR is polymorphic in the final index
> printer :: (Show b) => Frame C IO (M b) C r
> printer = foreverR $ do
>     a <- await
>     liftU $ print a

    However, when a 'Frame' no longer needs input then you should 'close' it as
    early as possible.  The earlier you 'close' upstream, the more promptly
    upstream gets finalized.

    If you write a stand-alone producer from start to finish, you can be sure it
    will never need upstream, so you can close it immediately:

> -- I'm keeping fromList's input end polymorphic for a later example
> fromList :: (M.Monad m) => [b] -> Frame b m (M a) C ()
> fromList xs = do
>     close
>     mapMR_ yield xs

    However, if @fromList@ were a library function, you would remove the 'close'
    statement as you cannot guarantee that your user won't want to 'await' after
    @fromList@.  Or, the user might want to call @fromList@ twice within the
    same 'Frame', and having two close statements would lead to a type error.
    Therefore, a good rule of thumb when writing library code for 'Frame's is to
    always let the user decide when to 'close' the 'Frame' unless you are
    writing a stand-alone 'Frame'.

    So for right now, I will leave the 'close' in @fromList@ for simplicity and
    treat it as a stand-alone 'Frame'.  Also, it will come in handy for a later
    example.
-}

{- $compose
    Composition works just like 'Pipe's, except you use the ('<-<') composition
    operator instead of ('<+<'):

> stack :: Stack IO ()
> stack = printer <-< take' 3 <-< fromList [1..]

    The 'Frame' equivalent to 'Pipeline' is a 'Stack' (mnemonic: call stack;
    also the name 'Frame' refers to a call stack frame):

> type Stack m r = Frame C m (M ()) C r

    Similarly, you use 'runFrame' instead of 'runPipe' to convert the 'Frame'
    back to the base monad:

>>> runFrame stack
1
2
3
You shall not pass!

    However, let's carefully inspect the type of composition:

> (<-<) :: Monad m
>  => Frame c m (M b) C r
>  -> Frame b m (M a) C r
>  -> Frame c m (M a) C r

    Each argument 'Frame' must begin in an open state and end in a closed state.
    This means that each 'Frame' in a 'Stack' must call 'close' exactly once
    before it may be used.  'runFrame' has the exact same restriction:

> runFrame :: Monad m => Stack m r -> m r
> runFrame ~  Monad m => Frame C m (M ()) C r -> m r

    Composition specifically requires the user to define when to finalize
    upstream and does not assume this occurs at the end of the 'Frame'.  This
    doesn't pose a problem for stand-alone 'Frame's, since they will know when
    they no longer need input, but smaller library components designed to be
    assembled into larger 'Frame's should let the user decide at the very last
    moment where to 'close' the 'Pipe'.  There is no way to know ahead of time
    where the 'close' should be until the complete 'Frame' has been assembled.
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
>     -- The following requires "import qualified Control.Monad as M"
>     finallyD (putStrLn "Closing file ..." M.>> hClose h) $ readFile' h

    The 'finallyD' function registers a block-level finalizer that executes if a
    downstream 'Pipe' terminates or if the block completes normally.  The more
    general 'finallyF' function will call the finalizer if /any/ 'Frame'
    terminates.

    Usually you would always want to use 'finallyF', but because of some type
    limitations you can only use 'finallyD' after a 'Frame' is closed.  A future
    release of this library will fix this and merge 'finallyD' into 'finallyF'.
    So that means that for everything beginning before a 'close' statement, use
    'finallyF', otherwise use 'finallyD'.

    Similarly, you can use the 'catchF' / 'catchD' counterparts to the
    \"finally\" functions.  The \"catch\" functions run the finalizer only if
    another 'Frame' terminates before the block is done, but not if the block
    terminates normally.

    We don't 'close' the @read'@ function because it's not a stand-alone
    'Frame'.  We want to be able to concatenate multiple @read'@s together
    within the same 'Frame', like so:

> files = do
>     close
>     read' "file1.txt"
>     read' "file2.txt"

    So let's assume those two files have the following contents:

    \"@file1.txt@\"

> Line 1
> Line 2
> Line 3

    \"@file2.txt@\"

> A
> B
> C

    We can now check to see if our @files@ producer works:

>>> runFrame $ printer <-< files
Opening file...
"Line1"
"Line2"
"Line3"
Closing file ...
Opening file...
"A"
"B"
"C"
Closing file ...

    More importantly, files are never opened if they aren't demanded and they
    are always properly finalized if the consumer terminates early:

>>> runFrame $ printer <-< take' 2 <-< files
Opening file...
"Line1"
"Line2"
Closing file ...
You shall not pass!

    So we get lazy, deterministic, and prompt resource management.  Nice!

-}

{- $fold
    'Frame's can actually do more than just manage finalization!  Using
    'Frame's, we can now correctly implement folds like @toList@ in a way that
    is truly compositional:

> toList :: (M.Monad m) => Frame b m (M a) (M a) [a]
> toList = do
>     a' <- awaitF
>     case a' of
>         Nothing -> return []
>         Just a  -> do
>             as <- toList
>             return (a:as)

    We used one new function this time: 'awaitF'.  This is like 'await' except
    that it returns a 'Nothing' if upstream terminates before 'yield'ing back a
    value.  This allows you to intercept upstream termination and do some
    cleanup, and in our case we use it to end the fold.

    You only receive a 'Nothing' once when you use 'awaitF'.  Any attempt to
    request more input after you receive the first 'Nothing' will terminate the
    current 'Frame' using the upstream return value.  In fact, 'await' is built
    on top of 'awaitF':

> await = do
>     a' <- awaitF
>     case a' of
>         Nothing -> await
>         Just a  -> return a

    If it gets a 'Nothing', it just ignores it and 'await's again, choosing to
    not do any cleanup.

    Now let's make sure our @toList@ function works.  I didn't make @toList@ a
    stand-alone 'Frame', so we will have to include a 'close' statement to
    complete it before composing it:

> p1 = do
>     xs <- toList
>     close
>     return (Just xs)
>
> p2 xs = do
>     fromList xs
>     return Nothing -- Remember: they need the same return type

>>> runFrame $ p1 <-< p2 [1..10]
Just [1,2,3,4,5,6,7,8,9,10]
-}

{- $strict
    Lazy resource management has one important disadvantage: we can't free the
    resource until downstream no longer needs input.  Many libraries duplicate
    their code to provide Lazy and Strict versions, allowing the user to decide
    if they want:

    * Lazy input, which conserves memory, but holds onto the resource until
      downstream is done processing it

    * Strict input, which loads everything into memory, but can then immediately
      dispose of the resource before the input is processed

    What if there were a way to seamlessly switch between those semantics or
    even choose something in between?  Well, it turns out we can!

    First, we can combine @fromList@ and @toList@ into something even cooler:

> strict :: (M.Monad m) => Frame a m (M a) C ()
> strict = do
>     xs <- toList
>     fromList xs

    As the name suggests, the @strict@ function is strict in its input.
    @strict@ loads the entire input into memory, finalizes upstream, then
    proceeds to hand the input off to downstream.  We can prove this just by
    using it:

>>> runFrame $ printer <-< strict <-< files
> Opening file...
> Closing file ...
> Opening file...
> Closing file ...
> "Line1"
> "Line2"
> "Line3"
> "A"
> "B"
> "C"

    Both files were disposed of immediately, at the expense of using more
    memory.

    But what if we want something in between strictness and laziness?  Maybe 
    something like this:

>>> runFrame $ printer <-< strict <-< take' 2 <-< files
Opening file...
Closing file ...
You shall not pass!
"Line1"
"Line2"

    Now we have the best of both worlds.  We can pick and choose how much of
    our source to strictly load into memory.  In the above example, we specified
    that we wanted to be strict only in the first two lines of our input, and as
    a result the third line of \"@file1.txt@\" is never read and \"@file2.txt@\"
    is never even opened!

    Now we have a way to seamlessly slide anywhere on the spectrum between
    laziness and strictness, and it's all implemented entirely within Haskell
    in a way that is elegant and intuitive without the use of artificial and
    clumsy 'seq' annotations.
-}

{- $robust
    The 'Frame' implementation exposes all internals, yet this does not
    compromise safety or invariants in any way.  The library's implementation is
    \"correct-by-construction\", meaning that you can extend it with your own
    features if you so choose, and you never have to worry about accidentally
    breaking any laws, such as the associativity of composition.

    This has the following important practical benefits for finalization and
    folds:

    * Finalizers never get duplicated or dropped

    * Folds can be performed anywhere within the 'Stack', not just at the most
      downstream 'Frame', as the @strict@ example illustrates.

    * You can reason about each 'Frame's finalization behavior completely
      independently of other 'Frame's.

    Composition elegantly handles every single corner case.  This directly
    follows from strictly enforcing the 'Category' laws, because categories have
    no corners!
-}
