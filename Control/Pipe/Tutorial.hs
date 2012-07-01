{-|
    This module provides the tutorial for "Control.Pipe".
-}

module Control.Pipe.Tutorial (
    -- * Types
    -- $type

    -- * Composition
    -- $compose

    -- * Modularity
    -- $modular

    -- * Vertical Concatenation
    -- $vertical

    -- * Return Values
    -- $return

    -- * Termination
    -- $terminate

    -- * Resource Management
    -- $resource

    -- *Frames
    -- $frames
    ) where

-- For documentation
import Control.Category
import Control.Frame hiding (await, yield)
import Control.Monad.Trans.Class
import Control.Pipe
import Data.Void

{- $type
    This library represents streaming computations using a single data type:
    'Pipe'.

    'Pipe' is a monad transformer that extends the base monad with the ability
    to 'await' input from or 'yield' output to other 'Pipe's.  'Pipe's resemble
    enumeratees in other libraries because they receive an input stream and
    transform it into a new output stream.

    I'll introduce our first 'Pipe', which is a verbose version of the Prelude's
    'take' function:

> take' :: Int -> Pipe a a IO ()
> take' n = do
>     replicateM_ n $ do
>         x <- await
>         yield x
>     lift $ putStrLn "You shall not pass!"

    This 'Pipe' forwards the first @n@ values it receives undisturbed, then it
    outputs a cute message.

    Let's dissect the above 'Pipe''s type to learn a bit about how 'Pipe's work:

>      | Input Type | Output Type | Base monad | Return value
> Pipe   a            a             IO           ()

    So @take'@ 'await's input values of type \'@a@\' from upstream 'Pipe's and
    'yield's output values of type \'@a@\' to downstream 'Pipe's.  @take'@ uses
    'IO' as its base monad because it invokes the 'putStrLn' function.  If we
    were to remove the call to 'putStrLn', the compiler would infer the
    following type instead, which is polymorphic in the base monad:

> take' :: (Monad m) => Int -> Pipe a a m ()

    Now let's create a function that converts a list into a 'Pipe' by 'yield'ing
    each element of the list:

> fromList :: (Monad m) => [b] -> Pipe a b m ()
> fromList = mapM_ yield

    Note that @fromList xs@ is polymorphic in its input.  This is because it
    does not 'await' any input.  If we wanted, we could type-restrict it to:

> fromList :: (Monad m) => [b] -> Pipe () b m ()

    There is no type that forbids a 'Pipe' from 'await'ing, but you can
    guarantee that if it does 'await', the request is trivially satisfiable by
    supplying it with @()@.

    A 'Pipe' that doesn't 'await' (any useful input) can serve as the first
    stage in a 'Pipeline'.  I provide a type synonym for this common case:

> type Producer b m r = Pipe () b m r

    'Producer's resemble enumerators in other libraries because they function as
    data sources.

    You can then use the 'Producer' type synonym to rewrite the type signature
    for @fromList@ as:

> fromList :: (Monad m) => [b] -> Producer b m ()

    Now let's create a 'Pipe' that prints every value delivered to it:

> printer :: (Show b) => Pipe b c IO r
> printer = forever $ do
>     x <- await
>     lift $ print x

    Here, @printer@ is polymorphic in its output.  We could type-restrict it to
    guarantee it will never 'yield' by setting the output to 'Void', from
    @Data.Void@:

> printer :: (Show b) => Pipe b Void IO r

    A 'Pipe' that never 'yield's can be the final stage in a 'Pipeline'.  Again,
    I provide a type synonym for this common case:

> type Consumer b m r = Pipe b Void m r

    So we could instead write @printer@'s type as:

> printer :: (Show b) => Consumer b IO r

    'Consumer's resemble iteratees in other libraries because they function as
    data sinks.
-}

{- $compose
    What distinguishes 'Pipe's from every other iteratee implementation is that
    they form a true 'Category'.  Because of this, you can literally compose
    'Pipe's into 'Pipeline's using ordinary composition:

> newtype PipeC m r a b = PipeC { unPipeC :: Pipe a b m r }
> instance Category (PipeC m r) where ...

    For example, you can compose the above 'Pipe's with:

> pipeline :: Pipe () Void IO ()
> pipeline = unPipeC $ PipeC printer . PipeC (take' 3) . PipeC (fromList [1..])

    The compiler deduces that the final 'Pipe' must be blocked at both ends,
    meaning it will never 'await' useful input and it will never 'yield' any
    output.  This represents a self-contained 'Pipeline' and I provide a type
    synonym for this common case:

> type Pipeline m r = Pipe () Void m r

    Also, I provide '<+<' as a convenience operator for composing 'Pipe's
    without the burden of wrapping and unwrapping newtypes:

> p1 <+< p2 == unPipeC $ PipeC p1 . PipeC p2

    So you can rewrite @pipeline@ as:

> pipeline :: Pipeline IO ()
> pipeline = printer <+< take' 3 <+< fromList [1..]

    Like many other monad transformers, you convert the 'Pipe' monad back to the
    base monad using some sort of \"@run...@\" function.  In this case, it's the
    'runPipe' function:

> runPipe :: (Monad m) => Pipeline m r -> m r

    'runPipe' only works on self-contained 'Pipeline's, but you don't need to
    worry about explicitly type-restricting any of your 'Pipe's.  Self-contained
    'Pipeline's will automatically have polymorphic input and output ends and
    they will type-check when you provide them to 'runPipe'.

    Let's try using 'runPipe':

>>> runPipe pipeline
1
2
3
You shall not pass!

    Fascinating!  Our 'Pipe' terminates even though @printer@ never terminates
    and @fromList@ never terminates when given an infinite list.  To illustrate
    why our 'Pipe' terminates, let's outline the 'Pipe' flow control rules for
    composition:

    * 'Pipe's are lazy, so execution begins at the most downstream 'Pipe'
      (@printer@ in our example).

    * Upstream 'Pipe's only run if input is requested from them and they only
      run as long as necessary to 'yield' back a value.

    * If a 'Pipe' terminates, it terminates every other 'Pipe' composed with it.

    Another way to think of this is like a stack where each 'Pipe' is a frame on
    that stack:

    * If a 'Pipe' 'await's input, it blocks and pushes the next 'Pipe' upstream
      onto the stack until that 'Pipe' 'yield's back a value.

    * If a 'Pipe' 'yield's output, it pops itself off the stack and restores
      control to the original downstream 'Pipe' that was 'await'ing its input.
      This binds its result to the return value of the pending 'await' command.

    All of these flow control rules uniquely follow from the 'Category' laws.

    It might surprise you that termination brings down the entire 'Pipeline'
    until you realize that:

    * Downstream 'Pipe's depending on the terminated 'Pipe' cannot proceed

    * Upstream 'Pipe's won't be further evaluated because the terminated 'Pipe'
      will not request any further input from them

    So in our previous example, the 'Pipeline' terminated because \"@take' 3@\"
    terminated and brought down the entire 'Pipeline' with it.

    Actually, these flow control rules will mislead you into thinking that
    composed 'Pipe's behave as a collection of sub-'Pipe's with some sort of
    message passing architecture between them, but nothing could be further from
    the truth! When you compose 'Pipe's, they automatically fuse into a single
    'Pipe' that corresponds to how you would have written the control flow by
    hand.

    For example, if you compose @printer@ and @fromList@:

> printer <+< fromList [1..]

    The result is indistinguishable from:

> lift (mapM_ print [1..])

    ... which is what we would have written by hand if we had not used 'Pipe's
    at all!  All 'runPipe' does is just remove the 'lift'!
-}

{- $modular
    Given a loop like:

> loop :: IO r
> loop = forever $ do
>     x <- dataSource
>     y <- processData x
>     dataSink y

    We could decompose it into three separate parts:

> stage1 :: Producer a IO r
> stage1 = forever $ do
>     x <- dataSource
>     yield x
>
> stage2 :: Pipe a b IO r
> stage2 = forever $ do
>     x <- await
>     y <- processData x
>     yield y
>
>
> stage3 :: Consumer b IO r
> stage3 = forever $ do
>     y <- await
>     dataSink y
>
> stage3 <+< stage2 <+< stage1 = lift loop

    In other words, 'Pipe's let you decompose loops into modular components,
    which promotes loose coupling and allows you to freely mix and match those
    components.

    To demonstrate this, let's define a new data source that indefinitely
    prompts the user for integers:

> prompt :: Producer Int IO a
> prompt = forever $ do
>     lift $ putStrLn "Enter a number: "
>     n <- read <$> lift getLine
>     yield n

    Now we can use it as a drop-in replacement for @fromList@:

>>> runPipe $ printer <+< take' 3 <+< prompt
Enter a number:
1<Enter>
1
Enter a number:
2<Enter>
2
Enter a number:
3<Enter>
3
You shall not pass!

-}

{- $vertical
    You can easily \"vertically\" concatenate 'Pipe's, 'Producer's, and
    'Consumer's, all using simple monad sequencing: ('>>').  For example, here
    is how you concatenate 'Producer's:

>>> runPipe $ printer <+< (fromList [1..3] >> fromList [10..12])
1
2
3
10
11
12

    Here's how you would concatenate 'Consumer's:

>>> let print' n = printer <+< take' n :: (Show a) => Int -> Consumer a IO ()
>>> runPipe $ (print' 3 >> print' 4) <+< fromList [1..]
1
2
3
You shall not pass!
4
5
6
7
You shall not pass!

   ... but the above example is gratuitous because we could have just
   concatenated the intermediate @take'@ 'Pipe':

>>> runPipe $ printer <+< (take' 3 >> take' 4) <+< fromList [1..]
1
2
3
You shall not pass!
4
5
6
7
You shall not pass!

-}

{- $return
    'Pipe' composition imposes an important requirement: You can only compose
    'Pipe's that have the same return type.  For example, I could write the
    following function:

> deliver :: (Monad m) => Int -> Consumer a m [a]
> deliver n = replicateM n await

    ... and I might try to compose it with @fromList@:

>>> runPipe $ deliver 3 <+< fromList [1..10] -- wrong!

    ... but this wouldn't type-check, because @fromList@ has a return type of
    @()@ and @deliver@ has a return type of @[Int]@.  Composition requires that
    every 'Pipe' has a return value ready in case it terminates first.

    Fortunately, we don't have to rewrite the @fromList@ function because we can
    just add a return value using vertical concatenation:

>>> runPipe $ deliver 3 <+< (fromList [1..10] >> return [])
[1,2,3]

    ... although a more idiomatic Haskell version would be:

>>> runPipe $ (Just <$> deliver 3) <+< (fromList [1..10] *> pure Nothing)
Just [1,2,3]

    This forces you to cover all code paths by thinking about what return value
    you would provide if something were to go wrong.  For example, let's say I
    were to make a mistake and request more input than @fromList@ can deliver:

>>> runPipe $ (Just <$> deliver 99) <+< (fromList [1..10] *> pure Nothing)
Nothing

    The type system saved me by forcing me to cover all corner cases and handle
    every way my program could terminate.
-}

{- $terminate

    Now what if you wanted to write a 'Pipe' that only reads from its input end
    (i.e. a 'Consumer') and returns a list of every value delivered to it when
    its input 'Pipe' terminates?

> toList :: (Monad m) => Consumer a m [a]
> toList = ???

    You can't write such a 'Pipe' because if its input terminates then it brings
    down @toList@ with it!  This is correct because @toList@ as defined is not
    compositional (yet!).

    To see why, let's say you somehow got @toList@ to work and the following
    imaginary code sample worked:

>>> runPipe $ toList <+< (fromList [1..5] >> return [])
[1,2,3,4,5]

    @toList@ is defined to return its value when the 'Pipe' immediately upstream
    (@fromList@ in this case) terminates.  This behavior immediately leads to a
    problem.  What if I were to insert an \"identity\" 'Pipe' between @toList@
    and @fromList@:

> identity = forever $ await >>= yield
> -- This is how id is actually implemented!

    This 'Pipe' forwards every valued untouched, so we would expect it to not
    have any affect if we were to insert it in the middle:

>>> runPipe $ toList <+< identity <+< (fromList [1..5] >> return [])
??? -- Oops! Something other than [1,2,3,4,5], perhaps even non-termination

    The answer couldn't be @[1,2,3,4,5]@ because @toList@ would monitor 
    @identity@ instead of @fromList@ and since @identity@ never terminates
    @toList@ never terminates.  This is what I mean when I say that @toList@'s
    specified behavior is non-compositional.  It only works if it is coupled
    directly to the desired 'Pipe' and breaks when you introduce intermediate
    stages.

    This was not an intentional design choice, but rather a direct consequence
    of enforcing the 'Category' laws when I was implementing 'Pipe''s 'Category'
    instance.  Satisfying the 'Category' laws forces code to be compositional.

    Note that a terminated 'Pipe' only brings down 'Pipe's composed with it.  To
    illustrate this, let's use the following example:

> p = do a <+< b
>        c

    @a@, @b@, and @c@ are 'Pipe's, and @c@ shares the same input and output as
    the composite 'Pipe' @a <+< b@, otherwise we cannot combine them within the
    same monad.  In the above example, either @a@ or @b@ could terminate and
    bring down the other one since they are composed, but @c@ is guaranteed to
    continue after @a <+< b@ terminates because it is not composed with them.
    Conceptually, we can think of this as @c@ automatically taking over the
    'Pipe''s channeling responsibilities when @a <+< b@ can no longer continue.
    There is no need to \"restart\" the input or output manually as in some
    other iteratee libraries.

    The @pipes@ library, unlike other iteratee libraries, grounds its vertical
    and horizontal concatenation in category theory by deriving horizontal
    concatenation ('.') from its 'Category' instance and vertical concatenation
    ('>>') from its 'Monad' instance.  This makes it easier to reason about
    'Pipe's because you can leverage your intuition about 'Category's and
    'Monad's to understand their behavior.  The only 'Pipe'-specific primitives
    are 'await' and 'yield'.
-}

{- $resource
    Here's another problem with 'Pipe' composition: resource finalization.
    Let's say we have the file \"test.txt\" with the following contents:

> Line 1
> Line 2
> Line 3

  .. and we wish to lazily read one line at a time from it:

> readFile' :: Handle -> Producer Text IO ()
> readFile' h = do
>     eof <- lift $ hIsEOF h
>     when (not eof) $ do
>         s <- lift $ hGetLine h
>         yield s
>         readFile' h

    We could then try to be slick and write a lazy version that only reads as
    many lines as we request:

> read' :: FilePath -> Producer Text IO ()
> read' file = do
>     lift $ putStrLn "Opening file ..."
>     h <- lift $ openFile file ReadMode
>     readFile' h
>     lift $ putStrLn "Closing file ..."
>     lift $ hClose h

    Now compose!

>>> runPipe $ printer <+< read' "test.xt"
Opening file ...
"Line 1"
"Line 2"
"Line 3"
Closing file ...

    So far, so good.  Equally important, the file is never opened if we replace
    @printer@ with a 'Pipe' that never demands input:

>>> runPipe $ (lift $ putStrLn "I don't need input") <+< read' "test.txt"
I don't need input

    There is still one problem, though. What if we wrote:

>>> runPipe $ printer <+< take' 2 <+< read' "test.txt"
Opening file ...
"Line 1"
"Line 2"
You shall not pass!

    Oh no!  While it was lazy and only read two lines from the file, it was also
    too lazy to properly close our file!  \"@take' 2@\" terminated before
    @read'@, preventing @read'@ from properly closing \"test.txt\".  This is why
    'Pipe' composition fails to guarantee deterministic finalization.
-}

{- $frames
    So with 'Pipe's, we can neither write folds, nor can we finalize resources
    deterministically.  Fortunately, there is a solution: 'Frame's.  Check out
    "Control.Frame.Tutorial" for an introduction to a type that enriches 'Pipe's
    with the ability to fold and finalize resources correctly.
-}