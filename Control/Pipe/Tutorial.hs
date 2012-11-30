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

    -- * Folds
    -- $folds

    -- * Resource Management
    -- $resource

    -- * Bidirectional Pipes
    -- $bidirectional
    ) where

-- For documentation
import Control.Category
import Control.Monad.Trans.Class
import Control.Proxy

{- $type
    The @pipes@ library simplifies writing streaming computations layered on top
    of \"proxies\".

    Let's begin with the simplest proxy, a 'Producer':

> import Control.Monad
> import Control.Monad.Trans.Class (lift)
> import Control.Proxy
> 
> promptInt :: (Proxy p) => () -> Producer p Integer IO r
> promptInt () = runIdentityP $ forever $ do
>     lift $ putStrLn "Enter an Integer:"
>     n <- lift readLn
>     respond n

    The above 'Producer' prompts for 'Integer's from the user and then sends the
    integers downstream using 'respond'.

    The next simplest proxy is a 'Consumer':

> printer :: (Show a, Proxy p) => () -> Consumer a IO r
> printer () = runIdentityP $ forever $ do
>     a <- request ()
>     lift $ putStrLn "Received a value:"
>     lift $ print a

    The above 'Consumer' 'request's 'Show'able values from upstream, and then
    'print's them.

    We connect a 'Consumer' and a 'Producer' using '(<-<)', and run the result
    using 'runProxy':

> runProxy $ printer <-< promptInt
Enter an Integer:
1<Enter>
Received a value:
1
Enter an Integer:
5<Enter>
Received a value:
5
...

    This proceeds endlessly until we hit @Ctrl-C@ to interrupt it.  We would
    like to limit the number of iterations, so lets define an intermediate
    'Pipe' which behaves like a verbose 'take':

> take' :: (Monad m, Proxy p) => Int -> () -> Pipe a a m ()
> take' n () = runIdentityP $ replicateM_ n $ do
>     a <- request ()
>     respond a

    The simplest proxy type this library provides is a 'Pipe', which is a
    unidirectional proxy.  A 'Pipe' is a monad transformer that extends the base
    monad with the ability to 'request' input from upstream 'Pipe's or
    'respond' with output to downstream 'Pipe's.

    I'll introduce our first 'Pipe', which is a verbose version of the Prelude's
    'take' function:

> -- Ignore the 'runIdentityP' and extra ()'s for now
> take' :: (Proxy p) => Int -> () -> Pipe p a a IO ()
> take' n () = runIdentityP $ do
>     replicateM_ n $ do
>         x <- request ()
>         respond x
>     lift $ putStrLn "You shall not pass!"

    This 'Pipe' forwards the first @n@ values it receives undisturbed, then it
    outputs a cute message.

    Let's dissect the above 'Pipe''s type to learn a bit about how 'Pipe's work:

>      | Underlying     | Input | Output | Base  | Return
>      | Implementation | Type  | Type   | Monad | Value
> Pipe   p                a       a        IO      ()

    So @take'@ 'request's input values of type \'@a@\' from upstream 'Pipe's and
    'respond's with output values of type \'@a@\' to downstream 'Pipe's.
    @take'@ uses 'IO' as its base monad because it invokes the 'putStrLn'
    function.  If we remove the call to 'putStrLn', the type would be
    polymorphic in the base monad:

> take' :: (Monad m, Proxy p) => Int -> () -> Pipe p a a m ()

    Now let's create a function that converts a list into a 'Pipe' by 'yield'ing
    each element of the list:

> fromList :: (Monad m, Proxy p) => [b] -> () -> Pipe p a b m ()
> fromList bs () = runIdentityP $ mapM_ respond bs

    Note that @fromList xs@ is polymorphic in its input because it does not
    'request' anything, so we could type restrict it to not 'request' any
    useful input:

> fromList :: (Monad m, Proxy p) => [b] -> () -> Pipe () b m ()

    A 'Pipe' that doesn't 'request' (any useful input) can serve as the first
    stage in a 'Pipeline'.  I provide a type synonym for this common case:

> -- The true Producer type synonym differs slightly
> type Producer p b m r = Pipe p () b m r

    A 'Producer' is just a data source that can use the base monad to generate
    more data.

    You can then use the 'Producer' type synonym to rewrite the type signature
    for @fromList@ as:

> fromList :: (Monad m, Proxy p) => [b] -> () -> Producer p b m ()

    Now let's create a 'Pipe' that prints every value delivered to it:

> printer :: (Show b, Proxy p) => () -> Pipe p b c IO r
> printer () = runIdentityP $ forever $ do
>     x <- request ()
>     lift $ print x

    Here, @printer@ is polymorphic in its output.  We could type-restrict it to
    guarantee it will never 'respond' by setting the output to 'C', an unhabited
    type that \'@C@\'loses the output end:

> printer :: (Show b, Proxy p) => () -> Pipe p b C IO r

    A 'Pipe' that never 'respond's can be the final stage in a 'Pipeline'.
    Again, I provide a type synonym for this common case:

> type Consumer p b m r = Pipe p b C m r

    So we could instead write @printer@'s type as:

> printer :: (Show b, Proxy p) => () -> Consumer p b IO r

    A 'Consumer' is a data sink that can run actions in the base monad depending
    on what kind of data it receives.
-}

{- $compose
    What distinguishes 'Pipe's from every other iteratee implementation is that
    they form a true 'Category'.  Because of this, you can literally compose
    'Pipe's into 'Pipeline's using ordinary composition:

> newtype PipeC m r a b = PipeC { unPipeC :: Pipe a b m r }
> instance Category (PipeC m r) where ...

    For example, you can compose the above 'Pipe's with:

> pipeline :: Pipe () C IO ()
> pipeline = unPipeC $ PipeC printer . PipeC (take' 3) . PipeC (fromList [1..])

    The compiler deduces that the final 'Pipe' must be blocked at both ends,
    meaning it will never 'await' useful input and it will never 'yield' any
    output.  This represents a self-contained 'Pipeline' and I provide a type
    synonym for this common case:

> type Pipeline m r = Pipe () C m r

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

    * When a 'Pipe' 'await's, it blocks until it receives input from the next
      'Pipe' upstream

    * When a 'Pipe' 'yield's, it blocks until it receives a new 'await' request
      from downstream.

    * If a 'Pipe' terminates, it terminates every other 'Pipe' composed with it.

    All of these flow control rules uniquely follow from the 'Category' laws.

    It might surprise you that termination brings down the entire 'Pipeline'
    until you realize that:

    * Downstream 'Pipe's depending on the result from the terminated 'Pipe'
      cannot proceed

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

{- $folds
    While we cannot intercept termination, we can still fold our input.  We can
    embed 'WriterT' in our base monad, since 'Pipe' is a monad transformer, and
    store the result in the monoid:

> toList :: Consumer a (WriterT [a] m) r
> toList = forever $ do
>     a <- await
>     lift $ tell [a]

>>> execWriterT $ runPipe $ toList <+< fromList [1..4]
[1,2,3,4]

    But what if other pipes have a base monad that is not compatible, such as:

> prompt3 :: Producer Int IO a
> prompt3 = take' 3 <+< prompt

    That's okay, because we can transparently 'lift' any Pipe's base monad,
    using 'hoistFreeT' from @Control.Monad.Trans.Free@ in the @free@ package:

>>> execWriterT $ runPipe $ toList <+< hoistFreeT lift prompt3
3<Enter>
4<Enter>
6<Enter>
[3,4,6]

-}

{- $resource
    Pipes handle streaming computations well, but do not handle resource
    management well.  To see why, let's say we have the file \"@test.txt@\"
    with the following contents:

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

    The "Control.Frame" module of this library provides a temporary solution to
    this problem, but in the longer run there will be a more elegant solution
    built on top of "Control.Proxy".
-}

{- $bidirectional
    The 'Pipe' type suffers from one restriction: it only handles a
    unidirectional flow of information.  If you want a bidirectional 'Pipe'
    type, then use the 'Proxy' type from "Control.Proxy", which generalizes the
    'Pipe' type to bidirectional flow.

    More importantly, the 'Proxy' type is a strict superset of the 'Pipe' type,
    so all 'Pipe' utilities and extensions are actually written as 'Proxy'
    utilities and extensions, in order to avoid code duplication.

    So if you want to use these extensions, import "Control.Proxy" instead,
    which exports a backwards compatible 'Pipe' implementation along with all
    utilities and extensions.  The 'Pipe' implementation in "Control.Pipe.Core"
    exists purely as a reference implementation for people who wish to study the
    simpler 'Pipe' type when building their own iteratee libraries.
-}
