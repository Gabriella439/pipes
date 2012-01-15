{-
    Copyright 2012 Gabriel Gonzalez

    This file is part of the Haskell Pipes Library.

    The is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hPDB is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the Haskell Pipes Library.  If not, see
    <http://www.gnu.org/licenses/>.
-}

{-|
    This library only provides a single data type: 'Pipe'.

    'Pipe' is a monad transformer that extends the base monad with the ability
    to 'await' input from or 'yield' output to other 'Pipe's.  'Pipe's resemble
    enumeratees in other libraries because they receive an input stream and
    transform it into a new stream.

    I'll introduce our first 'Pipe', which is a verbose version of the Prelude's
    'take' function:

> take' :: Int -> Pipe a a IO ()
> take' n = do
>     replicateM_ n $ do
>         x <- await
>         yield x
>     lift $ putStrLn "You shall not pass!"

    This 'Pipe' allows the first @n@ values it receives to pass through
    undisturbed, then it outputs a cute message and shuts down.  Shutdown is
    automatic when you reach the end of the monad.  You don't need to send a
    special signal to connected 'Pipe's to let them know you are done handling
    input or generating output.

    Let's dissect the above 'Pipe''s type to learn a bit about how 'Pipe's work:

>      | Input Type | Output Type | Base monad | Return value
> Pipe   a            a             IO           ()

    So @take'@ 'await's input of type @a@ from upstream 'Pipe's and 'yield's
    output of type @a@ to downstream 'Pipe's.  @take'@ uses 'IO' as its base
    monad because it invokes the 'putStrLn' function.  If we remove the call to
    'putStrLn' the compiler infers the following type instead, which is
    polymorphic in the base monad:

> take' :: (Monad m) => Int -> Pipe a a m ()

    'Pipe's are conservative about using the base monad.  In fact, you can only
    invoke the base monad by using the 'lift' function from 'Pipe''s
    'MonadTrans' instance.  If you never use 'lift', your 'Pipe' will translate
    into pure code.

    Now let's create a function that converts a list into a 'Pipe' by
    'yield'ing each element of the list:

> fromList :: (Monad m) => [a] -> Pipe Zero a m ()
> fromList = mapM_ yield

    The 'Zero' in the type signature represents a type with no constructors
    and we use it to block the input end of the 'Pipe' so that it can't request
    any input from an upstream 'Pipe'.  You can think of @fromList@ as a one way
    'Pipe' that can only deliver output, which makes it suitable for the first
    stage in a 'Pipeline'.  I provide a type synonym for this common case:

> type Producer b m r = Pipe Zero b m r

    You can then rewrite the type signature for @fromList@ as:

> fromList :: (Monad m) => [a] -> Producer a m ()

    Note that you don't have to block the input end with the 'Zero' type.  If
    you let the compiler infer the type, you would get:

> fromList :: (Monad m) => [a] -> Pipe b a m ()

    The compiler says that the input could be anything since without any calls
    to 'await' it can't infer the input type.  I only provide the 'Zero' type
    as a convenience so that you can intentionally block 'Pipe' ends.

    'Producer's resemble enumerators in other libraries because they are a data
    source.  'Producer's never use 'await' statements.

    Now let's create a 'Pipe' that prints every value delivered to it and never
    terminates:

> printer :: (Show a) => Pipe a Zero IO b
> printer = forever $ do
>     x <- await
>     lift $ print x

    The 'Zero' in @printer@'s type signature indicates that it never delivers
    output downstream, so it represents the final stage in a 'Pipeline'.  Again,
    I provide a type synonym for this common case:

> type Consumer a m r = Pipe a Zero m r

    So we could instead write @printer@'s type as:

> printer :: (Show a) => Consumer a IO b

    'Consumer's resemble iteratees in other libraries because they are a data
    sink.  'Consumer's never use 'yield' statements.

    What distinguishes 'Pipe's from every other iteratee implementation is that
    they form a 'Category'.  Because of this, you can literally compose 'Pipe's
    into 'Pipeline's.  'Pipe's actually possess two 'Category' instances:

> newtype Lazy   m r a b = Lazy   { unLazy   :: Pipe a b m r }
> newtype Strict m r a b = Strict { unStrict :: Pipe a b m r }
> instance Category (Lazy   m r) where ...
> instance Category (Strict m r) where ...

    The first category composes pipes with 'Lazy' semantics and the second one
    composes 'Pipe's with 'Strict' semantics.  I'll begin by demonstrating
    'Lazy' semantics.

    For example, you can compose the above 'Pipe's with:

> pipeline :: Pipe Zero Zero IO ()
> pipeline :: unLazy $ Lazy printer . Lazy (take' 3) . Lazy (fromList [1..])

    The compiler deduces that the final 'Pipe' must be blocked at both ends,
    meaning it will never 'await' any input and it will never 'yield' any
    output.  This represents a self-contained 'Pipeline' and I provide a type
    synonym for this common case:

> type Pipeline m r = Pipe Zero Zero m r

    Also, I provide convenience operators for composing 'Pipe's without the
    burden of wrapping and unwrapping newtypes.  For example, to compose 'Pipe's
    using 'Lazy' semantics, just use the '<+<' operator:

> p1 <+< p2 = unLazy $ Lazy p1 <<< Lazy p2 -- (<<<) is the same as (.)

    So you can rewrite @pipeline@ as:

> pipeline :: Pipeline IO ()
> pipeline = printer <+< take' 3 <+< fromList [1..]

    Like many other monad transformers, you convert the 'Pipe' monad back to the
    base monad using some sort of \"@run...@\" function.  In this case, it's the
    'runPipe' function:

> runPipe :: (Monad m) => Pipeline m r -> m r

    'runPipe' only works on self-contained 'Pipeline's.  This is the only
    function in the entire library that actually requires the 'Zero' type
    because it must guarantee that its argument 'Pipe' will never try to
    'await' or 'yield'.  You don't need to worry about explicitly giving it
    capped 'Pipe's because self-contained 'Pipe's will automatically have
    polymorphic input and output ends and 'runPipe' will just assume those ends
    are 'Zero'.

    Let's try using 'runPipe':

>>> runPipe pipeline
1
2
3
You shall not pass!

    Fascinating!  Our 'Pipe' terminated even though @printer@ never terminates
    and @fromList@ never terminates when given an infinite list.  To illustrate
    why our 'Pipe' terminated, let's outline the 'Pipe' flow control rules for
    'Lazy' composition:

    * Execution begins at the most downstream 'Pipe' (@printer@ in our example).

    * If a 'Pipe' 'await's input, it blocks and transfers control to the next
      'Pipe' upstream until that 'Pipe' 'yield's back a value.

    * If a 'Pipe' 'yield's output, it restores control to the original
      downstream 'Pipe' that was 'await'ing its input and binds its result to
      the return value of the 'await' command.

    * If a 'Pipe' terminates, it terminates every other 'Pipe' composed with it.

    The last rule follows from laziness.  If a 'Pipe' terminates then every
    downstream 'Pipe' depending on its output cannot proceed, and upstream
    'Pipe's are never evaluated because the terminated 'Pipe' will not request
    values from them any longer.

    So in our previous example, the 'Pipeline' terminated because @take' 3@
    terminated and brought down the entire 'Pipeline' with it.

    'Pipe's promote loose coupling, allowing you to mix and match them
    transparently using composition.  For example, we can define a new
    'Producer' pipe that indefinitely prompts the user for integers:

> prompt :: Producer Int IO a
> prompt = forever $ do
>     lift $ putStrLn "Enter a number: "
>     n <- read <$> lift getLine
>     yield n

    Now we can compose it with any of our previous 'Pipe's:

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

    Pipe composition imposes an important limitation: You can only compose
    'Pipe's that have the same return type.  For example, I could write the
    following function:

> deliver :: (Monad m) => Int -> Consumer a m [a]
> deliver n = replicateM n await

    ... and I might try to compose it with @fromList@:

>>> runPipe $ deliver 3 <+< fromList [1..10] -- wrong!

    ... but this wouldn't type-check, because @fromList@ has a return type of
    @()@ and @deliver@ has a return type of @[Int]@.  'Lazy' composition
    requires that every 'Pipe' has a return value ready in case it terminates
    first.  This was not a conscious design choice, but rather a requirement of
    the 'Category' instance.

    Fortunately, we don't have to rewrite the @fromList@ function because we can
    add a return value using vertical concatenation:

>>> runPipe $ deliver 3 <+< (fromList [1..10] >> return [])
[1,2,3]

    ... although a more idiomatic Haskell version would be:

>>> runPipe $ (Just <$> deliver 3) <+< (fromList [1..10] *> pure Nothing)
Just [1,2,3]

    This forces you to cover all code paths by thinking about what return value
    you would provide if something were to go wrong.  For example, let's say I
    make a mistake and request more input than @fromList@ can deliver:

>>> runPipe $ (Just <$> deliver 99) <+< (fromList [1..10] *> pure Nothing)
Nothing

    The type system saved me by forcing me to handle all possible ways my
    program could terminate.

    Now what if you want to write a 'Pipe' that only reads from its input end
    (i.e. a 'Consumer') and returns a list of every value delivered to it when
    its input 'Pipe' terminates?

> toList :: (Monad m) => Consumer a m [a]
> toList = ???

    You can't write such a 'Pipe' because if its input terminates then it brings
    down @toList@ with it!  This is a good thing because @toList@ as defined
    is not compositional.

    To see why, let's say you somehow got @toList@ to work and the following
    imaginary code sample worked:

>>> runPipe $ toList <+< (fromList [1..5] >> return [])
[1,2,3,4,5]

    @toList@ is defined to return its value when the 'Pipe' immediately upstream
    (@fromList@ in this case) terminates.  This behavior immediately leads to a
    problem.  What if I were to insert an \"identity\" 'Pipe' between
    @toList@ and @fromList@:

> identity = forever $ await >>= yield
> -- This is how id in both categories is actually implemented

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

    This fortunate limitation was not an intentional design choice, but rather
    an inadvertent consequence of enforcing the 'Category' laws when I was
    implementing 'Pipe''s 'Category' instance.  Satisfying the 'Category' laws
    forces code to be compositional.

    Note that a terminated 'Pipe' only brings down 'Pipe's composed with it.  To
    illustrate this, let's use the following example:

> p = do a <+< b
>        c

    @a@, @b@, and @c@ are 'Pipe's, and @c@ shares the same input and output as
    @a <+< b@, otherwise we cannot combine them within the same monad.  In the
    above example, either @a@ or @b@ could terminate and bring down the other
    one since they are composed, but @c@ is guaranteed to continue after
    @a <+< b@ terminates because it is not composed with them.  Conceptually,
    we can think of this as @c@ automatically taking over the 'Pipe''s
    channeling responsibilities when @a <+< b@ can no longer continue.  There
    is no need to \"restart\" the input or output manually as in some other
    iteratee libraries.

    The @pipes@ library, unlike other iteratee libraries, grounds its vertical
    and horizontal concatenation in mathematics by deriving horizontal
    concatenation ('.') from 'Category' instance and vertical concatenation
    ('>>') from its 'Monad' instance.  This makes it easier to reason about
    'Pipe's because you can leverage your intuition about 'Category's and
    'Monad's to understand their behavior.  The only 'Pipe'-specific primitives
    are the 'await' and 'yield' functions.

    'Lazy' composition has one important defect: resource finalization.  Let's
    say we have the file \"test.txt\" with the following contents:

> This is a test.
> Don't panic!
> Calm down, please!

  .. and we wish to lazily read a line at a time from it:

> readFile' :: Handle -> Producer Text IO ()
> readFile' h = do
>     eof <- lift $ hIsEOF h
>     if eof
>       then return ()
>       else do
>           s <- lift $ hGetLine h
>           yield s
>           readFile' h

    We can use our 'Monad' and 'Category' instances to generate a
    resource-efficient version that only reads as many lines as we request:

> read' n = do
>         lift $ putStrLn "Opening file ..."
>         h <- lift $ openFile "test.txt"
>         take' n <+< readFile' h
>         lift $ putStrLn "Closing file ..."
>         lift $ hClose h

    Now compose!

>>> runPipe $ printer <+< read' 2
Opening file ...
"This is a test."
"Don't panic!"
Closing file ...

>>> runPipe $ printer <+< read' 99
Opening file ...
"This is a test."
"Don't panic!"
"Calm down, please!"
Closing file ...

    In the first example, @take' n <+< readFile' h@ terminates because
    @take'@ only requested 2 lines.  In the second example, it terminates
    because @readFile'@ ran out of input.  However, in both cases the 'Pipe'
    never reads more lines than we request frees \"test.txt\" immediately when
    it was no longer needed.

    Even more importantly, the @file@ is never opened if we replace @printer@
    with a 'Pipe' that never demands input:

>>> runPipe $ (lift $ putStrLn "I don't need input") <+< read' 2
I don't need input

    There is still one problem, though. What if we wrote:

>>> runPipe $ printer <+< take' 1 <+< read' 3
Opening file ...
"This is a test."

    Oh no!  Our 'Pipe' didn't properly close our file!  @take' 1@ terminated
    before @read' 3@, preventing @read' 3@ from properly closing \"test.txt\".
    We can force the @read' 3@ 'Pipe' to close the file by using the 'discard'
    function:

> discard :: (Monad m) => Pipe a b m r
> discard = forever await

    If we append 'discard' to @take' 1@, we will drive @read' 3@ to completion
    by continuing to pull values from it:

>>> runPipe $ printer <+< (take' 1 >> discard) <+< read' 3
Opening file ...
"This is a test."
Closing file ...

   This allows @read' 3@ to complete so it can properly finalize itself.  I
   include a convenience operator for this behavior:

> p1 <-< p2 = (p1 >> discard) <+< p2

   Interestingly, '<-<' forms a 'Category', too, namely the 'Strict' category.
   This 'Category' draws down all input by default (as the name suggests).  I
   call it the 'Strict' 'Category' because 'discard' resembles 'seq'.  'discard'
   drives its input to continue until one upstream 'Pipe' terminates and this
   behavior resembles forcing its input to weak head normal form.  If every
   'Pipe' drives its input to weak head normal form, you get 'Strict'
   composition.

   'Strict' composition works terribly on infinite inputs, as you would expect:

>>> runPipe $ printer <-< take' 3 <-< prompt
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
Enter a number:
4<Enter>
5<Enter>
6<Enter>
... <Prompts for input indefinitely and discards it>

    'Strict' composition works best for inputs that are finite and require
    finalization.  'Lazy' composition works best for inputs that are infinite
    (and obviously an infinite input never needs finalization).

    However, unlike conventional strictness in Haskell, 'Strict' 'Pipe's do not
    load the entire input in memory.  They still stream and immediately handle
    input just as 'Lazy' 'Pipe's.  The only difference is that they guarantee
    input finalization (for better or for worse).  Also, for 'Strict'
    'Pipeline's the return value must come from the most upstream 'Pipe'.  Other
    than that, 'Strict' composition will have the exact same sequence of monadic
    effects, resource usage, memory profile, and performance.

    Like Haskell, you can mix 'Lazy' and 'Strict' composition.  Keep in mind,
    though, that while '<+<' is associative with itself and '<-<' is associative
    with itself, mixtures of them are not associative.  Alternatively, you
    could stick to 'Lazy' composition and sprinkle 'discard' statements
    wherever you desire strictness.  It's up to you.  However, when designing
    library functions, make them 'Lazy' by default, since you can make 'Lazy'
    code 'Strict' by adding a 'discard' statement, but you can't make 'Strict'
    code 'Lazy'.
-}

module Control.Pipe (module Control.Pipe.Common) where

import Control.Category
import Control.Monad.Trans
import Control.Pipe.Common
