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
    they form a 'Category'.  Because of this, you can compose 'Pipe's
    (literally) into 'Pipelines'.  'Pipe's actually possess two 'Category'
    instances:

> newtype Lazy   m r a b = Lazy   { unLazy   :: Pipe a b m r }
> newtype Strict m r a b = Strict { unStrict :: Pipe a b m r }
> instance Category (Lazy   m r) where ...
> instance Category (Strict m r) where ...

    The first category composes pipes with 'Lazy' semantics and the second one
    composes 'Pipe's with 'Strict' semantics.  You usually want 'Lazy'
    semantics.

    For example, you can compose the above 'Pipe's with:

> pipeline :: Pipe Zero Zero IO ()
> pipeline :: unLazy $ Lazy printer . Lazy (take 3) . Lazy (fromList [1..])

    The compiler deduces from composition that the final 'Pipe' must be blocked
    at both ends, meaning it will never 'await' any input and it will never
    'yield' any output.  This represents a self-contained 'Pipeline' and I
    provide a type synonym for this common case:

> type Pipeline m r = Pipe Zero Zero m r

    Also, I provide convenience operators for composing 'Pipe's without the
    burden of wrapping and unwrapping newtypes.  For example, to compose 'Pipe's
    using 'Lazy' semantics, just use the '<+<' operator:

> p1 <+< p2 = unLazy $ Lazy p1 <<< Lazy p2 -- (<<<) is the same as (.)

    So you can rewrite @pipeline@ as:

> pipeline :: Pipeline IO ()
> pipeline = printer <+< take 3 <+< fromList [1..]

    Like many other monad transformers, you convert the 'Pipe' monad back to the
    base monad some sort of \"@run...@\" function.  In this case, it's the
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
    why our 'Pipe' terminated, I'll discuss how 'Pipe' control flow works,
    which is pretty simple.

    A 'Pipe' does one of four things:

    * It calls 'lift' to invoke an action in the base monad

    * It 'await's a value

    * It 'yield's a value

    * It terminates (i.e. reaches the end of its monad)

    Both 'Lazy' and 'Strict' composition have two things in common:

    * When a 'Pipe' calls @lift x@, it executes @x@ in the base monad

    * When any 'Pipe' terminates, every 'Pipe' composed with it also terminates.

    So when @take'@ terminated, it brought down the entire 'Pipeline'.

    Under 'Lazy' composition:

    * Control begins at the most downstream 'Pipe'.

    * When a 'Pipe' calls 'await', it blocks and transfers control to the next
      'Pipe' upstream.

    * If the upstream 'Pipe' 'yield's a value, that value is bound to the return
      value of 'await' and control is restored to the original downstream
      'Pipe'.

I'll bastardize Haskell's layout rules to illustrate:

> --    *******Downstream Pipe******       ********Upstream Pipe*******
> p1 = (do lift $ putStrLn "Await x") <+< (do lift $ putStrLn "Yield 5")
>      (   x <- await               )     (   yield 5                  )
>      (   lift $ print x           )

>>> runPipe p1
Await x
Yield 5
5

    Under 'Strict' composition:

    * Control begins at the most upstream 'Pipe'.

    * When a 'Pipe' calls 'yield', it blocks and transfers control to the next
      'Pipe' downstream.

    * If the downstream 'Pipe' calls 'await', the original 'yield'ed
      value is bound to the return value of 'await' and control is restored to
      the original upstream 'Pipe'.  Yes, that means that it returns control
      before it even uses the value.

    Let's run the 'Strict' version of the above example:

> p1 <-< p2 = unStrict $ Strict p1 <<< Strict p2
>
> --    *******Downstream Pipe******       ********Upstream Pipe*******
> p2 = (do lift $ putStrLn "Await x") <-< (do lift $ putStrLn "Yield 5")
>      (   x <- await               )     (   yield 5                  )
>      (   lift $ print x           )

>>> runPipe p2
Yield 5
Await x

    Control never reaches the @print x@ statement because once the downstream
    'Pipe' calls 'await' it restores control to the upstream 'Pipe'.  However,
    we could restore control to the downstream 'Pipe' by 'yield'ing another
    value:

> --    *******Downstream Pipe******       ********Upstream Pipe*******
> p3 = (do lift $ putStrLn "Await x") <-< (do lift $ putStrLn "Yield 5")
>      (   x <- await               )     (   yield 5                  )
>      (   lift $ print x           )     (   yield 3                  )

>>> runPipe p2
Yield 5
Await x
5

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

    Let's run the 'Strict' version and see what happens!

>>> runPipe $ printer <-< take' 3 <-< prompt
Enter a number:
1<Enter>
Enter a number:
2<Enter>
Enter a number:
3<Enter>
1
Enter a number:
4<Enter>
2
You shall not pass!

    Wow!  Big difference!  The 'Strict' 'Pipeline' didn't even bother to print
    the first value until after the user had already entered three.  This
    illustrates the important distinction between 'Lazy' and 'Strict'
    'Pipeline's:

    * 'Lazy' 'Pipeline's are \"pull-based\" and will generate as much output as
      possible while drawing as little input as possible.

    * 'Strict' 'Pipeline's are \"push-based\" and will will draw as much input
      as possible while generating as little output as possible.

    In the above example, the only reason our 'Pipeline' even printed values at
    all is because @printer@ and @take'@ had to process some values in order to
    make room for new output from @prompt@.

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

    Here's an example of concatenating 'Consumer's:

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

    There is one caveat: You can only compose 'Pipe's that have the same return
    type.  For example, I could write the following function:

> deliver :: (Monad m) => Int -> Consumer a m [a]
> deliver n = replicateM n await

    ... and I might try to compose it with @fromList@:

>>> runPipe $ deliver 3 <+< fromList [1..10] -- wrong!

    ... but this wouldn't type-check, because @fromList@ has a return type of
    @()@ and @deliver@ has a return type of @[Int]@.  Composition requires that
    every 'Pipe' has a return value ready in case it terminates first.
    This was not a conscious design choice, but rather a requirement of the
    'Category' laws.

    Fortunately, we don't have to rewrite the @fromList@ function because we can
    use vertical concatenation to add a return value to it:

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

    The 'Category' instance just saved me from programming a run-time failure by
    forcing me to handle all possible ways my program could terminate.

    Now what if you want to write a 'Pipe' that only reads from its input end
    (i.e. a Consumer) and returns a list of every value delivered to it when its
    input 'Pipe' terminates?

> toList :: (Monad m) => Consumer a m [a]
> toList = ???

    You can't write such a 'Pipe' because it is not compositional!  More
    specifically, @toList@ cannot monitor its input and return a value upon
    input termination because if the input terminates then the input provides
    the return value, not @toList@, and the input brings down @toList@ with it
    when it terminates.  This is a good thing because if @toList@ could find a
    way to work around this limitation, it wouldn't be compositional.

    To see why, let's say you somehow got @toList@ to work and the following
    imaginary code sample worked:

>>> runPipe $ toList <+< (fromList [1..5] >> return [])
[1,2,3,4,5]

    For @toList@ to work, it must return its value when the 'Pipe' immediately 
    upstream (@fromList@ in this case) terminates.  This behavior immediately
    leads to a problem.  What if I were to insert an \"identity\" 'Pipe' between
    @toList@ and @fromList@:

> identity = forever $ await >>= yield
> -- This is how id in both categories is implemented

    This 'Pipe' forwards every valued untouched, so we would expect it to not
    have any affect if we were to insert it in the middle:

>>> runPipe $ toList <+< identity <+< (fromList [1..5] >> return [])
??? -- Something other than [1,2,3,4,5], perhaps even non-termination

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

    You might think that this will lead to resource management problems because
    of never releasing resources upon termination, but you'd be wrong.  To
    illustrate, let's use the following example:

> p = do a <+< b
>        c

    @a@, @b@, and @c@ are 'Pipe's, and @c@ shares the same input and output as
    @a <+< b@, otherwise we cannot combine them within the same 'Monad'.  In the
    above example, either @a@ or @b@ could terminate and bring down the other
    one since they are composed, but @c@ is guaranteed to continue after
    @a <+< b@ terminates because it is not composed with them.

    Because 'Pipe' is a 'Monad' and a 'Category', it covers any possible
    behavior you could desire while still forcing you to write robust and
    composable code.  Let's use a resource management example.  Say we have the
    file \"test.txt\" with the following contents:

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
>           readFile' 

    We can use our 'Monad' and 'Category' instances to generate a
    resource-efficient 'Pipeline':

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
    frees \"test.txt\" immediately when it was no longer needed.

    Even more importantly, the @file@ is never opened if we replace @printer@
    with a 'Pipe' that never demands input:

>>> runPipe $ (lift $ putStrLn "I don't need input") <+< read' 2
I don't need input

    We say that the file is opened \"on-demand\".  There is still one problem,
    though. What if we wrote:

>>> runPipe $ printer <+< take' 1 <+< read' 3
Opening file ...
"This is a test."

    Oh no!  Our 'Pipe' didn't properly close our file!  @take' 1@ terminated
    before @read' 3@, preventing @read' 3@ from properly closing \"test.txt\".
    So while you can efficiently and lazily allocate resources using this
    library, it lacks modularity and composability in this respect.  This
    remains the only inelegant aspect of this library's implementation.
-}

module Control.Pipe (module Control.Pipe.Common) where

import Control.Pipe.Common
