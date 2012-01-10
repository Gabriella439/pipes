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
    'Pipe' is a monad transformer that extends the base monad with the ability
    to 'await' input from or 'yield' output to other 'Pipe's.  'Pipe's resemble
    enumeratees in other libraries because they receive an input stream and
    transform it into a new stream.

    I'll introduce our first 'Pipe', which is a verbose version of the Prelude's
    'take' function:

> take' :: Int -> Pipe a a IO ()
> take' n = do
>     replicateM_ n $ do
>         x <- await -- request input from the upstream pipe and bind it to x
>         yield x    -- deliver x as output to the next downstream pipe
>     lift $ putStrLn "You shall not pass!"

    This 'Pipe' allows the first @n@ values it receives to pass through
    undisturbed, then it outputs a cute message and shuts down.  Shutdown is
    automatic when you reach the end of the monad.  There is no special signal
    you need to send to connected 'Pipe's to let them know you are done
    handling input or generating output.

    Let's dissect the above 'Pipe''s type to learn a bit about how 'Pipe's work:

>      | Input Type | Output Type | Base monad | Return value
> Pipe   a            a             IO           ()

    @take'@ uses 'IO' as its base monad because it invokes the 'putStrLn'
    function.  Had it not used any functions from the base monad, the compiler
    would have inferred the following type signature:

> take' :: (Monad m) => Int -> Pipe a a m ()

    'Pipe's are conservative about invoking the base monad.  In fact, the only
    way to do it is to use the 'lift' function from the 'MonadTrans' instance.
    This means that 'runPipe' will translate any 'Pipeline' that doesn't call
    'lift' into pure (i.e. non-monadic) code.

    Like all monad transformers, 'Pipe' indicates the base monad using the
    second-to-last variable: @m@.  The two variables preceding the @m@
    correspond to the types of the pipe's input and output, in that order.
    Our @take'@ 'Pipe' requests input values of type @a@ and since it just
    outputs them without changing them, the output type matches the input type:
    @a@.

    Now let's create a function that transforms a list into a 'Pipe' by
    'yield'ing each element of the list:

> fromList :: (Monad m) => [a] -> Pipe Zero a m ()
> fromList = mapM_ yield

    The 'Zero' in the type signature represents a type with no constructors
    and we use it to block the input end of the 'Pipe' so that it can't request
    any input from an upstream 'Pipe'.  You can think of @fromList@ as a one way
    'Pipe' that can only deliver output, which makes it suitable for the first
    stage in a 'Pipeline'.  I provide a type synonym for this common case:

> type Producer b m r = Pipe Zero b m r

    It's not much shorter, but perhaps its easier on your eyes.  You can then
    rewrite the type signature for @fromList@ as:

> fromList :: (Monad m) => [a] -> Producer a m ()

    Note that you don't have to block the input end with the 'Zero' type.  If
    you let the compiler infer the type, you would get:

> fromList :: (Monad m) => [a] -> Pipe b a m ()

    This tells us the compiler can't infer the input type because the @fromList@
    pipe never calls the 'await' function.  However, if you try to hook up
    another 'Pipe' to deliver values to the polymorphic input end of @fromList@,
    it will block until @fromList@ terminates because @fromList@ never handles
    any input values delivered to it by 'Pipe's.  The 'Zero' type is provided as
    a convenience so that you can block 'Pipe' ends and specify that it's
    unproductive to connect to that end.

    'Producer's resemble enumerators in other libraries because they only
    generate values.  The source of their values might be a pure value you
    provide them (i.e. the list we passed to @fromList@) or a query in the base
    monad, but it won't be derived from another 'Pipe', since 'Producer's never
    receive values using 'await' statements.

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

    So we can write @printer@'s type as:

> printer :: (Show a) => Consumer a IO b

    'Consumer's resemble iteratees in other libraries because they are a data
    sink.  'Consumer's will never use 'yield' statements.

    'Pipe's can be composed (literally) into pipelines because 'Pipe' has two
    newtypes with separate 'Category' implementations:

> newtype Lazy   m r a b = Lazy   { unLazy   :: Pipe a b m r }
> newtype Strict m r a b = Strict { unStrict :: Pipe a b m r }
> instance Category (Lazy   m r) where ...
> instance Category (Strict m r) where ...

    The first category composes pipes with 'Lazy' semantics and the second one
    with 'Strict' semantics.  For more details, see below, but you probably want
    'Lazy' semantics.

    For example, you can compose the above 'Pipe's with:

> pipeline :: Pipe Zero Zero IO ()
> pipeline :: unLazy $ Lazy printer . Lazy (take 3) . Lazy (fromList [1..])

    The compiler deduce's the final 'Pipe's type to have both ends blocked,
    which means it will never 'await' any input and it will never 'yield' any
    output, therefore it forms a self-contained 'Pipeline'.  I provide a type
    synonym for this common case:

> type Pipeline m r = Pipe Zero Zero m r

    Also, I provide convenience operators for composing 'Pipe's without the
    burden of wrapping and unwrapping newtypes.  For example, to compose 'Pipe's
    lazily, just use the '<+<' operator:

> p1 <+< p2 = unLazy $ Lazy p1 <<< Lazy p2 -- (<<<) is the same as (.)

    So you can rewrite @pipeline@ as:

> pipeline :: Pipeline IO ()
> pipeline = printer <+< take 3 <+< fromList [1..]

    Like many other monad transformers, you convert the 'Pipe' monad back to the
    base monad some sort of \"@run...@\" function.  In this case, it's the
    'runPipe' function:

>>> runPipe pipeline :: IO ()
1
2
3
You shall not pass!

    'runPipe' only works on self-contained 'Pipeline's.  If a 'Pipe' is not
    self-contained, then it is not ready to be converted back to the base monad.
    'runPipe''s type signature says it all:

> runPipe :: (Monad m) => Pipeline m r -> m r

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

>>> let print3 = printer <+< take' 3 :: (Show a) => Consumer a IO ()
>>> runPipe $ (print3 >> print3) <+< fromList [1..]
1
2
3
You shall not pass!
4
5
6
You shall not pass!

   ... but the above example is gratuitous because we could have just
   concatenated the intermediate @take'@ 'Pipe':

>>> runPipe $ printer <+< (take' 3 >> take' 3) <+< fromList [1..]
1
2
3
You shall not pass!
4
5
6
You shall not pass!

    'Pipe's promote loose coupling, allowing you to mix and match them
    transparently using composition.  For example, we can define a new
    'Producer' pipe that indefinitely prompts the user for integers:

> prompt :: Producer Int IO a
> prompt = forever $ do
>     lift $ putStrLn "Enter a number: "
>     n <- read <$> lift getLine
>     yield n

    Now we can compose it with any compatible 'Pipe':

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

    Note that both @printer@ and @prompt@ are non-terminating pipes, yet our
    entire pipeline magically terminates gracefully after @take' 3@ stops.
    Under 'Lazy' composition, 'Pipe's terminate when they 'await' input from a
    terminated 'Pipe') and termination propagates upstream.  So @printer@
    terminated and shut down the entire 'Pipeline' because it 'await'ed a value
    from the terminated @(take' 3)@ 'Pipe'.

    Under 'Strict' composition, 'Pipe's terminate when they 'yield' a value to
    a terminated 'Pipe' and termination propagates downstream.  Let's repeat the
    above example with 'Strict' composition (which uses the '<-<' operator):

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
    the first value until after the user had already entered several.  This
    marks another difference between 'Strict' and 'Lazy' 'Pipeline's.  'Lazy'
    'Pipeline's prioritize downstream 'Pipe's, trying to generate as much output
    as possible before being forced to draw more input.  'Strict' 'Pipeline's
    prioritize upstream 'Pipe's, drawing as much input as possible before being
    forced to generate output.  In the above example, the only reason it even
    printed values at all is because the downstream 'Pipe's had to process some
    values in order to make room for new output from @prompt@.  This is why I
    recommend using 'Lazy' composition unless your goal is to draw as much input
    as possible.

    So far I haven't discussed the 'Pipe' monad's return type and all the above
    examples don't use the return type so I dodged the issue.  The truth is that
    there is nothing to dodge: the return type works just fine.   It's just
    poorly suited for communicating with other pipes.  I'll just list several
    disadvantages of using 'return' to communicate with other 'Pipe's:

    * 'return' forces tight coupling between 'Pipe's whereas 'yield' promotes
      loose coupling

    * 'return' can't communicate anything until the 'Pipe' terminates, whereas
      'yield' will deliver output immediately.  'return' won't even work for
      non-terminating 'Pipe's.

    * A 'Pipe''s return value can only be bound within a 'Pipe' of the same
      type, leading to very brittle and non-compositional code.

    Return values do serve a useful purpose, though, which is that when you run
    a 'Pipe', it will return a value in the base monad.  For 'Lazy' semantics
    this will correspond to the return value of the most downstream 'Pipe' that
    terminated of its own accord.  For 'Strict' semantics, it will correspond to
    the return value of the most upstream 'Pipe' that terminated of its own
    accord.  So while return values are useless for communicating values between
    'Pipe's they are indispensable for communicating results back to the base
    monad upon running the 'Pipe'.

    There is one caveat to using return types: You can only compose 'Pipe's that
    have the same return type.  For example, I could write the following
    function:

> deliver :: (Monad m) => Int -> Consumer a m [a]
> deliver n = replicateM n await

    ... and I might try to compose it with @fromList@:

>>> runPipe $ deliver 3 <+< fromList [1..10] -- wrong!

    ... but this wouldn't type-check, because @fromList@ has a return type of
    @()@ and @deliver@ has a return type of @[Int]@.  Composition requires that
    they match because the return value can potentially come from any 'Pipe' in
    the 'Pipeline', so every 'Pipe' in the chain has to have a return value
    ready just in case its value is used.  Fortunately, we don't have to rewrite
    the @fromList@ function because we can use vertical concatenation
    to add a return value to it:

>>> runPipe $ deliver 3 <+< (fromList [1..10] >> return [])
[1,2,3]

    ... although a more idiomatic Haskell solution would be:

>>> runPipe $ (Just <$> deliver 3) <+< (fromList [1..10] >> return Nothing)
Just [1,2,3]

    When would the return value of @fromList@ ever be used?  Under 'Lazy'
    composition, its return value would get chosen if it terminated before
    @deliver@ was done 'await'ing input from it.  For example, let's say I make
    a mistake and request more input than @fromList@ can deliver:

>>> runPipe $ (Just <$> deliver 99) <+< (fromList [1..10] >> return Nothing)
Nothing

    It does the right thing!

    Now what if you want to write a 'Pipe' that only reads from its input end
    (i.e. a Consumer) and returns a list of every value delivered to it when its
    input 'Pipe' terminates?

> toList :: (Monad m) => Consumer a m [a]
> toList = ???

    You can't write such a 'Pipe' because it is not compositional!  There is no
    control primitive to monitor if the 'Pipe's input has terminated.  The
    library has wait and 'yield'.  This was not an intentional design choice,
    but rather an inadvertent consequence of making 'Pipe' a 'Category'
    instance.  Satisfying the 'Category' laws renders it impossible to design a
    library that supports this kind of termination-monitoring behavior, which is
    provably non-compositional.

    We can show that @toList@'s defined behavior is not compositional without
    even knowing how it is implemented.  Let's say you somehow got @toList@ to
    work and the following imaginary code worked:

>>> runPipe $ toList <+< (fromList [1..5] >> return [])
[1,2,3,4,5]

    For @toList@ to work, it must return its value when the 'Pipe' immediately 
    upstream (@fromList@ in this case) terminates.  This behavior immediately
    leads to a problem.  What if I were to insert the identity 'Pipe' between
    @toList@ and its original source:

> -- This is the actual implementation for id in both Category instances
> id = Lazy $ forever $ await >>= yield 

    The identity 'Pipe' never terminates, so if I were to insert it between
    @toList@ and @fromList@, @toList@ would no longer work:

>>> runPipe $ toList <+< unLazy id <+< (fromList [1..5] >> return [])
??? -- we can't really say because this is an imaginary example

    The answer certainly wouldn't be @[1,2,3,4,5]@ because @toList@ would never
    terminate as long as it was coupled to the non-terminating 'id', despite the
    fact that it would be receiving the exact same output stream from 'id'.
    This is what I mean when I say that @toList@'s specified behavior is
    non-compositional.  It only works if it is coupled directly to the desired
    'Pipe' and breaks when you introduce intermediate stages.

    I implemented the 'Category' laws only because I thought it would be a
    convenient instance, but the process of doing so forced me to create a
    library that enforces modularity and compositionality.  So when you write
    a 'Pipe', you can be sure that it will:

    * seamlessly compose with other 'Pipe's with no additional code (just
      compose them and you are done!)

    * allow arbitrary insertion of intermediate 'Pipe's as long as types match

    * handle mixtures of terminating/non-terminating 'Pipe's gracefully
 -}
module Control.Pipe (module Control.Pipe.Common) where

import Control.Pipe.Common
