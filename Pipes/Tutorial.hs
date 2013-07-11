{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| @pipes@ is a clean and powerful stream processing library that lets you
    build and connect reusable streaming components like Unix pipes.

    You should use @pipes@ if:

    * you are streaming data,

    * you need to easily build or consume 'ListT' computations,

    * you want a reactive programming system, or

    * you like message passing.

    This tutorial covers the first two applications, and the @pipes-concurrency@
    package provides a separate tutorial covering reactive programming and
    message passing.
-}

module Pipes.Tutorial (
    -- * Easy to use
    -- $easytouse

    -- * Easy to build
    -- $easytobuild

    -- * Easy to understand
    -- $easytounderstand

    -- * Theory - Part 1
    -- $theory1

    -- * Types - Part 1
    -- $types1

    -- * Prelude
    -- $prelude

    -- * Sequence Pipes
    -- $sequence

    -- * Types - Part 2
    -- $types2

    -- * Theory - Part 2
    -- $theory2

    -- * Catch Errors
    -- $catch

    -- * Folds
    -- $folds

    -- * Types - Part 3
    -- $types3

    -- * Run base monads
    -- $run
    ) where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P

{- $easytouse
    Haskell pipes are simple to connect.  For example, here's how you echo
    'P.stdin' to 'P.stdout', just like the Unix @cat@ program when given no
    arguments:

> -- cat.hs
> 
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.stdin >-> P.stdout) ()

    If you compile and run it, it will copy standard input to standard output:

> $ ghc -O2 cat.hs
> $ ./cat
> Echo<Enter>
> Echo
> ABC<Enter>
> ABC
> ^D
> $ ./cat <cat.hs
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.stdin >-> P.stdout) ()
> $

    Compare the @pipes@ program to the equivalent hand-written loop:

> import Control.Monad
> import System.IO
> 
> main = loop
>   where
>     loop = do
>         eof <- hIsEOF stdin
>         unless eof $ do
>             str <- getLine
>             putStrLn str
>             loop

    The hand-written version tightly integrates the input and output logic
    together and is less declarative of our intent.

    When we decouple input and output logic we can easily insert intermediate
    transformation stages.  The next example demonstrates this by inserting a
    'P.take' processing stage to emulate the @head@ utility:

> -- head.hs
> 
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.stdin >-> P.take 10 >-> P.stdout) ()

    Loose coupling also means that we can easily swap out new inputs and
    outputs.  Let's use this trick to simulate the @yes@ command by replacing
    'P.stdin' with an endless list of \"y\"s:

> -- yes.hs
> 
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.fromList (repeat "y") >-> P.stdout) ()

    Normally if we wanted to connect two programs we'd do it in Unix-land using
    Unix pipes:

> $ ./yes | ./head
> y
> y
> y
> y
> y
> y
> y
> y
> y
> y
> $

    ... but with @pipes@ we can keep all the logic in Haskell:

> -- combined.hs
> 
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.fromList (repeat "y") >-> P.take 10 >-> P.stdout) ()

    This gives the same behavior, but this time the Haskell pipes do all the
    information passing:

> $ ./combined
> y
> y
> y
> y
> y
> y
> y
> y
> y
> y
> $

    Haskell pipes promote a compositional approach to stream programming where
    you mix and match modular components to assemble useful programs.
-}

{- $easytobuild
    Haskell pipes are simple to write.  For example, here's the code for
    'P.stdout':

> import Control.Monad
> import Pipes
>
> stdout :: () -> Consumer String IO r
> stdout () = forever $ do
>     str <- request ()
>     lift $ putStrLn str

    We 'request' values of type 'String', so 'P.stdout' is a 'Consumer' of
    'String's.  'Consumer' is a monad transformer that extends the base monad
    with the ability to 'request' new input from upstream.

    'P.stdin' is only slightly more complicated because we have to also check
    for the end of the input:

> import qualified System.IO as IO
>
> stdin :: () -> Producer String IO ()
> stdin () = loop
>   where
>     loop = do
>         eof <- lift $ IO.hIsEOF IO.stdin
>         unless eof $ do
>             str <- lift getLine
>             respond str
>             loop

    We 'respond' with values of type 'String', so 'P.stdin' is a 'Producer' of
    'String's.  'Producer' is a monad transformer that extends the base monad
    with the ability to 'respond' with new output to send downstream.

    Notice how much 'P.stdin' resembles our original hand-written loop, but this
    time we use 'respond'  instead of 'putStrLn'.  The 'respond' command hands
    off the 'String' to an unspecified downstream stage which will decide how to
    process the 'String'.  This decouples the 'String' creation logic from the
    'String' consumption logic so that we can mix and match different sources
    and sinks.

    The last component is 'P.take' which only transmits a fixed number of
    values:

> take :: Int -> () -> Pipe a a IO ()
> take n () = replicateM_ n $ do
>     a <- request ()
>     respond a

    We 'request' values of any type, @a@, and 'respond' with values of the same
    type, @a@, so 'P.take' is a 'Pipe' from @a@ to @a@.  'Pipe' is a monad
    transformer that extends the base monad with the ability to both 'request'
    new input and 'respond' with new output.

    Notice that 'take' can transmit values of any type, not just 'String's.
    Haskell pipes differ from Unix pipes because they can receive and transmit
    typed values and they are not limited to textual input and output.
-}

{- $easytounderstand
    You connect pipes using the ('>->') composition operator.

    If you connect a 'Producer' and a 'Consumer', you get a self-contained
    'Effect':

> (>->) :: (() -> Producer a m r)
>       -> (() -> Consumer a m r)
>       -> (() -> Effect     m r)

    If you connect a 'Producer' and a 'Pipe', you get a 'Producer':

> (>->) :: (() -> Producer a m r)
>       -> (() -> Pipe   a b m r)
>       -> (() -> Producer b m r)

    If you connect a 'Pipe' and a 'Consumer', you get a 'Consumer':

> (>->) :: (() -> Pipe   a b m r)
>       -> (() -> Consumer b m r)
>       -> (() -> Consumer a m r)

    Finally, you can only run self-contained 'Effect's, using 'runEffect':

> runEffect :: Effect m r -> m r

    This improves upon the traditional Unix pipe operator because the compiler
    will guarantee that:

    * we don't connect pipes with mismatched types,

    * we don't connect an open end to a closed end, and

    * we don't leave a dangling input or output end.
-}

{- $theory1
    ('>->') has the very nice property that it is associative: meaning that it
    behaves the exact same way no matter how you group composition:

> -- Associativity
> (p1 >-> p2) >-> p3 = p1 >-> (p2 >-> p3)

    ... so you can always safely omit the parentheses since the meaning is
    unambiguous:

> p1 >-> p2 >-> p3

    This guarantees that you can reason about each pipe's behavior independently
    of other pipes, otherwise composition wouldn't be associative.

    Also, we can prove that pipe composition does not leak any side effects or
    implementation details.  We only need to define an empty test pipe named
    'pull', which auto-forwards all values and never tampers with the stream:

> -- pull's true implementation is more general
> pull :: (Monad m) => () -> Pipe a a m r
> pull () = forever $ do
>     a <- request ()
>     respond a

    We expect that if composition does not leak any information then composing
    'pull' should have no effect:

> -- Left Identity
> pull >-> p = p
>
> -- Right Identity
> p >-> pull = p

    ... and this turns out to be true, guaranteeing that pipe composition is
    invisible.

    Fascinatingly, this means that pipes are a 'Category' in disguise, where
    ('>->') is the composition operator and 'pull' is the identity.  The above
    equations are the 'Category' laws.

    The @pipes@ library uses category theory pervasively to eliminate large
    classes of bugs and promote intuitive behavior.  Unlike Unix pipes, you will
    not encounter dark corners of the @pipes@ API that give weird behavior
    because all the primitives are built on a proven mathematical foundation.
-}

{- $types1
    You might wonder why these pipes require an argument of type @()@.  This is
    because 'Consumer's, 'Producer's, and 'Pipe's are all special cases of fully
    bidirectional 'Proxy's which can send information upstream, too.  The
    ('>->') uses this initial argument in the general case:

> (>->) :: (b' -> Proxy a' a b' b m r) 
>       -> (c' -> Proxy b' b c' c m r)
>       -> (c' -> Proxy a' a b' b m r)

    This is also the same reason that every 'request' so far used an empty @()@
    argument.  In the general case you can provide a non-empty argument to
    send upstream to parametrize the 'request'.

    'Consumer's, 'Producer's, and 'Pipe's are all type synonyms around the
    'Proxy' type, which is why you can reuse ('>->') to connect all of them:

> data C  -- C is uninhabited, and stands for 'C'losed
>
> type Producer a   m r = Proxy C  () () a m r
> type Pipe     a b m r = Proxy () a  () b m r
> type Consumer   b m r = Proxy () b  () C m r
> type Effect       m r = Proxy C  () () C m r

    To learn more about this, you can read the documentation in the "Pipes"
    module, which discusses how these extra type parameters are used to
    implement several advanced streaming features.  Otherwise, just remember
    that your pipes and 'request's require an argument of type @()@ if you stick
    to the common case of composing unidirectional pipes using ('>->').
-}

{- $prelude
    @pipes@ provides a Prelude of utilities in "Pipes.Prelude" that generalize
    their list-based counterparts.

    For example, you can 'P.map' a function to convert it to a 'Pipe':

> P.map :: (Monad m) => (a -> b) -> Pipe a b m r

    You can also 'P.zipWith' two 'Producer's the same way you would 'zipWith'
    lists:

> P.zipWith
>     :: (Monad m)
>     => (a -> b -> c)
>     -> (() -> Producer a m r)
>     -> (() -> Producer b m r)
>     -> (() -> Producer c m r)

    Using these two functions we can implement the @nl@ utility to number all
    lines:

> import Text.Printf
>
> numbers :: (Monad m) => () -> Producer String m ()
> numbers = P.fromList [(1::Int)..] >-> P.map (printf "%6d\t")
> 
> main = runEffect $ (P.zipWith (++) numbers P.stdin >-> P.stdout) ()

    Or you can print all natural numbers by combining 'P.fromList':

> P.fromList :: (Monad m) => [b] -> () -> Producer b m ()

    ... with 'P.print':

> P.print :: (Show a) => () -> Consumer a IO r

>>> runEffect $ (P.fromList [0..] >-> P.print) ()
0
1
2
3
4
...

-}


{- $sequence
    All pipes are special cases of the 'Proxy' type, and the 'Proxy' type is a
    'Monad', therefore you can combine pre-existing pipes by sequencing them
    using @do@ notation.

    For example, we can build a 'Producer' that begins with a pre-defined script
    and then have a human take over if the downstream 'Consumer' exhausts our
    script:

> serve :: () -> Producer String IO ()
> serve () = do
>     -- Lead with the script
>     P.fromList ["Hi, my name is Gabriel.", "How may I help you?"] ()
>     -- Then the human continues
>     P.stdin ()

    Since composable 'Proxy's require an initial argument, we must supply that
    argument if we wish to sequence them.  This is why we apply 'P.fromList' and
    'P.stdin' to their initial @()@ argument in order to call them in the
    'Proxy' monad.

    When we sequence two 'Proxy's, the first 'Proxy' handles as much input or
    output as possible.  So if downstream requests two lines of input then our
    'P.fromList' handles it all and the 'P.stdin' never kicks in, so our human
    is off the hook:

>>> runEffect $ (serve >-> P.take 2 >-> P.stdout) ()
Hi, my name is Gabriel.
How may I help you?
>>>

    However, if downstream exhausts 'P.fromList', then the 'P.stdin' proxy
    takes over and then we must type in the remaining lines:

>>> runEffect $ (serve >-> P.take 3 >-> P.stdout) ()
Hi, my name is Gabriel.
How may I help you?
Give me one second.<Enter>
Give me one second.

    You can sequence 'Pipe's or 'Consumer's, too.  For example, the following
    'Pipe' tries to speed things up a little bit after three messages:

> hurryUp :: (Monad m) => () -> Pipe String String m ()
> hurryUp () = do
>     P.take 3 ()  -- 'take' is a 'Pipe'
>     respond "Will that be all?"
>     pull ()      -- 'pull' is a 'Pipe'

    You can even nest composed 'Proxy's within the 'Proxy' monad.  For example,
    we can refine our original script by shutting down the 'Producer' when the
    human types @\"Bye\"@:

> serve :: () -> Producer String IO ()
> serve () = do
>     P.fromList ["Hi, my name is Gabriel.", "How may I help you?"] ()
>     (P.stdin >-> P.takeWhile (/= "Bye")) ()

    This works because the result of 'Proxy' composition is still a 'Proxy', and
    therefore can be sequenced just like any other 'Proxy' once we apply its
    initial argument.  Let's verify that this actually works:

>>> runEffect $ (serve >-> hurryUp >-> P.stdout) ()
Hi, my name is Gabriel.
How may I help you?
Give me one second.<Enter>
Give me one second.
Will that be all?
Bye<Enter>
>>>

-}

{- $types2
    ('>->') combines pipes by interleaving their actions in the base monad.
    Therefore, you can only compose two pipes if they share the same base monad.

    For example, the 'P.read' pipe fails in 'ErrorT' if it cannot parse a value:

> read :: (Monad m, Read a) => () -> Pipe String a (ErrorT String m) r

    However, if we try to compose 'P.read' with 'P.stdin':

> readInt = P.stdin >-> P.read

    ... then we will get a type error:

>    Couldn't match expected type `IO'
>                with actual type `ErrorT String m0'
>    ...

    The type-checker complains that the base monads don't match: 'P.stdin' needs
    'IO' as the base monad, but 'P.read' needs @(ErrorT String m)@ as the base
    monad.

    To unify their base monads we use 'hoist' from the 'MFunctor' class which
    applies transformations to base monads:

> hoist :: (Monad m, MFunctor t) => (m a -> n a) -> t m b -> t n b

    All pipes implement 'MFunctor' so we can 'hoist' the 'lift' function to make
    our pipes agree on using @(ErrorT String IO)@ for their base monad:

> P.stdin
>     :: () -> Producer String IO ()
>
> hoist lift . P.stdin
>     :: () -> Producer String (ErrorT String IO) ()

    Once they agree on the base monad we can compose them directly:

> readInt :: () -> Producer Int (ErrorT String IO) ()
> readInt = hoist lift . P.stdin >-> P.read

    Now we can validate that all input lines are 'Int's before 'print'ing them:

>>> -- Remember, we need to hoist P.print, too!
>>> runErrorT $ runEffect $ (readInt >-> hoist lift . P.print) ()
42<Enter>
42
555<Enter>
555
Four<Enter>
Left "Pipes.Prelude.read: no parse"

    Note that ('.') has higher precedence than ('>->') so you can use ('.') to
    easily modify pipes in the middle of a composition chain without using
    parentheses.
-}

{- $theory2
    You don't need to individually 'hoist' several consecutive pipes in a row.
    If you have the following pattern:

> hoist lift . p1 >-> hoist lift . p2

    ... you can instead condense these into a single call to 'hoist':

> hoist lift . (p1 >-> p2)

    In fact, 'hoist' has the nice property that you can factor or distribute
    'hoist' over the category of pipe composition:

> hoist k . (p1 >-> p2) = hoist k . p1 >-> hoist k . p2
>
> hoist k . pull = pull

    Interestingly, these two equations are functor laws in disguise!

    To see how, remember that functors transform one category to another such
    that:

> fmap (f . g) = fmap f . fmap g
>
> fmap id = id

    If you replace 'fmap' with @(hoist k .)@, replace ('.') with ('>->'), and
    replace 'id' with 'pull', you get the above functor laws for 'hoist'.

    The @pipes@ prelude is full of functions that define their behavior in terms
    of functor laws.  For example, 'P.map' transforms the category of functions
    to the category of pipe composition:

> P.map (f . g) = P.map f >-> P.map g
>
> P.map id = pull

    Functor laws like these let you easily reason about the behavior of @pipes@
    utilities.  See if you can spot other functor laws in "Pipes.Prelude".
-}

{- $catch
    Use 'PL.catchError' from "Pipes.Lift" if you want to catch any errors raised
    in 'ErrorT'.  Here's an example program that recovers from 'P.read' errors
    by printing the error and retrying the read:

> -- catch.hs
> 
> import Control.Monad.Trans.Error
> import Pipes
> import qualified Pipes.Lift as PL
> import qualified Pipes.Prelude as P
> 
> keepReading :: () -> Producer Int (ErrorT String IO) ()
> keepReading () = loop
>   where
>     loop =
>         (hoist lift . P.stdin >-> P.read) ()
>       `PL.catchError` (\e -> do
>         lift $ lift $ putStrLn e
>         loop )
> 
> main = runErrorT $ runEffect $ (keepReading >-> hoist lift . P.print) ()

    This prints the error to the console and continues reading if the user input
    does not parse to an 'Int':

> $ ./catch
> 134<Enter>
> 134
> Test
> Pipes.Prelude.read: no parse
> 79<Enter>
> 79
> ^D
> $

-}

{- $folds
    The @pipes@ Prelude provides several folds which store their results in a
    'WriterT' layer in the base monad.  For example, you can count the number of
    lines of input like the @wc@ command if you use the 'P.length' fold:

> P.length :: (Monad m) => () -> Consumer a (WriterT (Sum Int) m) r

    Just don't forget to use 'hoist' since the base monads don't match:

> -- wc.hs
>
> import Pipes
> import qualified Pipes.Prelude as P
> import Control.Monad.Trans.Writer.Strict
>
> main = do
>     numLines <- execWriterT $ runEffect $ (hoist lift . P.stdin >-> P.length) ()
>     print numLines

    Let's try it:

> $ ./wc < wc.hs
> Sum {getSum = 9}
> $ ./wc
> How<Enter>
> many<Enter>
> lines?<Enter>
> ^D
> Sum {getSum = 3}
> $

    However, Haskell pipes can read and transmit typed values, so let's take
    advantage of that to do some @awk@-like arithmetic.  We'll combine
    'P.readLn' which 'read's typed values from standard input:

> -- Like our 'readInt', except throws exceptions on failed parses
> P.readLn :: (Read b) => () -> Producer b IO ()

    ... and fold those values using 'P.sum':

> P.sum :: (Monad m, Num a) => () -> Consumer a (WriterT (Sum a) m) r

    Whenever we fold things using 'WriterT' we can use 'runWriterT' or
    'execWriterT' to retrieve the result of the fold:

> -- sum.hs
>
> import Control.Monad.Trans.Writer.Strict
> import Pipes
> import qualified Pipes.Prelude as P
>
> main = do
>     total <- execWriterT $ runEffect $ (hoist lift . P.readLn >-> P.sum) ()
>     print total
-}

{- $types3
    You can reason about how @pipes@ behave by following the types.  In the last
    pipeline we began from 'P.readLn' (which defaulted to 'Integer's):

> P.readLn
>     :: () -> Producer Integer IO ()

    ... and 'hoist'ed a 'lift' so that its base monad matches 'P.sum':

> hoist lift . P.readLn
>     :: (MonadTrans t)
>     => () -> Producer Integer (t                     IO) ()
>
> P.sum
>     :: (Monad m)
>     => () -> Consumer Integer (WriterT (Sum Integer) m ) ()

    The \'@t@\' will type-check as @WriterT (Sum Integer@) and the \'@m@\' will
    type-check as 'IO', so the two base monads match.

    'P.readLn' is a 'Producer' and 'P.sum' is a 'Consumer', so when we compose
    them we get an 'Effect':

> hoist lift . P.readLn >-> P.sum
>     :: () -> Effect (WriterT (Sum Integer) IO) ()

    We can't run the 'Effect' until we apply the pipeline to @()@:

> (hoist lift . P.readLn >-> P.sum) ()
>     :: Effect (WriterT (Sum Integer) IO) ()

    ... and then we retrieve the action in the base monad using 'runEffect':

> runEffect $ (hoist lift . P.readLn >-> P.sum) ()
>     :: WriterT (Sum Integer) IO ()

    This is the right type for 'execWriterT', which runs the fold:

> execWriterT $ runEffect $ (hoist lift . P.readLn >-> P.sum) ()
>     :: IO (Sum Integer)

    Now we've built an 'IO' action that folds user input into a 'Sum'.
-}

{- $run
    The above folds will not run in constant space because both 'WriterT'
    implementations in @transformers@ are not sufficiently strict.  Fortunately,
    you can use 'execWriterP' to work around this, which has the following type
    signature:

> execWriterP
>     :: (Monad m, Monoid w)
>     => Proxy a' a b' b (WriterT w m) r -> Proxy a' a b' b m w

    'runWriterP' and 'execWriterP' let you unwrap 'WriterT' layers in the base
    monad without having to unwrap the 'Proxy' layer.  As a bonus, they both
    keeps the 'WriterT' accumulator strict, so they serve a dual purpose.  You
    will see the difference if you try to use 'execWriterP' versus
    'execWriterT' for large folds:

> import Control.Monad.Trans.Writer.Strict
> import Pipes
> import Pipes.Lift
> import qualified Pipes.Prelude as P
> 
> main = do
>     -- This version overflows
>     -- total <- execWriterT $ runEffect $
>     --      (P.fromList [(1::Int)..10000000] >-> P.sum) ()
> 
>     -- This version runs in constant space
>     total <- runEffect $ execWriterP $
>         (P.fromList [(1::Int)..10000000] >-> P.sum) ()
>     print total

    "Pipes.Lift" provides several functions like these which let you unwrap
    monad transformers in the base monad without unwrapping the 'Proxy' type:

> runErrorP
>     :: (Monad m)
>     => Proxy a' a b' b (ErrorT e m) r -> Proxy a' a b' b m (Either e r)
>
> runReaderP
>     :: (Monad m)
>     => i -> Proxy a' a b' b (ReaderT i m) r -> Proxy a' a b' b m r
>
> runStateP
>     :: (Monad m)
>     => s -> Proxy a' a b' b (StateT s m) r -> Proxy a' a b' b m (r, s)

    This makes it easy to extend pipes by just stashing all the desired
    functionality in the base monad.  Then you can unwrap the monad transformers
    when you are done using them, without having to leave the 'Proxy' monad.
-}
