{-| @pipes@ is a clean and powerful stream processing library that lets you
    build and connect reusable streaming components like Unix pipes.

    You should use @pipes@ if:

    * you are streaming data,

    * you need \"ListT done right\", or

    * you want a reactive programming system.

    This tutorial covers the first two applications, and the @pipes-concurrency@
    package provides a separate tutorial covering reactive programming.
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

    -- * Types - Part 2
    -- $types2

    -- * Theory - Part 2
    -- $theory2

    -- * Folds
    -- $folds

    -- * Types - Part 3
    -- $types3
    ) where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
import Pipes
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

    Compare this to the equivalent hand-written loop:

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

    This gives the same behavior, but this time Haskell pipes do all the
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
    off the 'String' to a downstream stage which decides how to process the
    'String'.  This neatly packages away this messy loop into a self-contained
    and reusable component so that nobody has to write that loop ever again.

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

    You can only run self-contained 'Effect's, using 'runEffect':

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

    The @pipes@ library leverages category theory pervasively in order to
    eliminate large classes of bugs and promote intuitive behavior.  Unlike Unix
    pipes, you will never encounter dark corners of the @pipes@ API that give
    weird behavior because all the primitives are built on a proven mathematical
    foundation.
-}

{- $types1
    You might wonder why these pipes require an argument of type @()@.  This is
    because 'Consumer's, 'Producer's, and 'Pipe's are all special cases of fully
    bidirectional 'Proxy's, and the ('>->') uses this initial argument in the
    general case:

> (>->) :: (b' -> Proxy a' a b' b m r) 
>       -> (c' -> Proxy b' b c' c m r)
>       -> (c' -> Proxy a' a b' b m r)

    'Consumer's, 'Producer's, and 'Pipe's are all type synonyms around the
    'Proxy' type, which is why you can reuse ('>->') to connect all of them:

> type Producer a   m r = forall x' x      . Proxy x' x () a m r
> type Pipe     a b m r =                    Proxy () a () b m r
> type Consumer   b m r = forall      y' y . Proxy () b y' y m r
> type Effect       m r = forall x' x y' y . Proxy x' x y' y m r

    See the advanced section on bidirectionality if you want to learn more.
    Otherwise, just remember that your pipes require an argument of type @()@ if
    you stick to the common case of composing unidirectional pipes using
    ('>->').
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

{- $types2
    ('>->') combines pipes by interleaving their actions in the base monad.
    Therefore, you can only compose two pipes if they share the same base monad.

    For example, you might define a simple parsing pipe that throw errors upon
    failed parses:

> import Control.Monad.Trans.Error
>
> parseInts :: (Monad m) => () -> Pipe String Int (ErrorT String m) r
> parseInts () = forever $ do
>     str <- request ()
>     case reads str of
>         []       -> lift $ throwError "Could not parse an integer"
>         (n, _):_ -> respond n

    However, you will get a type error if you compose it directly with 'P.stdin'
    or 'P.print':

>>> runErrorT $ runEffect $ (P.stdin >-> parseInts >-> P.print) ()
<interactive>:2:38:
    Couldn't match expected type `IO'
                with actual type `ErrorT String m0'
    ...

    The type-checker complains that the base monads don't match: 'P.stdin' and
    'P.print' use 'IO' as their base monad, but @parseInts@ uses 'ErrorT'.

    To unify their base monads we use 'hoist' from the 'MFunctor' class which
    applies transformations to base monads:

> hoist :: (Monad m, MFunctor t) => (m a -> n a) -> t m b -> t n b

    All pipes implement 'MFunctor' so we can 'hoist' the 'lift' function to make
    our pipes agree on using 'ErrorT String IO' for their base monad:

> P.stdin
>     :: () -> Producer String IO ()
>
> hoist lift . P.stdin
>     :: () -> Producer String (ErrorT String IO) ()
>
> P.print
>     :: (Show a) => () -> Consumer a IO ()
>
> hoist lift . P.print
>     :: (Show a) => () -> Consumer a (ErrorT String IO) ()

    Once they agree on the base monad we can compose them directly:

>>> runErrorT $ runEffect $ (hoist lift . P.stdin >-> parseInts >-> hoist lift . P.print) ()
42<Enter>
42
555<Enter>
555
Four<Enter>
Left "Could not parse an integer"

    Note that ('.') has higher precedence than ('>->') so you can use ('.') to
    easily modify pipes while ignoring their initial argument.
-}

{- $theory2
    You don't need to individually 'hoist' several consecutive pipes in a row.
    'hoist' has the nice property that you can factor out any consecutive group
    of 'hoist's into a single call to 'hoist'.

> hoist lift . (p1 >-> p2) = hoist lift . p1 >-> hoist lift . p2
>
> hoist lift . pull = pull

    These two equations are functor laws in disguise!

    Functors transform one category to another such that:

> fmap (f . g) = fmap f . fmap g
>
> fmap id = id

    If you replace 'fmap' with @(hoist lift .)@, replace ('.') with ('>->'), and
    replace 'id' with 'pull', you get the above two equations for 'hoist'.

    The @pipes@ prelude is full of functors like these.  For example, 'P.map'
    transforms the category of functions to the category of pipe composition:

> P.map (f . g) = P.map f >-> P.map g
>
> P.map id = pull

    See if you can spot some other functors in the prelude.
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

> $ ./wc <wc.hs
> Sum {getSum = 9}
> $ ./wc
> How<Enter>
> many<Enter>
> lines<Enter>
> ^D
> Sum {getSum = 3}
> $

    However, Haskell pipes can read and transmit typed values, so let's take
    advantage of that to do some @awk@-like arithmetic.  We'll combine
    'P.readLn' which 'read's typed values from standard input:

> readLn :: (Read b) => () -> Producer b IO ()

    ... and fold those values using 'P.sum':

> sum :: (Monad m, Num a) => () -> Consumer a (WriterT (Sum a) m) r

    Whenever we fold things using 'WriterT' we have to use 'runWriterT' or
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
    You can reason about how @pipes@ behave just by following the types.  In the
    last pipeline we began from 'P.readLn' (which defaults to 'Integer's):

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
