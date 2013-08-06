{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| Conventional Haskell stream programming forces you to pick only two of the
    following three features:

    * Effects

    * Streaming

    * Composability

    If you sacrifice /Effects/ you get Haskell's pure and lazy lists, which you
    can transform using composable functions in constant space, but without
    interleaving effects.

    If you sacrifice /Streaming/ you get 'mapM', 'forM' and \"ListT done wrong\"
    (from @transformers@), which are composable and effectful, but do not return
    a single result until the whole list has first been processed and loaded
    into memory.

    If you sacrifice /Composability/ you write a tightly coupled read,
    transform, and write loop in 'IO', which is streaming and effectful, but is
    not modular or separable.

    @pipes@ gives you all three features: effectful, streaming, and composable
    programming.  @pipes@ also provides a wide variety of stream programming
    abstractions which are all subsets of a single unified machinery:

    * effectful 'Producer's (like generators),

    * effectful 'Consumer's (like iteratees),

    * effectful 'Pipe's (like Unix pipes), and:

    * 'ListT' done right.

    All of these are connectable and you can combine them together in clever and
    unexpected ways because they all share the same underlying type.

    @pipes@ requires a basic understanding of monad transformers, which you can
    learn about by searching for \"Monad Transformers - Step by Step\".  This is
    a beginner-friendly and accessible paper that teaches basic monad
    transformer usage.  Pay careful attention to section 2.5, describing the
    use of 'lift'.  After that, study the documentation for the
    @Control.Monad.Trans.Class@ module from the @transformers@ library.

    If you want a Quick Start guide, read the documentation in "Pipes.Prelude"
    from top to bottom.

    This tutorial is more extensive and explains the @pipes@ API in greater
    detail and illustrates several idioms.  You can follow along by using the
    complete code examples in the Appendix section at the bottom of this
    tutorial.
-}

module Pipes.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Producers
    -- $producers

    -- * Composability
    -- $composability

    -- * Consumers
    -- $consumers

    -- * Pipes
    -- $pipes

    -- * Appendix
    -- $appendix
    ) where

import Control.Category
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
import Prelude hiding ((.), id)

{- $introduction
    The @pipes@ library decouples stream processing stages from each other so
    that you can mix and match diverse stages to produce useful streaming
    programs.  If you are a library writer, @pipes@ lets you package up
    streaming components into a reusable interface.  If you are an application
    writer, @pipes@ lets you connect pre-made streaming components with minimal
    effort to produce a working program that streams data highly efficiently in
    constant memory.

    To enforce loose coupling, components can only communicate with each other
    using two commands:

    * 'yield': Produce values

    * 'await': Consume values

    The four central types correspond to the four permutations in which you can
    enable or disable these two commands:

    * 'Producer's can only 'yield' values and model streaming sources

    * 'Consumer's can only 'await' values and model streaming sinks

    * 'Pipe's can both 'await' and 'yield' values and model stream
      transformations

    * 'Effect's can neither 'yield' nor 'await' and model non-streaming
      components

    You can connect these components together in four separate ways which
    closely parallel the four types:

    * 'for' connects 'Producer's

    * '>~' connects 'Consumer's

    * '>->' connects 'Pipe's

    * ('>>=') connects 'Effect's

    However, as you progress through this tutorial you will learn that all four
    of these operators actually work on mixtures of all four types, producing
    surprising and emergent behavior.
-}

{- $producers
    @pipes@ represents an effectful stream of input using the 'Producer' type,
    which is a monad transformer that extends a base monad with the ability to
    incrementally 'yield' output to an anonymous downstream handler.  This lets
    us decouple how we generate values from how we consume them.

    The following @stdin@ 'Producer' shows how to incrementally read in lines
    from standard input and 'yield' them downstream, terminating when reaching
    the end of the input:

> -- echo.hs
>
> import Control.Monad (unless)
> import Pipes
> import qualified System.IO as IO
>
> --       +--------+-- A 'Producer' of 'String's
> --       |        |
> --       |        |      +-- Every monad transformer has a base monad.
> --       |        |      |   This time the base monad is 'IO'.
> --       |        |      |  
> --       |        |      |  +-- Every monadic action has a return value.
> --       |        |      |  |   This action returns '()' when finished
> --       v        v      v  v
> stdin :: Producer String IO ()
> stdin = do
>     eof <- lift $ IO.hIsEOF IO.stdin  -- 'lift' an 'IO' action from the base
>                                       -- monad
>     unless eof $ do
>         str <- lift getLine           -- Read a line of input
>         yield str                     -- 'yield' the line of input
>         stdin                         -- Loop

    Whenever you see a type of the form:

> Producer a m r

    ... you should think: \"This outputs a stream of zero or more value of type
    @a@ and finishes off with a single return value of type @r@.

    'yield' emits a value, suspending the current 'Producer' until the value is
    consumed.  You can think of 'yield' as having the following type:

@
 'yield' :: (Monad m) => a -> 'Producer' a m ()
@

    The true type of 'yield' is actually more general and powerful.  Throughout
    the tutorial I will present type signatures like this that are simplified at
    first and then later I will show you neat ways that you can generalize them.
    So read the above type signature as simply saying: \"You can use 'yield' in
    a 'Producer' context, but you may be able to use 'yield' in other contexts,
    too.\"

    The simplest way to consume a 'Producer' is a 'for' loop, which has the
    following type:

@
 \-\-                  +-- Producer      +-- The body of the      +-- Result
 \-\-                  |   to loop       |   loop                 |   
 \-\-                  v   over          v                        v  
 \-\-                  --------------    ---------------------    --------------
 'for' :: (Monad m) => 'Producer' a m r -> (a -> 'Producer' b m r) -> 'Producer' b m r
@

    @(for producer f)@ loops over @(producer)@, substituting each 'yield' in
    @(producer)@ with @(f)@.  If @(f)@ 'yield's any values of its own, then
    these become the new output type of the final result.

    Again, the above type signature is not the true type of 'for', which is
    more general.  Think of the above type signature as saying: \"If the first
    argument of 'for' is a 'Producer' and the second argument returns a
    'Producer', then the final result must be a 'Producer'.\"  If we provide
    arguments other than 'Producer's then the type of the result will also
    change to something other than a 'Producer'.

    Here's an example 'for' @loop@ in action:

> -- echo.hs
>
> loop :: Producer a IO ()
> loop = for stdin $ \str -> do  -- Read this like: "for str in stdin"
>     lift $ putStrLn str        -- The body of the 'for' loop
>
> -- even better: loop = for stdin (lift . putStrLn)

    In this example, 'for' loops over @stdin@ and replaces every 'yield' in
    @stdin@ with the body of the loop.  This is exactly equivalent to the
    following code:

> -- This definition of 'loop' is exactly equivalent to the previous one:
> loop = do
>     eof <- lift $ IO.hIsEOF IO.stdin
>     unless eof $ do
>         str <- lift getLine
>         (lift . putStrLn) str  -- Here we've replaced the original 'yield'
>         loop

    You can think of 'yield' as creating a hole and a 'for' loop is one way to
    fill that hole.

    After this substitution, 'loop' consumed all of the values generated by
    @stdin@ and did not re-'yield' any new values
    , so even though
    @loop@ /technically/ qualifies a 'Producer', it does not use any
    'Producer'-specific features like 'yield':

> --               +-- 'loop' doesn't 'yield' any values, so 'a' is polymorphic,
> --               |   meaning that 'a' could in principle be any type.
> --               v
> loop :: Producer a IO ()

    In fact, every single action in the body of our 'for' @loop@ is just
    trivially 'lift'ed from the base monad and we never 'yield'.  @pipes@
    defines a type synonym for this special case where we only 'lift' and never
    'yield:

@
 data 'X'  -- The uninhabited type

\ type 'Effect' m r = 'Producer' 'X' m r
@

    Since 'X' is uninhabited, a 'Producer' only type-checks as an 'Effect' if
    the 'Producer' never outputs any values and therefore never uses any
    'Producer'-specific features.  @loop@ satisfies this criterion, so we can
    narrow the type signature of @loop@ even further to:

@
 loop :: (Monad m) => 'Effect' 'IO' ()
@

    An 'Effect' always exactly corresponds to an action in the base monad, so we
    can always 'run' these 'Effect's to lower them back down to the base monad:

@
 'run' :: (Monad m) => 'Effect' m r -> m r
@

    'run' is the last piece of the puzzle we need to complete our program:

> -- echo.hs
>
> main :: IO ()
> main = run loop
> 
> -- or you could just inline the 'loop':
> -- main = run $ for stdin (lift . putStrLn)

    Our final program loops over standard input and echoes every line to
    standard output until we hit @Ctrl-D@ to end the input stream:

> $ ghc -O2 echo.hs
> $ ./echo
> Test<Enter>
> Test
> ABC<Enter>
> ABC
> <Ctrl-D>
> $

    You can also loop over lists, too.  To do so, convert the list to a
    'Producer' using 'each':

@
 'each' :: (Monad m) => [a] -> 'Producer' a m ()
 each as = mapM_ yield as
@

    Use this to iterate over lists using a \"foreach\" loop:

>>> run $ for (each [1..4]) (lift . print)
1
2
3
4

-}

{- $composability
    You might wonder why the body of a 'for' loop can be a 'Producer'.  Let's
    test out this feature by defining a new loop body that @duplicate@s every
    value:

> -- nested.hs
>
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude already has stdin
> 
> duplicate :: (Monad m) => a -> Producer a m ()
> duplicate x = do
>     yield x
>     yield x
>
> loop :: Producer String IO ()
> loop = for P.stdin duplicate
>
> -- This is the exact same as:
> --
> -- loop = for P.stdin $ \x -> do
> --     yield x
> --     yield x

    This time our @loop@ outputs 'String's, specifically two copies of every
    line that we read from standard input.

    Since @loop@ is itself a 'Producer', we can loop over our @loop@:

> -- nested.hs
>
> -- I heard you like loops
> main = run $ for loop (lift . putStrLn)

    This creates a program which echoes every line from standard input to
    standard output twice:

> $ ./nested
> Test<Enter>
> Test
> Test
> ABC<Enter>
> ABC
> ABC
> <Ctrl-D>
> $

    But is this feature really necessary?  Couldn't we have instead written this
    using a nested for loop?

> main = run $
>     for P.stdin $ \str1 ->
>         for (duplicate str1) $ \str2 ->
>             lift $ putStrLn str

    Yes, we could have!  In fact, this is a special case of the following
    equality, which always holds no matter what:

@
 \-\- m :: (Monad m) =>      'Producer' a m ()  -- i.e. \'P.stdin\'
 \-\- f :: (Monad m) => a -> 'Producer' b m ()  -- i.e. \'duplicate\'
 \-\- g :: (Monad m) => b -> 'Producer' c m ()  -- i.e. \'(lift . putStrLn)\'

\ for (for m f) g = for m (\x -> for (f x) g)
@

    We can understand the rationale behind this equality if we define the
    following operator that is the point-free counterpart to 'for':

@
 (~>) :: (Monad m)
      => (a -> 'Producer' b m r)
      -> (b -> 'Producer' c m r)
      -> (a -> 'Producer' c m r)
 (f ~> g) x = for (f x) g
@

    Using the ('~>') operator, we can transform our original equality into the
    following more symmetric form:

@
 f :: (Monad m) => a -> 'Producer' b m r
 g :: (Monad m) => b -> 'Producer' c m r
 h :: (Monad m) => c -> 'Producer' d m r

\ \-\- Associativity
 (f ~> g) ~> h = f ~> (g ~> h)
@

    This looks just like an associativity law.  In fact, ('~>') has another nice
    property, which is that 'yield' is its left and right identity:

> -- Left Identity
> yield ~> f = f

> -- Right Identity
> f ~> yield = f

    In other words, 'yield' and ('~>') form a 'Category' where ('~>') plays the
    role of the composition operator and 'yield' is the identity.

    Notice that if we translate the left identity law to use 'for' instead of
    ('~>') we get:

> for (yield x) f = f x

    This just says that if you iterate over a single-element 'Producer' with no
    side effects, then you can instead cut out the middle man and directly apply
    the body of the loop to that single element.

    If we translate the right identity law to use 'for' instead of ('~>') we
    get:

> for m yield = m

    This just says that if the only thing you do is re-'yield' every element of
    a stream, you get back your original stream.

    These three \"for loop\" laws summarize our intuition for how 'for' loops
    should behave:

> for (for m f) g = for m (\x -> for (f x) g)
>
> for (yield x) f = f x
>
> for m yield = m

    ... and because these are just 'Category' laws in disguise that means that
    'Producer's are composable in a theoretically rigorous sense.

    In fact, we get more out of this than just a bunch of equations.  We also
    get a useful operator, too: ('~>').  We can use this operator to condense
    our original code into the following more succinct form:

> main = run $ for P.stdin (duplicate ~> lift . putStrLn)
>
> -- Read as: "for each line in stdin, duplicate and putStrLn it"

    This means that we can also choose to program in a more functional style and
    think of stream processing in terms of composing transformations using
    ('~>') instead of nesting a bunch of 'for' loops.

    The above example is a microcosm of the design philosophy behind the @pipes@
    library:

    * Define primitives in terms of categories

    * Specify expected behavior in terms of category laws

    * Think compositionally instead of sequentially
-}

{- $consumers
    Sometimes you don't want use a 'for' loop because you don't want to consume
    every element of a 'Producer' or because you don't want to process every
    value of a 'Producer' the exact same way.

    The most general solution is to externally iterate over the 'Producer' using
    the 'next' command:

@
 'next' :: (Monad m) => 'Producer' a m r -> m (Either r (a, 'Producer' a m r))
@

    Think of 'next' as pattern matching on the head of the 'Producer'.  This
    'Either' returns a 'Left' if the 'Producer' is done or it returns a 'Right'
    containing the next value, @a@, along with the remainder of the 'Producer'.

    However, sometimes we can get away with something a little more elegant,
    like a 'Consumer', which represents an effectful sink of values.  A
    'Consumer' is a monad transformer that extends the base monad with the
    ability to incrementally 'await' input from an anonymous upstream source.
    The following @printN@ 'Consumer' shows how to 'print' out only the first
    @n@ elements received:

> -- printn.hs
>
> import Control.Monad (replicateM_)
> import Pipes
> import qualified Pipes.Prelude as P
>
> --               +--------+-- A 'Consumer' of 'String's
> --               |        |
> --               v        v
> printN :: Int -> Consumer String IO ()
> printN n = replicateM_ n $ do  -- Repeat the following block 'n' times
>     str <- await               -- 'await' a new 'String'
>     lift $ putStrLn str        -- Print out the 'String'

    'await' is the dual of 'yield': we suspend our 'Consumer' until we receive a
    new value:

@
 'await' :: (Monad m) => 'Consumer' a m a
@

    One way to feed a 'Consumer' is to repeatedly feed the same input using
    using ('>~'):

@
 \-\-                   +- Feed       +- Consumer to    +- Returns new
 \-\-                   |  action     |  feed           |  Effect
 \-\-                   v             v                 v  
 \-\-                   ----------    --------------    ----------
 ('>~') :: (Monad m) => 'Effect' m b -> 'Consumer' b m c -> 'Effect' m c
@

    This runs the given 'Effect' every time the 'Consumer' 'await's a value,
    using the return value of the 'Effect' to supply the input:

>>> run $ lift getLine >~ printN 3
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
>>>

    You might wonder why ('>~') uses an 'Effect' instead of a raw action in the
    base monad.  The reason why is that ('>~') actually permits the following
    more general type:

@
 ('>~') :: (Monad m) => 'Consumer' a m b -> 'Consumer' b m c -> 'Consumer' a m c
@

    We can feed a 'Consumer' with yet another 'Consumer' so that you can 'await'
    while you 'await'.  For example, we could define the following intermediate
    'Consumer' that requests two 'String's and returns them concatenated:

> -- printn.hs
>
> doubleUp :: (Monad m) => Consumer String m String
> doubleUp = do
>     str1 <- await
>     str2 <- await
>     return (str1 ++ str2)
>
> -- even better: doubleUp = (++) <$> await <*> await

    We can now insert this in between @(lift getLine)@ and @printN@ and see what
    happens:

>>> run $ lift getLine >~ doubleUp >~ printN 3
Test<Enter>
ing<Enter>
Testing
ABC<Enter>
DEF<Enter>
ABCDEF
42<Enter>
000<Enter>
42000

    'doubleUp' splits every request from 'printN' into two separate requests and
    returns back the concatenated result.

    We didn't need to parenthesize the above chain of ('>~') operators, because
    ('>~') is associative:

> -- Associativity
> (f >~ g) >~ h = f >~ (g >~ h)

    ... so we can always omit the parentheses since the meaning is unambiguous:

> f >~ g >~ h

    Also, ('>~') has an identity, which is 'await'!

> -- Left identity
> await >~ f = f
>
> -- Right Identity
> f >~ await = f

    In other words, ('>~') and 'await' form a 'Category', too, and 'Consumer's
    are also composable.
-}

{- $pipes
    We don't need to restrict ourselves to using 'Producer's exclusively or
    'Consumer's exclusively.  We can connect 'Producer's and 'Consumer's
    together using ('>->'):

@
('>->') :: (Monad m)
     => 'Producer' a m r
     -> 'Consumer' a m r
     -> 'Effect'     m r
@

    This returns an 'Effect' which we can 'run':

> -- printn.hs
>
> main = run $ P.stdin >-> printN 3

    This will prompt the user for input three times, echoing each input:

> $ ./printn
> Test<Enter>
> Test
> ABC<Enter>
> ABC
> 42<Enter>
> 42
> $

    ('>->') pairs every 'await' in the 'Consumer' with a 'yield' in the
    'Producer'.  Since our 'Consumer' only calls 'await' three times, our
    'Producer' only 'yield's three times and therefore only prompts the user
    for input three times.  Once the 'Consumer' terminates the whole 'Effect'
    terminates.

    The opposite is true, too: if the 'Producer' terminates early, then the
    whole 'Effect' terminates.

> $ ./printn
> Test<Enter>
> Test
> <Ctrl-D>
> $

    This is why ('>->') requires that both the 'Producer' and 'Consumer' share
    the same type of return value: whichever one terminates first provides the
    return value for the entire 'Effect'.

    Let's test this by modifying our 'Producer' to return 'False' and our
    'Consumer' to return 'True':

> -- printn.hs
>
> import Control.Applicative ((<$))  -- (<$) modifies return values
>
> main = do
>     finished <- run $ (False <$ P.stdin) >-> (True <$ printN 3)
>     putStrLn $ if finished then "Success!" else "You had one job..."

    This lets us diagnose whether the 'Producer' or 'Consumer' terminated first:

> $ ./printn
> Test<Enter>
> Test
> ABC<Enter>
> ABC
> 42<Enter>
> 42
> Success!
> $ ./printn
> <Ctrl-D>
> You had one job...
> $

    You might wonder why ('>->') returns an 'Effect' that we have to 'run'
    instead of directly returning an action in the base monad.  This is because
    you can connect things other than 'Producer's and 'Consumer's, like 'Pipe's,
    which are effectful stream transformations.

    A 'Pipe' is a monad transformer that is a mix between a 'Producer' and
    'Consumer', because a 'Pipe' can both 'await' and 'yield'.  The following
    example 'Pipe' behaves like the Prelude's 'take', only allowing a fixed
    number of values to flow through:

> -- take.hs
>
> import Control.Monad (replicateM_)
> import Pipes
> import Prelude hiding (take)
>
> take :: (Monad m) => Int -> Pipe a a m ()
> take n = replicateM_ n $ do
>     a <- await
>     yield a
-}

{- $conclusion
    This tutorial covers the core concepts of connecting, building, and reading
    @pipes@ code.  However, this library is only the core component in an
    ecosystem of streaming components.  More powerful libraries that build upon
    @pipes@ include:

    * @pipes-safe@: Resource management and exception safety for @pipes@

    * @pipes-concurrency@: Concurrent reactive programming and message passing

    * @pipes-parse@: Central idioms for stream parsing

    * @pipes-arrow@: Push-based directed acyclic graphs for @pipes@

    These libraries provide functionality specialized to common streaming
    domains.  Additionally, there are several derived libraries on Hackage that
    provide even higher-level functionality, which you can find by searching
    under the \"Pipes\" category or by looking for packages with a @pipes-@
    prefix in their name.  Current examples include:

    * @pipes-network@/@pipes-network-tls@: Networking

    * @pipes-zlib@: Compression and decompression

    * @pipes-binary@: Binary serialization

    * @pipes-attoparsec@: High-performance parsing

    Even these derived packages still do not explore the full potential of
    @pipes@ functionality.  Advanced @pipes@ users can explore this library in
    greater detail by studying the documentation in the "Pipes" module to learn
    about the symmetry behind the underlying 'Proxy' type and operators.

    To learn more about @pipes@, ask questions, or follow @pipes@ development,
    you can subscribe to the @haskell-pipes@ mailing list at:

    <https://groups.google.com/forum/#!forum/haskell-pipes>

    ... or you can mail the list directly at
    <mailto:haskell-pipes@googlegroups.com>.
-}

{- $appendix

@
\-\- echo.hs

import Control.Monad (unless)
import Pipes
import qualified System.IO as IO

stdin :: Producer String IO ()
stdin = do
    eof <- lift $ IO.hIsEOF IO.stdin
    unless eof $ do
        str <- lift getLine
        yield str
        stdin

loop :: Effect IO ()
loop = for stdin $ \str -> do
    lift $ putStrLn str

main :: IO ()
main = run loop

\-\- nested.hs

import Pipes
import qualified Pipes.Prelude as P

duplicate :: (Monad m) => a -> Producer a m ()
duplicate x = do
    yield x
    yield x

loop :: Producer String IO ()
loop = for P.stdin duplicate

main  = run $ for loop (lift . putStrLn)
@

-}
