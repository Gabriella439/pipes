{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| @pipes@ is a lightweight and powerful library for processing effectful
    streams in constant memory.

    @pipes@ supports a wide variety of stream programming abstractions,
    including:

    * Generators, for loops, and internal \/ external iterators

    * 'ListT' done right

    * Unix pipes

    * Folds

    * Message passing and reactive programming (using the @pipes-concurrency@
      library)

    * Stream parsing (using the @pipes-safe@ library)

    * Exception-safe streams (using the @pipes-safe@ library)

    * Directed acyclic graphs (using the @pipes-arrow@ library)

    If you want a really fast Quick Start guide, read the documentation in
    "Pipes.Prelude" from top to bottom.

    This tutorial is more extensive and explains the @pipes@ API in greater
    detail and illustrates several idioms.
-}

module Pipes.Tutorial (
    -- * Producers
    -- $producers

    -- * Theory
    -- $theory

    -- * Consumers
    -- $consumers

    -- * Appendix
    -- $appendix
    ) where

import Control.Category
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
import Prelude hiding ((.), id)

{- $producers
    The library represents effectful streams of input using 'Producer's.  A
    'Producer' is a monad transformer that extends the base monad with the
    ability to incrementally 'yield' output.  The following @stdin@ 'Producer'
    shows how to incrementally read and 'yield' lines from standard input,
    terminating when we reach the end of the input:

> -- echo.hs
>
> import Control.Monad (unless)
> import Pipes
> import qualified System.IO as IO
>
> --       +--------+-- A 'Producer' of 'String's
> --       |        |
> --       |        |      +-- The base monad is 'IO'
> --       |        |      |
> --       |        |      |  +-- Returns '()' when finished
> --       |        |      |  |
> --       v        v      v  v
> stdin :: Producer String IO ()
> stdin = do
>     eof <- lift $ IO.hIsEOF IO.stdin  -- 'lift' actions from the base monad
>     unless eof $ do
>         str <- lift getLine           -- Read a line of input
>         yield str                     -- 'yield' the line of input
>         stdin                         -- Loop

    'yield' emits a value, suspending the current 'Producer' until the value is
    consumed:

> yield :: (Monad m) => a -> Producer a m ()

    The simplest way to consume a 'Producer' is a 'for' loop, which has the
    following type:

> for :: (Monad m) => Producer a m r -> (a -> Producer b m r) -> Producer b m r

    Notice how this type greatly resembles the type of @(flip concatMap)@ (or
    ('>>=') for the list monad):

> flip concatMap :: [a] -> (a -> [b]) -> [b]

    Here's an example 'for' @loop@:

> -- echo.hs
>
> --               +-- 'loop' does not emit any values, so 'a' is polymorphic
> --               |
> --               v
> loop :: Producer a IO ()
> loop = for stdin $ \str -> do  -- Read this like: "for (str in stdin):"
>     lift $ putStrLn str        -- The body of the 'for' loop
>
> -- even better: loop = for stdin (lift . putStrLn)

    Notice how 'loop' does not re-emit any values of its own.  @pipes@ defines a
    type synonym for this special case:

> data X  -- The uninhabited type
>
> type Effect m r = Producer X m r

    A 'Producer' only type-checks as an 'Effect' if the 'Producer' never outputs
    any values, because 'X' is uninhabited.  This means we can change the type
    signature of @loop@ to:

> loop :: (Monad m) => Effect IO ()

    'Effect's are special because we can 'run' any 'Effect' and convert it back
    to the base monad:

> run :: (Monad m) => Effect m r -> m r

    If our @loop@ had any residual unhandled output, then the following 'run'
    would not type-check:

> -- echo.hs
>
> main :: IO ()
> main = run loop

    The final program loops over standard input and echoes every line to
    standard output:

> $ ghc -O2 echo.hs
> $ ./echo
> Test<Enter>
> Test
> ABC<Enter>
> ABC
> ^D
> $

    You can find the full @echo.hs@ program in the Appendix section at the end
    of this tutorial along with all the other following examples.
-}

{- $theory
    You might wonder why the body of a 'for' loop can be a 'Producer'.  Let's
    test out this feature by defining a new loop body that re-'yield's every
    value twice:

> -- nested.hs
>
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude already has 'stdin'
>
> body :: (Monad m) => a -> Producer a m ()
> body x = do
>     yield x
>     yield x
>
> loop :: Producer String IO ()
> loop = for P.stdin body
>
> -- This is the same as:
> --
> -- loop = for P.stdin $ \str -> do
> --     yield str
> --     yield str

    This time @loop@ outputs 'String's, specifically two copies of every line
    read from standard input.

    Since @loop@ is itself a 'Producer', we can loop over our @loop@, dawg:

> -- nested.hs
>
> main  = run $ for loop (lift . putStrLn)

    This creates a program which echoes every line from standard input to
    standard output twice:

> $ ./nested
> Test<Enter>
> Test
> Test
> ABC<Enter>
> ABC
> ABC
> ^D

    But is this feature really necessary?  Couldn't we have rewritten this using
    a nested for loop instead?

> main = run $
>     for P.stdin $ \str1 ->
>         for (body str1) $ \str2 ->
>             lift $ putStrLn str

    Yes, we could have!  In fact, this is a special case of the following
    equality, which always holds no matter what:

> -- m :: (Monad m) =>      Producer a m ()  -- i.e. 'P.stdin'
> -- f :: (Monad m) => a -> Producer b m ()  -- i.e. 'body'
> -- g :: (Monad m) => b -> Producer c m ()  -- i.e. '(lift . putStrLn)'
>
> for (for m f) g = for m (\x -> for (f x) g)

    We can understand the rationale behind this equality if we define the
    following operator that is the point-free counterpart to 'for':

> (/>/) :: (Monad m)
>       => (a -> Producer b m r)
>       -> (b -> Producer c m r)
>       -> (a -> Producer c m r)
> (f />/ g) x = for (f x) g

    Using this operator we can transform our original equality into the
    following more symmetric form:

> f :: (Monad m) => a -> Producer b m r
> g :: (Monad m) => b -> Producer c m r
> h :: (Monad m) => c -> Producer d m r
>
> -- Associativity
> (f />/ g) />/ h = f />/ (g />/ h)

    That's much more symmetric.  In fact, it looks just like an associativity
    law for the composition operator of a 'Control.Category.Category', where
    ('/>/') behaves like the composition operator.  If that were true, though,
    then we would also expect two identity laws and, sure enough, 'yield'
    behaves like the identity of ('/>/'):

> -- Left Identity
> yield />/ f = f
>
> -- Right Identity
> f />/ yield = f

    If we translate the left identity law to use 'for' instead of ('/>/') we
    get:

> for (yield x) f = f x

    This just says that if you iterate over a single-element 'Producer' with no
    side effects, then you can instead cut out the middle man and directly apply
    the body of the loop to that single element.

    If we translate the right identity law to use 'for' instead of ('/>/') we
    get:

> for m yield = m

    This just says that if the only thing you do is re-'yield' every element of
    a stream, you get back your original stream.

    These three \"for loop\" laws summarize our common-sense intuition for how
    'for' loops should behave:

> for (for m f) g = for m (\x -> for (f x) g)
>
> for (yield x) f = f x
>
> for m yield = m

    ... and they miraculously fall out of the 'Control.Category.Category' laws
    for ('/>/') and 'yield'.

    In fact, we get more out of this than just a bunch of equations.  We also
    got a useful operator, too: ('/>/').  We can use this operator to condense
    our original code into the following more succinct form:

> main  = run $ for P.stdin (body />/ lift . putStrLn)

    This means that we can also choose to program in a more functional style and
    think of stream processing as composing a sequence of transformations using
    ('/>/') instead of nesting a bunch of 'for' loops.

    The above example is a microcosm of the design philosophy behind the @pipes@
    library:

    * Define primitives in terms of categories

    * Specify expected behavior in terms of category laws

    * Think compositionally instead of sequentially
-}

{- $consumers
    Sometimes you don't want use a 'for' loop because you don't want to process
    every value of a 'Producer' the exact same way.  For example, there is no
    (easy) way to consume only the first few elements of a 'Producer' using a
    'for' loop.

    The most general option is to externally iterate over the 'Producer' using
    the 'next' command:

> next :: (Monad m) => Producer a m r -> m (Either r (a, Producer a m r))

    Think of 'next' as pattern matching on the head of the 'Producer'.  This
    'Either' returns a 'Left' if the 'Producer' is done or it returns a 'Right'
    containing the next value, @a@, along with the remainder of the 'Producer'.

    However, sometimes we can get away with something a little more elegant,
    like a 'Consumer', which represents an effectful fold.  A 'Consumer' is a
    monad transformer that extends the base monad with the ability to
    incrementally 'await' input.  The following @printN@ 'Consumer' shows how to
    'print' out only the first @n@ elements received:

> import Pipes
>
> --                           +--------+-- A 'Consumer' of 'Show'able 'a's
> --                           |        |
> --                           v        v
> printN :: (Show a) => Int -> Consumer a IO ()
> printN n = replicateM_ n $ do
>     a <- await ()  -- 'await' a new input
>     lift $ print a

    'await' is the dual of 'yield': we suspend our 'Pipe' until we are supplied
    with a new value.
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

> -- echo.hs
>
> import Control.Monad (unless)
> import Pipes
> import qualified System.IO as IO
>
> stdin :: Producer String IO ()
> stdin = do
>     eof <- lift $ IO.hIsEOF IO.stdin
>     unless eof $ do
>         str <- lift getLine
>         yield str
>         stdin
>
> loop :: Effect IO ()
> loop = for stdin $ \str -> do
>     lift $ putStrLn str
>
> main :: IO ()
> main = run loop

> -- nested.hs
>
> import Pipes
> import qualified Pipes.Prelude as P
>
> body :: (Monad m) => a -> Producer a m ()
> body x = do
>     yield x
>     yield x
>
> loop :: Producer String IO ()
> loop = for P.stdin body
>
> main  = run $ for loop (lift . putStrLn)

-}
