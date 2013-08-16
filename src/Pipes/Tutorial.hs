{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| Conventional Haskell stream programming forces you to choose only two of the
    following three features:

    * Effects

    * Streaming

    * Composability

    If you sacrifice /Effects/ you get Haskell's pure and lazy lists, which you
    can transform using composable functions in constant space, but without
    interleaving effects.

    If you sacrifice /Streaming/ you get 'mapM', 'forM' and
    \"ListT done wrong\", which are composable and effectful, but do not return
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
    learn about by reading either:

    * the paper \"Monad Transformers - Step by Step\",

    * chapter 18 of \"Real World Haskell\" on monad transformers, or:

    * the documentation of the @transformers@ library.

    If you want a Quick Start guide to @pipes@, read the documentation in
    "Pipes.Prelude" from top to bottom.

    This tutorial is more extensive and explains the @pipes@ API in greater
    detail and illustrates several idioms.
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

    -- * ListT
    -- $listT

    -- * Tricks
    -- $tricks

    -- * Conclusion
    -- $conclusion

    -- * Appendix: Types
    -- $types
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
    effort to produce a highly-efficient program that streams data in constant
    memory.

    To enforce loose coupling, components can only communicate using two
    commands:

    * 'yield': Send output data

    * 'await': Receive input data

    @pipes@ has four types of components built around these two commands:

    * 'Producer's can only 'yield' values and they model streaming sources

    * 'Consumer's can only 'await' values and they model streaming sinks

    * 'Pipe's can both 'yield' and 'await' values and they model stream
      transformations

    * 'Effect's can neither 'yield' nor 'await' and they model non-streaming
      components

    You can connect these components together in four separate ways which
    parallel to the four above types:

    * 'for' handles 'yield's

    * ('>~') handles 'await's

    * ('>->') handles both 'yield's and 'await's

    * ('>>=') handles return values

    As you connect components their types will change to reflect inputs and
    outputs that you've fused away.  You know that you're done connecting things
    when you get an 'Effect', meaning that you have handled all inputs and
    outputs.  You 'run' this final 'Effect' to begin streaming.
-}

{- $producers
    'Producer's are effectful streams of input.  Specifically, a 'Producer' is a
    monad transformer that extends any base monad with a new 'yield' command.
    This 'yield' command lets you send output downstream to an anonymous
    handler, decoupling how you generate values from how you consume them.

    The following @stdin@ 'Producer' shows how to incrementally read in
    'String's from standard input and 'yield' them downstream, terminating
    gracefully when reaching the end of the input:

> -- echo.hs
>
> import Control.Monad (unless)
> import Pipes
> import System.IO (isEOF)
>
> --       +--------+-- A 'Producer' that yields 'String's
> --       |        |
> --       |        |      +-- Every monad transformer has a base monad.
> --       |        |      |   This time the base monad is 'IO'.
> --       |        |      |  
> --       |        |      |  +-- Every monadic action has a return value.
> --       |        |      |  |   This action returns '()' when finished
> --       v        v      v  v
> stdin :: Producer String IO ()
> stdin = do
>     eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
>     unless eof $ do
>         str <- lift getLine
>         yield str            -- 'yield' the 'String'
>         stdin                -- Loop

    'yield' emits a value, suspending the current 'Producer' until the value is
    consumed.  If nobody consumes the value (which is possible) then 'yield'
    never returns.  You can think of 'yield' as having the following type:

@
 'yield' :: 'Monad' m => a -> 'Producer' a m ()
@

    The true type of 'yield' is actually more general and powerful.  Throughout
    the tutorial I will present type signatures like this that are simplified at
    first and then later reveal more general versions.  So read the above type
    signature as simply saying: \"You can use 'yield' within a 'Producer', but
    you may be able to use 'yield' in other contexts, too.\"

    Click the link to 'yield' to navigate to its documentation.  There you will
    see the fully general type and underneath you will see equivalent simpler
    types.  One of these says that 'yield' can also be used within a 'Pipe':

@
 'yield' :: 'Monad' m => a -> 'Pipe' x a m ()
@

    Use simpler types like these to guide you until you understand the fully
    general type.

    'for' loops are the simplest way to consume a 'Producer' like @stdin@.
    'for' has the following type:

@
 \-\-                +-- Producer      +-- The body of the   +-- Result
 \-\-                |   to loop       |   loop              |
 \-\-                v   over          v                     v
 \-\-                --------------    ------------------    ----------
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Effect' m ()) -> 'Effect' m r
@

    @(for producer body)@ loops over @(producer)@, substituting each 'yield' in
    @(producer)@ with @(body)@.

    You can also deduce that behavior purely from the type signature:

    * The body of the loop takes exactly one argument of type @(a)@, which is
      the same as the output type of the 'Producer'.  Therefore, the body of the
      loop must get its input from that 'Producer' and nowhere else.

    * The return value of the input 'Producer' matches the return value of the
      result, therefore 'for' must loop over the entire 'Producer' and not skip
      anything.

    Again, the above type signature is not the true type of 'for', which is
    actually more general.  Think of the above type signature as saying: \"If
    the first argument of 'for' is a 'Producer' and the second argument returns
    a 'Effect', then the final result must be an 'Effect'.\"

    Click the link to 'for' to navigate to its documentation.  There you will
    see the fully general type and underneath you will see equivalent simpler
    types.  One of these says that the body of the loop can be an 'Producer',
    too:

@
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Producer' b m ()) -> 'Producer' b m r
@

    The first type signature I showed for 'for' was a special case of this
    slightly more general signature.  An 'Effect' is just a 'Producer' that
    never 'yield's (i.e. an 'Effect' only 'lift's):

@
 data 'X'  -- The uninhabited type

\ type 'Effect' m r = 'Producer' 'X' m r
@

    If a 'Producer' never 'yield's, it will type check as an 'Effect'.

    This is why 'for' permits two different type signatures.  The first type
    signature is just a special case of the second one:

@
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Producer' b m ()) -> 'Producer' b m r

\ -- Specialize \'b\' to \'X\'
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Producer' 'X' m ()) -> 'Producer' 'X' m r

\ -- Producer X = Effect
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Effect'     m ()) -> 'Effect'     m r
@

    This is the same trick that all @pipes@ functions use to work with various
    combinations of 'Producer's, 'Consumer's, 'Pipe's, and 'Effect's.  Each
    function really has just one general type, which you can then simplify down
    to multiple useful alternative types.

    Here's an example use of a 'for' @loop@, where the second argument (the
    loop body) is an 'Effect':

> -- echo.hs
>
> loop :: Effect IO ()
> loop = for stdin $ \str -> do  -- Read this like: "for str in stdin"
>     lift $ putStrLn str        -- The body of the 'for' loop
>
> -- more concise: loop = for stdin (lift . putStrLn)

    In this example, 'for' loops over @stdin@ and replaces every 'yield' in
    @stdin@ with the body of the loop, printing each line.  This is exactly
    equivalent to the following code, which I've placed side-by-side with the
    original definition of @stdin@ for comparison:

> loop = do                      |  stdin = do
>     eof <- lift isEOF          |      eof <- lift isEOF
>     unless eof $ do            |      unless eof $ do
>         str <- lift getLine    |          str <- lift getLine
>         (lift . putStrLn) str  |          yield str
>         loop                   |          stdin

    You can think of 'yield' as creating a hole and a 'for' loop is one way to
    fill that hole.

    Notice how the final @loop@ only 'lift's actions from the base monad and
    does nothing else.  This property is true for all 'Effect's, which are just
    glorified wrappers around actions the base monad.  This means we can 'run'
    these 'Effect's to remove their 'lift's and lower them back down to the
    equivalent computation in the base monad:

@
 'run' :: 'Monad' m => 'Effect' m r -> m r
@

    This is the real type signature of 'run', which refuses to accept anything
    other than an 'Effect'.  This ensures that we handle all inputs and outputs
    before streaming data:

> -- echo.hs
>
> main :: IO ()
> main = run loop

    ... or you could inline the entire @loop@ into the following one-liner:

> main = run $ for stdin (lift . putStrLn)

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

    The final behavior is indistinguishable from just removing all the 'lift's
    from @loop@:

> main = do               |  loop = do
>     eof <- isEof        |      eof <- lift isEof
>     unless eof $ do     |      unless eof $ do
>         str <- getLine  |          str <- lift getLine
>         putStrLn str    |          (lift . putStrLn) str
>         main            |          loop

    This @main@ is what we might have written by hand if we were not using
    @pipes@, but with @pipes@ we can decouple the input and output logic from
    each other.  When we connect them back together, we still produce streaming
    code equivalent to what a sufficiently careful Haskell programmer would
    write.

    You can also use 'for' to loop over lists, too.  To do so, convert the list
    to a 'Producer' using 'each', which is exported by default from "Pipes":

> each :: (Monad m) => [a] -> Producer a m ()
> each as = mapM_ yield as

    Combine 'for' and 'each' to iterate over lists using a \"foreach\" loop:

>>> run $ for (each [1..4]) (lift . print)
1
2
3
4

    'each' is actually more general and works for any 'Foldable':

@
 'each' :: ('Monad' m, 'Foldable' f) => f a -> 'Producer' a m ()
@

     So you can loop over any 'Foldable' container or even a 'Maybe':

>>> run $ for (each (Just 1)) (lift . print)
1

-}

{- $composability
    You might wonder why the body of a 'for' loop can be a 'Producer'.  Let's
    test out this feature by defining a new loop body that @duplicate@s every
    value:

> -- nested.hs
>
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude already has 'stdin'
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

    This time our @loop@ is a 'Producer' that outputs 'String's, specifically
    two copies of each line that we read from standard input.  Since @loop@ is a
    'Producer' we cannot 'run' it because there is still unhandled output.
    However, we can use yet another 'for' to handle this new duplicated stream:

> -- nested.hs
>
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

    But is this really necessary?  Couldn't we have instead written this using a
    nested for loop?

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

    We can understand the rationale behind this equality if we first define the
    following operator that is the point-free counterpart to 'for':

@
 (~>) :: (Monad m)
      => (a -> 'Producer' b m r)
      -> (b -> 'Producer' c m r)
      -> (a -> 'Producer' c m r)
 (f ~> g) x = for (f x) g
@

    Using ('~>') (pronounced \"into\"), we can transform our original equality
    into the following more symmetric form:

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

    In other words, 'yield' and ('~>') form a 'Category', specifically the
    generator category, where ('~>') plays the role of the composition operator
    and 'yield' is the identity.  If you don't know what a 'Category' is, that's
    okay, and category theory is not a prerequisite for using @pipes@.  All you
    really need to know is that @pipes@ uses some simple category theory to keep
    the API intuitive and easy to use.

    Notice that if we translate the left identity law to use 'for' instead of
    ('~>') we get:

> for (yield x) f = f x

    This just says that if you iterate over a pure single-element 'Producer',
    then you could instead cut out the middle man and directly apply the body of
    the loop to that single element.

    If we translate the right identity law to use 'for' instead of ('~>') we
    get:

> for m yield = m

    This just says that if the only thing you do is re-'yield' every element of
    a stream, you get back your original stream.

    These three \"for loop\" laws summarize our intuition for how 'for' loops
    should behave and because these are 'Category' laws in disguise that means
    that 'Producer's are composable in a rigorous sense of the word.

    In fact, we get more out of this than just a bunch of equations.  We also
    get a useful operator, too: ('~>').  We can use this operator to condense
    our original code into the following more succinct form that composes two
    transformations:

> main = run $ for P.stdin (duplicate ~> lift . putStrLn)

    This means that we can also choose to program in a more functional style and
    think of stream processing in terms of composing transformations using
    ('~>') instead of nesting a bunch of 'for' loops.

    The above example is a microcosm of the design philosophy behind the @pipes@
    library:

    * Define the API in terms of categories

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
 'next' :: 'Monad' m => 'Producer' a m r -> m ('Either' r (a, 'Producer' a m r))
@

    Think of 'next' as pattern matching on the head of the 'Producer'.  This
    'Either' returns a 'Left' if the 'Producer' is done or it returns a 'Right'
    containing the next value, @a@, along with the remainder of the 'Producer'.

    However, sometimes we can get away with something a little more simple and
    elegant, like a 'Consumer', which represents an effectful sink of values.  A
    'Consumer' is a monad transformer that extends the base monad with a new
    'await' command. This 'await' command lets you receive input from an
    anonymous upstream source.

    The following @stdout@ 'Consumer' shows how to incrementally 'await'
    'String's and print them to standard output, terminating gracefully when
    receiving a broken pipe error:

> import Control.Monad (unless)
> import Control.Exception (try, throwIO)
> import qualified GHC.IO.Exception as G
> import Pipes
>
> --        +--------+-- A 'Consumer' that awaits 'String's
> --        |        |
> --        v        v
> stdout :: Consumer String IO ()
> stdout = do
>     str <- await  -- 'await' a 'String'
>     x   <- lift $ try $ putStrLn str
>     case x of
>         -- Gracefully terminate if we got a broken pipe error
>         Left e@(G.IOError { G.ioe_type = t}) ->
>             lift $ unless (t == G.ResourceVanished) $ throwIO e
>         -- Otherwise loop
>         Right () -> stdout

    'await' is the dual of 'yield': we suspend our 'Consumer' until we receive a
    new value.  If nobody provides a value (which is possible) then 'await'
    never returns.  You can think of 'await' as having the following type:

@
 'await' :: 'Monad' m => 'Consumer' a m a
@

    One way to feed a 'Consumer' is to repeatedly feed the same input using
    using ('>~') (pronounced \"feed\"):

@
 \-\-                 +- Feed       +- Consumer to    +- Returns new
 \-\-                 |  action     |  feed           |  Effect
 \-\-                 v             v                 v  
 \-\-                 ----------    --------------    ----------
 ('>~') :: 'Monad' m => 'Effect' m b -> 'Consumer' b m c -> 'Effect' m c
@

    This runs the given 'Effect' every time the 'Consumer' 'await's a value,
    using the return value of the 'Effect' to supply the input:

>>> run $ lift getLine >~ stdout
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
...

    You might wonder why ('>~') uses an 'Effect' instead of a raw action in the
    base monad.  The reason why is that ('>~') actually permits the following
    more general type:

@
 ('>~') :: 'Monad' m => 'Consumer' a m b -> 'Consumer' b m c -> 'Consumer' a m c
@

    ('>~') is the dual of ('~>'), composing 'Consumer's instead of 'Producer's.

    This means that you can feed a 'Consumer' with yet another 'Consumer' so
    that you can 'await' while you 'await'.  For example, we could define the
    following intermediate 'Consumer' that requests two 'String's and returns
    them concatenated:

> -- printn.hs
>
> doubleUp :: (Monad m) => Consumer String m String
> doubleUp = do
>     str1 <- await
>     str2 <- await
>     return (str1 ++ str2)
>
> -- more concise: doubleUp = (++) <$> await <*> await

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

    In other words, ('>~') and 'await' form a 'Category', too, specifically the
    iteratee category, and 'Consumer's are also composable.
-}

{- $pipes
    Our previous programs were unsatisfactory because they were biased either
    towards the 'Producer' end or the 'Consumer' end.  As a result, we had to
    choose between gracefully handling end of input (using 'P.stdin') or
    gracefully handling broken pipes (using 'P.stdout'), but not both at the
    same time.

    However, we don't need to restrict ourselves to using 'Producer's
    exclusively or 'Consumer's exclusively.  We can connect 'Producer's and
    'Consumer's directly together using ('>->') (pronounced \"pipe\"):

@
 ('>->') :: 'Monad' m => 'Producer' a m r -> 'Consumer' a m r -> 'Effect' m r
@

    This returns an 'Effect' which we can 'run':

> -- echo2.hs
>
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude also provides 'stdout'
>
> main = run $ P.stdin >-> P.stdout

    This program is more declarative of our intent: we want to stream values
    from 'P.stdin' to 'P.stdout'.  The above \"pipeline\" not only echoes
    standard input to standard output, but also handles both end of input and
    broken pipe errors:

> $ ./echo2
> Test<Enter>
> Test
> ABC<Enter>
> ABC
> 42<Enter>
> 42
> <Ctrl-D>
> $

    ('>->') matches every 'await' in the 'Consumer' with a 'yield' in the
    'Producer'.  Every time 'P.stdout' awaits a 'String' it transfers control
    to 'P.stdin' and every time 'P.stdin' yields a 'String' it transfers control
    back to 'P.stdout'.

    Streaming stops when either 'P.stdin' terminates (i.e. end of input) or
    'P.stdout' terminates (i.e. broken pipe).  This is why ('>->') requires that
    both the 'Producer' and 'Consumer' share the same type of return value:
    whichever one terminates first provides the return value for the entire
    'Effect'.

    Let's test this by modifying our 'Producer' and 'Consumer' to each return a
    diagnostic 'String':

> -- echo3.hs
>
> import Control.Applicative ((<$))  -- (<$) modifies return values
> import Pipes
> import qualified Pipes.Prelude as P
> import System.IO
>
> main = do
>     hSetBuffering stdout NoBuffering
>     str <- run $ ("End of input!" <$ P.stdin) >-> ("Broken pipe!" <$ P.stdout)
>     hPutStrLn stderr str

    This lets us diagnose whether the 'Producer' or 'Consumer' terminated first:

> $ ./echo3
> Test<Enter>
> Test
> <Ctrl-D>
> End of input!
> $ ./echo3 | perl -e 'close STDIN'
> Test<Enter>
> Broken pipe!
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
> --              +--------- A 'Pipe' that
> --              |    +---- 'await's 'a's and
> --              |    | +-- 'yield's 'a's
> --              |    | |
> --              v    v v
> take ::  Int -> Pipe a a IO ()
> take n = replicateM_ n $ do
>     x <- await                             -- 'await' a value of type 'a'
>     yield x                                -- 'yield' a value of type 'a'
>     lift $ putStrLn "You shall not pass!"  -- Fly, you fools!

    You can use 'Pipe's to transform 'Producer's, 'Consumer's, or even other
    'Pipe's using the same ('>->') operator:

@
 ('>->') :: 'Monad' m => 'Producer' a m r -> 'Pipe'   a b m r -> 'Producer' b m r
 ('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Consumer' b m r -> 'Consumer' a m r
 ('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Pipe'   b c m r -> 'Pipe'   a c m r
@

    For example, you can compose 'P.take' after 'P.stdin' to limit the number of
    lines drawn from standard input:

> maxInput :: Int -> Producer String IO ()
> maxInput n = P.stdin >-> take n

>>> run $ maxInput 3 >-> P.stdout
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
You shall not pass!
>>>

    ... or you can pre-compose 'P.take' before 'P.stdout' to limit the number of
    lines written to standard output:

> maxOutput :: Int -> Consumer String IO ()
> maxOutput n = take n >-> P.stdout

>>> run $ P.stdin >-> maxOutput 3
<Exact same behavior>

    Those both gave the same behavior because ('>->') is associative:

> (p1 >-> p2) >-> p3 = p1 >-> (p2 >-> p3)

    Therefore we can just leave out the parentheses:

>>> run $ P.stdin >-> take 3 >-> P.stdout
<Exact same behavior>

    ('>->') is designed to behave like the Unix pipe operator, except with less
    quirks.  In fact, ('>->') also has an identity named 'cat' (named after the
    Unix @cat@ utility), which reforwards elements endlessly:

> cat :: (Monad m) => Pipe a a m r
> cat = forever $ do
>     x <- await
>     yield x

    Therefore, ('>->') and 'cat' form a 'Category', specifically the category of
    Unix pipes:

> -- Useless use of 'cat'
> cat >-> p = p
>
> -- Forwarding output to 'cat' does nothing
> p >-> cat = p

    A lot of Unix tools have very simple definitions when written using @pipes@:

> -- unix.hs
>
> import Control.Monad (forever)
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude provides 'take', too
> import Prelude hiding (head)
>
> head :: (Monad m) => Pipe a a m ()
> head = P.take
>
> yes :: (Monad m) => Producer String m r
> yes = forever $ yield "y"
>
> main = run $ yes >-> head 3 >-> P.stdout

    This prints out 3 \'@y@\'s, just like the equivalent Unix pipeline:

> $ ./unix
> y
> y
> y
> $ yes | head -3
> y
> y
> y
> $

    This lets us write \"Haskell pipes\" instead of Unix pipes.  These are much
    easier to build than Unix pipes and we can connect them directly within
    Haskell for interoperability with the Haskell language and ecosystem.
-}

{- $listT
    @pipes@ also provides a \"ListT done right\" implementation.

    To bind a list within a 'ListT' computation, combine 'Select' and 'each':

> import Pipes
> 
> pair :: ListT IO (Int, Int)
> pair = do
>     x <- Select $ each [1, 2]
>     lift $ putStrLn $ "x = " ++ show x
>     y <- Select $ each [3, 4]
>     lift $ putStrLn $ "y = " ++ show y
>     return (x, y)

    You can then loop over a 'ListT' by using 'for' and 'every':

@
 'every' :: 'Monad' m => 'ListT' m a -> 'Producer' a m ()
@

>>> run $ for (every pair) (lift . print)
x = 1
y = 3
(1,3)
y = 4
(1,4)
x = 2
y = 3
(2,3)
y = 4
(2,4)

    You can also go the other way, too, binding 'Producer's directly within a
    'ListT'.  In fact, this is actually what 'Select' was already doing:

> Select :: Producer a m () -> ListT m a

    This lets you write crazy code like:

> import Pipes
> import qualified Pipes.Prelude as P
> 
> quitter :: Producer String IO ()
> quitter = P.stdin >-> P.takeWhile (/= "quit")
> 
> pairs :: ListT IO String
> pairs = do
>     str1 <- Select quitter
>     str2 <- Select quitter
>     return (str1 ++ " " ++ str2)

    Here we're binding standard input non-deterministically as if it were an
    effectful list:

>>> run $ every pairs >-> P.stdout
Daniel<Enter>
Fischer<Enter>
Daniel Fischer
Wagner<Enter>
Daniel Wagner
quit<Enter>
Donald<Enter>
Stewart<Enter>
Donald Stewart
Duck<Enter>
Donald Duck
quit<Enter>
quit<Enter>
>>>

    Notice how this streams out values immediately as they are generated, rather
    than building up a large intermediate result and then printing all the
    values in one batch at the end.
-}

{- $tricks
    @pipes@ is more powerful than meets the eye so this section presents some
    non-obvious tricks you may find useful.

    For example, here's the definition of 'P.map' from "Pipes.Prelude":

> map :: (Monad m) => (a -> b) -> Pipe a b m r
> map f = for cat $ \a -> yield (f a)

    Notice how 'P.map' uses 'for' to loop over 'cat', which is a 'Pipe' instead
    of a 'Producer'.  This is because 'for' has the following even more general
    type:

@
 'for' :: 'Monad' m => 'Pipe' x b m r -> (b -> 'Producer' c m ()) -> 'Pipe' x c m r
@

    We can use 'for' to loop over the output of a 'Pipe' the same way we loop
    over the output of a 'Producer'.

    In the above code, 'cat' is the empty pipe that transmits all values
    downstream, so you can literally read the above code as saying: \"For all
    values flowing downstream, re-yield them after applying the function @f@\".

    You can use the dual trick for ('>~'), too:

> threeYs :: (Monad m) => Producer String m ()
> threeYs = return "y" >~ P.take 3

    Or what if you want to print all elements from a triply-nested list:

>>> run $ (each ~> each ~> each ~> lift . print) [[[1,2],[3,4]],[[5,6],[7,8]]]
1
2
3
4
5
6
7
8

    Another useful trick is to compose pipes within a pipe:

> customerService :: Producer String IO ()
> customerService = do
>     each ["Hello, how can I help you?", "Hold for one second."]
>     P.stdin >-> P.takeWhile (/= "Goodbye!")

    Or you can simulate a Scala-like 'for' loop (for the special case of
    collections) using 'ListT':

> run $ for (every $ do
>     i <- Select $ each [1..10]
>     j <- Select $ each [1..10]
>     return (i, j) ) (lift . print)

-}

{- $conclusion
    This tutorial covers the core concepts of connecting, building, and reading
    @pipes@ code.  However, this library is only the core component in an
    ecosystem of streaming components.  More powerful libraries that build upon
    @pipes@ include:

    * @pipes-concurrency@: Concurrent reactive programming and message passing

    * @pipes-parse@: Minimal utilities for stream parsing

    * @pipes-safe@: Resource management and exception safety for @pipes@

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
    @pipes@ functionality, which actually permits bidirectional communication.
    Advanced @pipes@ users can explore this library in greater detail by
    studying the documentation in the "Pipes.Core" module to learn about the
    symmetry of the underlying 'Proxy' type and operators.

    To learn more about @pipes@, ask questions, or follow @pipes@ development,
    you can subscribe to the @haskell-pipes@ mailing list at:

    <https://groups.google.com/forum/#!forum/haskell-pipes>

    ... or you can mail the list directly at
    <mailto:haskell-pipes@googlegroups.com>.

    Additionally, for questions regarding types or type errors, you might find
    the following appendix on types very useful.
-}

{- $types
    @pipes@ uses parametric polymorphism (i.e. generics) to overload all
    operations.  You've probably noticed this overloading already::

    * 'yield' works within both 'Producer's and 'Pipe's

    * 'await' works within both 'Consumer's and 'Pipe's

    * ('>->') connects 'Producer's, 'Consumer's, and 'Pipe's in varying ways

    This overloading is great when it works, but when connections fail they
    produce type errors that appear intimidating at first.  This section
    explains the underlying types so that you can work through type errors
    intelligently.

    'Producer's, 'Consumer's, 'Pipe's, and 'Effect's are all special cases of a
    single underlying type: a 'Proxy'.  This overarching type permits fully
    bidirectional communication on both an upstream and downstream interface.
    You can think of it as having the following shape:

> Proxy a' a b' b m r
>
> Upstream | Downstream
>     +---------+
>     |         |
> a' <==       <== b'  -- Information flowing upstream
>     |         |
> a  ==>       ==> b   -- Information flowing downstream
>     |    |    |
>     +----|----+
>          v
>          r

    The four core types do not use the upstream flow of information.  This means
    that the @a'@ and @b'@ in the above diagram go unused unless you use the
    more advanced features provided in "Pipes.Core".

    @pipes@ uses type synonyms to hide unused inputs or outputs and clean up
    type signatures.  These type synonyms come in two flavors:

    * Concrete type synonyms that explicitly close unused inputs and outputs of
      the 'Proxy' type

    * Polymorphic type synonyms that don't explicitly close unused inputs or
      outputs

    The concrete type synonyms use @()@ to close unused inputs and 'X' (the
    uninhabited type) to close unused outputs:

    * 'Effect': explicitly closes both ends, forbidding 'await's and 'yield's

> type Effect = Proxy X () () X
>
> Upstream | Downstream
>     +---------+
>     |         |
> X  <==       <== ()
>     |         |
> () ==>       ==> X
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Producer': explicitly closes the upstream end, forbidding 'await's

> type Producer b = Proxy X () () b
>
> Upstream | Downstream
>     +---------+
>     |         |
> X  <==       <== ()
>     |         |
> () ==>       ==> b
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Consumer': explicitly closes the downstream end, forbidding 'yield's

> type Consumer a = Proxy () a () X
>
> Upstream | Downstream
>     +---------+
>     |         |
> () <==       <== ()
>     |         |
> a  ==>       ==> X
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Pipe': marks both ends open, allowing both 'await's and 'yield's

> type Pipe a b = Proxy () a () b
>
> Upstream | Downstream
>     +---------+
>     |         |
> () <==       <== ()
>     |         |
> a  ==>       ==> b
>     |    |    |
>     +----|----+
>          v
>          r

    When you compose 'Proxy's using ('>->') all you are doing is placing them
    side by side and fusing them laterally.  For example, when you compose a
    'Producer', 'Pipe', and a 'Consumer', you can think of information flowing
    like this:

>      Producer                Pipe              Consumer
>     +---------+          +---------+          +---------+
>     |         |          |         |          |         |
> X  <==       <==   ()   <==       <==   ()   <==       <== ()
>     |  stdin  |          | take 3  |          |  stdout |
> () ==>       ==> String ==>       ==> String ==>       ==> X
>     |    |    |          |    |    |          |    |    |
>     +----|----+          +----|----+          +----|----+
>          v                    v                    v
>          ()                   ()                   ()

     Composition fuses away the intermediate interfaces, leaving behind an
     'Effect':

>                   Effect
>     +-------------------------------+
>     |                               |
> X  <==                             <== ()
>     |  stdin >-> take 3 >-> stdout  |
> () ==>                             ==> X
>     |                               |
>     +---------------|---------------+
>                     v
>                     ()
>

    @pipes@ also provides polymorphic type synonyms with apostrophes at the end
    of their names.  These use universal quantification to leave open any unused
    input or output ends (which I mark using @*@):

    * 'Producer'': marks the upstream end unused but still open

> type Producer' b m r = forall x' x . Proxy x' x () b m r
>
> Upstream | Downstream
>     +---------+
>     |         |
>  * <==       <== ()
>     |         |
>  * ==>       ==> b
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Consumer'': marks the downstream end unused but still open

> type Consumer' a m r = forall y' y . Proxy a () y' y m r
>
> Upstream | Downstream
>     +---------+
>     |         |
> a  <==       <== * 
>     |         |
> () ==>       ==> *
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Effect'': marks both ends unused but still open

> type Effect' a m r = forall x' x y' y . Proxy x' x y' y m r
>
> Upstream | Downstream
>     +---------+
>     |         |
>  * <==       <== * 
>     |         |
>  * ==>       ==> *
>     |    |    |
>     +----|----+
>          v
>          r

    Note that there is no polymorphic generalization of a 'Pipe'.

    Like before, if you compose a 'Producer'', a 'Pipe', and a 'Consumer'':

>      Producer'               Pipe              Consumer'
>     +---------+          +---------+          +---------+
>     |         |          |         |          |         |
>  * <==       <==   ()   <==       <==   ()   <==       <== *
>     |  stdin  |          | take 3  |          |  stdout |
>  * ==>       ==> String ==>       ==> String ==>       ==> *
>     |    |    |          |    |    |          |    |    |
>     +----|----+          +----|----+          +----|----+
>          v                    v                    v
>          ()                   ()                   ()

    ... they fuse into an 'Effect'':

>                   Effect'
>     +-------------------------------+
>     |                               |
>  * <==                             <== *
>     |  stdin >-> take 3 >-> stdout  |
>  * ==>                             ==> *
>     |                               |
>     +---------------|---------------+
>                     v
>                     ()
>

    Polymorphic type synonyms come in handy when you want to keep the type as
    general as possible.  For example, the type signature for 'yield' uses
    'Producer'' to keep the type signature simple while still leaving the
    upstream input end open:

@
 'yield' :: 'Monad' m => a -> 'Producer'' a m ()
@

    This type signature lets us use 'yield' within a 'Pipe', too, because the
    'Pipe' type synonym is a special case of the polymorphic 'Producer'' type 
    synonym:

@
 type 'Producer'' b m r = forall x' x . 'Proxy' x' x () b m r
 type 'Pipe'    a b m r =               'Proxy' () a () b m r
@

    The same is true for 'await', which uses the polymorphic 'Consumer'' type
    synonym:

@
 'await' :: 'Monad' m => 'Consumer'' a m a
@

    We can use 'await' within a 'Pipe' because a 'Pipe' is a special case of the
    polymorphic 'Consumer'' type synonym:

@
 type 'Consumer'' a   m r = forall y' y . 'Proxy' () a y' y m r
 type 'Pipe'      a b m r =               'Proxy' () a () b m r
@

    However, polymorphic type synonyms cause problems in many other cases:

    * They induce higher-rank types and require you to enable the @RankNTypes@
      extension to use them in your own type signatures:

    * They give the wrong behavior when used in the negative position of a
      function like this:

> f :: Producer' a m r -> ...  -- Wrong
>
> f :: Producer  a m r -> ...  -- Right

    * You can't use them within other types without the @ImpredicativeTypes@
      extension:

> io :: IO (Producer' a m r)  -- Type error

    * You can't partially apply them:

> stack :: MaybeT (Producer' a m) r  -- Type error

    In these scenarios you should fall back on the concrete type synonyms, which
    are better behaved.

    For the purposes of debugging type errors you can just remember that:

>  Input --+    +-- Output
>          |    |
>          v    v
> Proxy a' a b' b m r
>       ^    ^
>       |    |
>       +----+-- Ignore these

    For example, let's say that you try to 'run' the 'P.stdin' 'Producer'.  This
    produces the following type error:

>>> run P.stdin
<interactive>:4:5:
    Couldn't match expected type `X' with actual type `String'
    Expected type: Effect m0 r0
      Actual type: Proxy X () () String IO ()
    In the first argument of `run', namely `P.stdin'
    In the expression: run P.stdin

    'run' expects an 'Effect', which is equivalent to the following type:

> Effect          IO () = Proxy X () () X      IO ()

    ... but 'P.stdin' type-checks as a 'Producer', which has the following type:

> Producer String IO () = Proxy X () () String IO ()

    The fourth type variable (the output) does not match.  For an 'Effect' this
    type variable should be closed (i.e. 'X'), but 'P.stdin' has a 'String'
    output, thus the type error:

>    Couldn't match expected type `X' with actual type `String'

    Any time you get type errors like these you can work through them by
    expanding out the type synonyms and seeing which type variables do not
    match.
-}
