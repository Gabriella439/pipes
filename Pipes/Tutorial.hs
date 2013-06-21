{-| @pipes@ is an easy-to-use, powerful, and elegant stream processing library 

    Use @pipes@ to build and connect reusable components like Unix pipes.
-}

module Pipes.Tutorial (
    -- * Easy to use
    -- $easytouse

    -- * Easy to build
    -- $easytobuild

    -- * Easy to understand
    -- $easytounderstand

    -- * Types - Part 1
    -- $types1

    -- * Prelude
    -- $prelude

    -- * Types - Part 2
    -- $types2

    -- * Folds
    -- $folds
    ) where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
import Pipes
import qualified Pipes.Prelude as P

{- $easytouse
    Haskell pipes are simple to connect.  For example, here's how you echo
    'P.stdin' to 'P.stdout', just like @cat@ when given no arguments:

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
> ^D
> $ ./cat <cat.hs
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.stdin >-> P.stdout) ()
> $

    You can 'P.take' 10 lines of input to emulate the @head@ utility:

> -- head.hs
> 
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.stdin >-> P.take 10 >-> P.stdout) ()

    ... or you can simulate the @yes@ command by replacing 'P.stdin' with an
    endless list of @"y"@s:

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

    This gives the same behavior, but with the information passing done directly
    within Haskell:

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

    'P.stdin' is only slightly more complicated.  We must also remember to check
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
    These components differ from Unix pipes because they are not limited to
    text-based inputs or outputs.
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

{- $types1
    You might wonder why these pipes all take a @()@ argument.  This is because
    'Consumer's, 'Producer's, and 'Pipe's are all special cases of fully
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

    However, you will get a type error if you compose it directly with
    'P.stdout' or 'P.print':

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
    all our pipes agree on using 'ErrorT':

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

{- $folds
    The @pipes@ Prelude provides several folds which store their results in a
    'WriterT' layer in the base monad.  For example, if you want to count how
    many lines of input like the @wc@ command, you use the 'P.length' fold:

> length :: (Monad m) => () -> Consumer a (WriterT (Sum Int) m) r

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
-}

{- $types3
    Notice how we return a result from the @wc@ pipeline.
    Just follow the types to see why this works:

> hoist lift . P.stdin
>     :: (Monad m) => () -> Producer String (WriterT (Sum Int) IO) ()

    'P.length' has a polymorphic return value, @r@, because it never terminates,
    so @r@ will type-check as @()@ when we compose these two pipes:

> hoist lift . P.stdin >-> 
-}
