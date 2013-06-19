{-| @pipes@ is an easy-to-use, powerful, and elegant stream processing library 

    Use @pipes@ to build and connect reusable streaming components like Unix
    pipes.
-}

module Pipes.Tutorial (
    -- * Easy to use
    -- $easytouse

    -- * Easy to build
    -- $easytobuild

    -- * Easy to understand
    -- $easytounderstand

    -- * Types
    -- $types
    ) where

import Pipes
import qualified Pipes.Prelude as P

{- $easytouse
    Here's how you echo 'P.stdin' to 'P.stdout', just like @cat@ when given no
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
> ^D
> $ ./cat <cat.hs
> import Pipes
> import qualified Pipes.Prelude as P
> 
> main = runEffect $ (P.stdin >-> P.stdout) ()
> $

    You can 'P.take' 10 lines of input to emulate the @head@ utility:

> main = runEffect $ (P.stdin >-> P.take 10 >-> P.stdout) ()

    ... or you can simulate the @yes@ command by replacing 'P.stdin' with an
    endless list of @"y"@s:

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

> main = runEffect $ (P.fromList (repeat "y") >-> P.take 10 >-> P.stdout) ()
-}

{- $easytobuild
    We can easily implement all of the above utilities ourselves.  Here's the
    code for 'P.stdout':

> import Control.Monad
> import Pipes
>
> stdout :: () -> Consumer String IO r
> stdout () = forever $ do
>     str <- request ()
>     lift $ putStrLn str

    'Consumer' is a monad transformer that extends the base monad with the
    ability to 'request' new input.  We 'request' values of type 'String', so
    'P.stdout' is a 'Consumer' of 'String's.

    Notice that 'P.stdout' returns a polymorphic return value, @r@, because
    'P.stdout' never terminates.

    'P.stdin' is easy to define, too, as long as we remember to check for the
    end of the input.

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

    'Producer' is a monad transformer that extends the base monad with the
    ability to 'respond' with new output.  We 'respond' with values of type
    'String', so 'P.stdin' is a 'Producer' of 'String's.

    The last component is 'P.take' which only transmits a fixed number of
    values:

> take :: Int -> () -> Pipe a a IO ()
> take n () = replicateM_ n $ do
>     a <- request ()
>     respond a

    'Pipe' is a monad transformer that extends the base monad with the ability
    to both 'request' new input and 'respond' with new output.  We 'request'
    values of any type, @a@, and 'respond' with values of the same type, @a@.

    Notice how 'take' can transmit values of any type, not just 'String's.
    These components are not limited to text-based protocols like Unix
    pipelines.
-}

{- $easytounderstand
    You connect pipes using the ('>->') composition operator.

    If you connect a 'Producer' and a 'Consumer', you get an 'Effect':

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

    You can only run 'Effect's, using 'runEffect':

> runEffect :: Effect m r -> m r

    This improves upon the traditional Unix pipe operator because the compiler
    will guarantee that:

    * we don't connect pipes with mismatched types,

    * we don't connect an open end to a closed end, and

    * we don't leave a dangling input or output end.
-}

{- $types
    You might wonder why these pipes all take a @()@ argument.  This is because
    'Consumer's, 'Producer's, and 'Pipe's are all special cases of fully
     bidirectional 'Proxy's, and the ('>->') uses this initial argument in the
    general case:

> (>->) :: (b' -> Proxy a' a b' b m r) 
>       -> (c' -> Proxy b' b c' c m r)
>       -> (c' -> Proxy a' a b' b m r)

    'Consumer's, 'Producer's, and 'Pipe's are all type synonyms around the
    'Proxy' type, which is why you can reuse ('>->') to connect all of them:

> type Consumer a m r = forall y' y . Proxy () a y' y m r
> type Producer b m r = forall x' x . Proxy x' x () b m r
> type Pipe   a b m r =               Proxy () a () b m r

    See the advanced section on bidirectionality if you want to learn more.
    Otherwise, just remember that your pipes require an argument of type @()@ if
    you stick to composing unidirectional pipes using ('>->').
-}
