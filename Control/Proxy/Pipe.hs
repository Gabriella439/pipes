{-# LANGUAGE KindSignatures #-}

{-| This module provides an API similar to "Control.Pipe" for those who prefer
    the classic 'Pipe' API.

    This module differs slightly from "Control.Pipe" in order to promote
    seamless interoperability with both pipes and proxies.  See the \"Upgrade
    Pipes to Proxies\" section below for details. -}
module Control.Proxy.Pipe (
    -- * Create Pipes
    await,
    yield,
    pipe,

    -- * Compose Pipes
    (<+<),
    (>+>),
    idP,

    -- * Synonyms
    Pipeline,

    -- * Run Pipes
    -- $run

    -- * Upgrade Pipes to Proxies
    -- $upgrade
    ) where

import Control.Monad (forever)
import Control.Proxy.Class (Proxy(request, respond, (>->), (?>=)))
import Control.Proxy.Synonym (Pipe, Consumer, Producer, C)
import Control.Proxy.Trans.Identity (runIdentityP)

{-| Wait for input from upstream

    'await' blocks until input is available from upstream. -}
await :: (Monad m, Proxy p) => Pipe p a b m a
await = request ()

{-| Deliver output downstream

    'yield' restores control back downstream and binds its value to 'await'. -}
yield :: (Monad m, Proxy p) => b -> p a' a b' b m ()
yield b = runIdentityP $ do
    respond b
    return ()

-- | Convert a pure function into a pipe
pipe :: (Monad m, Proxy p) => (a -> b) -> Pipe p a b m r
pipe f = runIdentityP $ forever $ do
    a <- request ()
    respond (f a)

infixr 9 <+<
infixl 9 >+>

-- | Corresponds to ('<<<')/('.') from @Control.Category@
(<+<)
 :: (Monad m, Proxy p) => Pipe p b c m r -> Pipe p a b m r -> Pipe p a c m r
p1 <+< p2 = p2 >+> p1

-- | Corresponds to ('>>>') from @Control.Category@
(>+>)
 :: (Monad m, Proxy p) => Pipe p a b m r -> Pipe p b c m r -> Pipe p a c m r
p1 >+> p2 = ((\() -> p1) >-> (\() -> p2)) ()

-- | Corresponds to 'id' from @Control.Category@
idP :: (Monad m, Proxy p) => Pipe p a a m r
idP = runIdentityP $ forever $ do
    a <- request ()
    respond a

{-| A self-contained 'Pipeline' that is ready to be run

    'Pipeline's never 'request' nor 'respond'. -}
type Pipeline (p :: * -> * -> * -> * -> (* -> *) -> * -> *) = p C () () C

{- $run
    The "Control.Proxy.Core.Fast" and "Control.Proxy.Core.Correct" modules
    provide their corresponding 'runPipe' functions, specialized to their own
    'Proxy' implementations.

    Each implementation must supply its own 'runPipe' function since it is
    the only non-polymorphic 'Pipe' function and the compiler uses it to
    select which underlying proxy implementation to use. -}

{- $upgrade
    You can upgrade classic 'Pipe' code to work with the proxy ecosystem in
    steps.  Each change enables greater interoperability with proxy utilities
    and transformers and if time permits you should implement the entire upgrade
    for your libraries if you want to take advantage of proxy standard
    libraries.

    First, import "Control.Proxy" and "Control.Proxy.Pipe" instead of
    "Control.Pipe".  Then, add 'ProxyFast' after every 'Pipe', 'Producer', or
    'Consumer' in any type signature.  For example, you would convert this:

> import Control.Pipe
>
> fromList :: (Monad m) => [b] -> Producer b m ()
> fromList xs = mapM_ yield xs

    ... to this:

> import Control.Proxy
> import Control.Proxy.Pipe -- transition import
>
> fromList :: (Monad m) => [b] -> Producer ProxyFast b m ()
> fromList xs = mapM_ yield xs

    The change ensures that all your code now works in the 'ProxyFast' monad,
    which is the faster of the two proxy implementations.

    Second, modify all your 'Pipe's to take an empty '()' as their final
    argument, and translate the following functions:

    * ('<+<') to ('<-<')

    * 'runPipe' to 'runProxy'

    For example, you would convert this:

> import Control.Proxy
> import Control.Proxy.Pipe
>
> fromList :: (Monad m) => [b] -> Producer ProxyFast b m ()
> fromList xs = mapM_ yield xs

    ... to this:

> import Control.Proxy
> import Control.Proxy.Pipe
>
> fromList :: (Monad m) => [b] -> () -> Producer ProxyFast b m ()
> fromList xs () = mapM_ yield xs

    Now when you call these within a @do@ block  you must supplying an
    additional @()@ argument:

> examplePipe () = do
>     a <- request ()
>     fromList [1..a] ()

    This change lets you switch from pipe composition, ('<+<'), to proxy
    composition, ('<-<'), so that you can mix proxy utilities with pipes.

    Third, wrap your pipe's implementation in 'runIdentityP' (which
    "Control.Proxy" exports):

> import Control.Proxy
> import Control.Proxy.Pipe
>
> fromList xs () = runIdentityP $ mapM_ yield xs

    Then replace the 'ProxyFast' in the type signature with a type variable @p@
    constrained by the 'Proxy' type class:

> fromList :: (Monad m, Proxy p) => [b] -> () -> Producer p b m ()

    This change upgrades your 'Pipe' to work natively within proxies and proxy
    transformers, without any manual conversion or lifting.  You can now compose
    or sequence your 'Pipe' within any feature set transparently.

    Finally, replace each 'await' with @request ()@ and each 'yield' with
    'respond'.  Also, replace every 'Pipeline' with 'Session'.  This lets you
    drop the "Control.Proxy.Pipe" import:

> import Control.Proxy
>
> fromList :: (Monad m, Proxy p) => [b] -> () -> Producer p b m ()
> fromList xs () = runIdentityP $ mapM_ respond xs

    Also, I encourage you to continue using the 'Pipe', 'Consumer' and
    'Producer' type synonyms to simplify type signatures.  The following
    examples show how they cleanly mix with proxies and their extensions:

> import Control.Proxy
> import Control.Proxy.Trans.Either as E
> import Control.Proxy.Trans.State
>
> -- A Producer enriched with pipe-local state
> example1 :: (Monad m, Proxy p) => () -> Producer (StateP Int p) Int m r
> example1 () = forever $ do
>     n <- get
>     respond n
>     put (n + 1)
>
> -- A Consumer enriched with error-handling
> example2 :: (Proxy p) => () -> Consumer (EitherP String p) Int IO ()
> example2 () = do
>     n <- request ()
>     if (n == 0)
>         then E.throw "Error: received 0"
>         else lift $ print n

-}
