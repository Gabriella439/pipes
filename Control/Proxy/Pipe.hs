{-| This module provides an API similar to "Control.Pipe".

    "Control.Pipe.Core" provides the central, more extensive, documentation and
    "Control.Pipe.Tutorial" describes how to use pipes fluently.

    This module differs from "Control.Pipe" in important ways in order to
    encourage seamless interoperability between unidirectional 'Pipe' code and
    bidirectional 'Proxy' code.  See the \"Upgrade Pipes to Proxies\" section
    for details. -}
module Control.Proxy.Pipe (
    -- * Create Pipes
    await,
    yield,
    pipe,

    -- * Compose Pipes
    (<+<),
    (>+>),
    idP,

    -- * Run Pipes
    -- $run

    -- * Upgrade Pipes to Proxies
    -- $upgrade
    ) where

import Control.Proxy.Class
import Control.Proxy.Synonym (Pipe)

{-| Wait for input from upstream

    'await' blocks until input is available -}
await :: (Monad m, ProxyP p) => Pipe p a b m a
await = request ()

-- | Convert a pure function into a pipe
pipe :: (Monad m, ProxyP p) => (a -> b) -> Pipe p a b m r
pipe f = go where
    go =
        request ()    ?>= \a  ->
        respond (f a) ?>= \_ ->
        go

{-| Deliver output downstream

    'yield' restores control back downstream and binds the result to 'await'. -}
yield :: (Monad m, ProxyP p) => b -> Pipe p a b m ()
yield = respond

infixr 9 <+<
infixl 9 >+>

-- | Corresponds to ('<<<')/('.') from @Control.Category@
(<+<)
 :: (Monad m, ProxyP p) => Pipe p b c m r -> Pipe p a b m r -> Pipe p a c m r
p1 <+< p2 = ((\() -> p1) <-< (\() -> p2)) ()

-- | Corresponds to ('>>>') from @Control.Category@
(>+>)
 :: (Monad m, ProxyP p) => Pipe p a b m r -> Pipe p b c m r -> Pipe p a c m r
(>+>) = flip (<+<)

-- | Corresponds to 'id' from @Control.Category@
idP :: (Monad m, ProxyP p) => Pipe p a a m r
idP = go where
    go =
        request () ?>= \a  ->
        respond a  ?>= \() ->
        go

{- $run
    The "Control.Proxy.Core.Fast" and "Control.Proxy.Core.Correct" modules
    provide their corresponding 'runPipe' functions, specialized to their own
    'Proxy' implementations.

    Each implementation must supply its own 'runPipe' functions since it is
    the only non-polymorphic 'Pipe' function and the compiler uses it to
    select which underlying 'Proxy' implementation to use. -}

{- $upgrade
    You can upgrade code written to 'Control.Pipe' to work with the 'Proxy'
    ecosystem in stages.  Each change enables greater interoperability with
    proxy utilities and transformers and if time permits you should implement
    the entire upgrade for your libraries if you want to take advantage of
    proxy standard libraries.

    First, import "Control.Proxy" and "Control.Proxy.Pipe" instead of
    "Control.Pipe" and add 'Proxy' after every 'Pipe', 'Producer', or 'Consumer'
    in any type signature.  For example, you would convert this:

> import Control.Pipe
>
> fromList :: (Monad m) => [b] -> Producer b m r

    ... to this:

> import Control.Proxy
> import Control.Proxy.Pipe -- transition import
>
> fromList :: (Monad m) => [b] -> Producer Proxy b m r

    The change ensures that all your code now works in the 'Proxy' monad.

    Second, modify all your 'Pipe's to take an empty '()' as their final
    argument, and translate the following functions:

    * ('<+<') to ('<-<')

    * 'runPipe' to 'runProxy'

    For example, you would convert this:

> import Control.Proxy
> import Control.Proxy.Pipe
>
> fromList :: (Monad m) => [b] -> Producer Proxy b IO r
> fromList xs = mapM_ yield xs

    ... to this:

> import Control.Proxy
> import Control.Proxy.Pipe
>
> fromList :: (Monad m) => [b] -> () -> Producer Proxy b IO r
> fromList xs () = mapM_ yield xs

    You then call these within a @do@ block by supplying the '()' parameter:

> example = do
>     a <- request ()
>     fromList [1..a] ()

    This change lets you switch from pipe composition, ('<+<'), to proxy
    composition, ('<-<').

    Third, wrap your pipe's implementation in 'runIdentityP' (which
    "Control.Proxy" exports) and you can now drop the "Control.Proxy.Pipe"
    import:

> import Control.Proxy
> import Control.Proxy.Pipe
>
> fromList xs () = runIdentityP $ mapM_ yield xs

    The inferred type signature will now be polymorphic in the first type
    parameter:

> fromList :: (Monad m, ProxyP p) => [b] -> () -> Producer p b m r

    This change upgrades your 'Pipe' to work natively within proxies and proxy
    transformers, without any manual conversion or lifting.  You can now compose
    or sequence your 'Pipe' within any feature set transparently.

    Finally, replace each 'await' with @request ()@ and each 'yield' with
    'respond'.  This lets you drop the "Control.Proxy.Pipe" import:

> import Control.Proxy
>
> fromList :: (Monad m, ProxyP p) => [b] -> () -> Producer p b m r
> fromList xs () = runIdentityP $ mapM_ respond xs

    Also, continue to use the 'Pipe', 'Consumer' and 'Producer' type synonyms to
    simplify type signatures.  I encourage using them for unidirectional proxy
    code.  Here are some examples:

> import Control.Proxy
> import Control.Proxy.Trans.Either as E
> import Control.Proxy.Trans.State
>
> -- A Producer enriched with pipe-local state
> example1 :: (Monad m, ProxyP p) => () -> Producer (StateP Int p) Int m r
> example1 () = forever $ do
>     n <- get
>     respond n
>     put (n + 1)
>
> -- A Consumer enriched with error-handling
> example2 :: (ProxyP p) => () -> Consumer (EitherP String p) Int IO ()
> example2 () = do
>     n <- request ()
>     if (n == 0)
>         then E.throw "Error: received 0"
>         else lift $ print n

-}
