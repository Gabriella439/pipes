{-# LANGUAGE Rank2Types #-}

{-| This module provides an API compatible with "Control.Pipe"

    Consult "Control.Pipe.Core" for more extensive documentation and
    "Control.Pipe.Tutorial" for an extended tutorial. -}

module Control.Proxy.Pipe (
    -- * Types
    Pipe,
    Producer,
    Consumer,
    Pipeline,
    -- * Create Pipes
    await,
    yield,
    pipe,
    -- * Compose Pipes
    (<+<),
    (>+>),
    idP,
    -- * Run Pipes
    runPipe
    ) where

import Control.Proxy.Core
import Control.Proxy.Class
import Data.Closed (C)

{-| The type variables of @Pipe a b m r@ signify:

    * @a@ - The type of input received from upstream pipes

    * @b@ - The type of output delivered to downstream pipes

    * @m@ - The base monad

    * @r@ - The type of the return value -}
type Pipe   a b m r = Proxy () a () b m r

-- | A pipe that produces values
type Producer b m r = forall a   . Pipe a b m r

-- | A pipe that consumes values
type Consumer a m r = forall   b . Pipe a b m r

-- | A self-contained pipeline that is ready to be run
type Pipeline   m r = forall a b . Pipe a b m r

{-| Wait for input from upstream

    'await' blocks until input is available -}
await :: (Monad m) => Pipe a b m a
await = request ()
{-# INLINE await #-}

-- | Convert a pure function into a pipe
pipe :: (Monad m) => (a -> b) -> Pipe a b m r
pipe f = go where
    go = Request () (\a -> Respond (f a) (\() -> go))

{-| Deliver output downstream

    'yield' restores control back downstream and binds the result to 'await'. -}
yield :: (Monad m) => b -> Pipe a b m ()
yield = respond
{-# INLINE yield #-}

infixr 9 <+<
infixl 9 >+>

-- | Corresponds to ('<<<')/('.') from @Control.Category@
(<+<) :: (Monad m) => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p1 <+< p2 = ((\() -> p1) <-< (\() -> p2)) ()

-- | Corresponds to ('>>>') from @Control.Category@
(>+>) :: (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
(>+>) = flip (<+<)

-- | Corresponds to 'id' from @Control.Category@
idP :: (Monad m) => Pipe a a m r
idP = go where
    go = Request () (\a -> Respond a (\() -> go))

-- | Run the 'Pipe' monad transformer, converting it back to the base monad
runPipe :: (Monad m) => Pipeline m r -> m r
runPipe p' = go p' where
    go p = case p of
        Request _ fa  -> go (fa  ())
        Respond _ fb' -> go (fb' ())
        M         m   -> m >>= go
        Pure      r   -> return r
