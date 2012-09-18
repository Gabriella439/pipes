{-| This module provides an API compatible with "Control.Pipe"

    Consult "Control.Pipe" for more extensive documentation and
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

import Control.Monad (forever)
import Control.Monad.Trans.Free
import Control.Proxy.Core
import Control.Proxy.Class
import Data.Void (Void)

{-| The type variables of @Pipe a b m r@ signify:

    * @a@ - The type of input received from upstream pipes

    * @b@ - The type of output delivered to downstream pipes

    * @m@ - The base monad

    * @r@ - The type of the return value -}
type Pipe   a b = Proxy () a () b

-- | A pipe that produces values
type Producer b = Pipe ()    b

-- | A pipe that consumes values
type Consumer a = Pipe  a Void

-- | A self-contained pipeline that is ready to be run
type Pipeline   = Pipe () Void

{-| Wait for input from upstream

    'await' blocks until input is available -}
await :: (Monad m) => Pipe a b m a
await = request ()

-- | Convert a pure function into a pipe
pipe :: (Monad m) => (a -> b) -> Pipe a b m r
pipe f = forever $ do
    x <- await
    yield (f x)

{-| Deliver output downstream

    'yield' restores control back downstream and binds the result to 'await'. -}
yield :: (Monad m) => b -> Pipe a b m ()
yield = respond

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
idP = idT ()

-- | Run the 'Pipe' monad transformer, converting it back to the base monad
runPipe :: (Monad m) => Pipeline m r -> m r
runPipe = runPipe' . unProxy

runPipe' p = do
    x <- runFreeT p
    case x of
        Pure r -> return r
        Free (Request _ f) -> runPipe' (f ())
        Free (Respond _ f) -> runPipe' (f ())
