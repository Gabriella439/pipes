{-| The 'Pipe' type is a monad transformer that enriches the base monad with the
    ability to 'await' or 'yield' data to and from other 'Pipe's. -}

module Control.Pipe (
    -- * Types
    -- $types
    Pipe(..),
    Producer,
    Consumer,
    Pipeline,
    -- * Create Pipes
    -- $create
    await,
    yield,
    pipe,
    -- * Compose Pipes
    -- $category
    (<+<),
    (>+>),
    idP,
    PipeC(..),
    -- * Run Pipes
    -- $runpipe
    runPipe
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Category (Category((.), id), (<<<), (>>>))
import Control.Monad (forever)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Closed (C)
import Prelude hiding ((.), id)

{- $types
    The 'Pipe' type is strongly inspired by Mario Blazevic's @Coroutine@ type in
    his concurrency article from Issue 19 of The Monad Reader.
-}

{-|
    The base type for pipes

    * @a@ - The type of input received from upstream pipes

    * @b@ - The type of output delivered to downstream pipes

    * @m@ - The base monad

    * @r@ - The type of the return value
-}
data Pipe a b m r
  = Await (a -> Pipe a b m r)
  | Yield b    (Pipe a b m r)
  | M       (m (Pipe a b m r))
  | Pure r
{-
type PipeF a b x = Await (a -> x) | Yield b x deriving (Functor)

type Pipe a b = FreeT (PipeF a b)
-}

instance (Monad m) => Functor (Pipe a b m) where
    fmap f pr = go pr where
        go p = case p of
            Await   k  -> Await (\a -> go (k a))
            Yield b p' -> Yield b (go p')
            M       m  -> M (m >>= \p' -> return (go p'))
            Pure    r  -> Pure (f r)

instance (Monad m) => Applicative (Pipe a b m) where
    pure = Pure
    pf <*> px = go pf where
        go p = case p of
            Await   k  -> Await (\a -> go (k a))
            Yield b p' -> Yield b (go p')
            M       m  -> M (m >>= \p' -> return (go p'))
            Pure    f  -> fmap f px

instance (Monad m) => Monad (Pipe a b m) where
    return  = Pure
    pm >>= f = go pm where
        go p = case p of
            Await   k  -> Await (\a -> go (k a))
            Yield b p' -> Yield b (go p')
            M       m  -> M (m >>= \p' -> return (go p'))
            Pure    r  -> f r

instance MonadTrans (Pipe a b) where
    lift m = M (m >>= \r -> return (Pure r))

-- | A pipe that produces values
type Producer b m r = Pipe () b m r

-- | A pipe that consumes values
type Consumer a m r = Pipe a C m r

-- | A self-contained pipeline that is ready to be run
type Pipeline m r = Pipe () C m r

{- $create
    'yield' and 'await' are the only two primitives you need to create pipes.
    Since @Pipe a b m@ is a monad, you can assemble 'yield' and 'await'
    statements using ordinary @do@ notation.  Since @Pipe a b@ is also a monad
    transformer, you can use 'lift' to invoke the base monad.  For example, you
    could write a pipe stage that requests permission before forwarding any
    output:

> check :: (Show a) => Pipe a a IO r
> check = forever $ do
>     x <- await
>     lift $ putStrLn $ "Can '" ++ (show x) ++ "' pass?"
>     ok <- read <$> lift getLine
>     when ok (yield x)
-}

{-|
    Wait for input from upstream.

    'await' blocks until input is available from upstream.
-}
await :: Pipe a b m a
await = Await Pure

{-|
    Deliver output downstream.

    'yield' restores control back upstream and binds the result to 'await'.
-}
yield :: b -> Pipe a b m ()
yield b = Yield b (Pure ())

{-|
    Convert a pure function into a pipe

> pipe f = forever $ do
>     x <- await
>     yield (f x)
-}
pipe :: (Monad m) => (a -> b) -> Pipe a b m r
pipe f = go where
    go = Await (\a -> Yield (f a) go)

{- $category
    'Pipe's form a 'Category', meaning that you can compose 'Pipe's using
    ('<+<') and also define an identity 'Pipe': 'idP'.  These satisfy the
    category laws:

> idP <+< p = p
>
> p <+< idP = p
>
> (p1 <+< p2) <+< p3 = p1 <+< (p2 <+< p3)

    'Pipe' composition binds the output of the upstream 'Pipe' to the input of
    the downstream 'Pipe'.  Like Haskell functions, 'Pipe's are lazy, meaning
    that upstream 'Pipe's are only evaluated as far as necessary to generate
    enough input for downstream 'Pipe's.  If any 'Pipe' terminates, it also
    terminates every 'Pipe' composed with it.
-}

-- | 'Pipe's form a 'Category' instance when you rearrange the type variables
newtype PipeC m r a b = PipeC { unPipeC :: Pipe a b m r}

instance (Monad m) => Category (PipeC m r) where
    id = PipeC idP
    PipeC p1 . PipeC p2 = PipeC $ p1 <+< p2

-- | Corresponds to ('<<<')/('.') from @Control.Category@
(<+<) :: (Monad m) => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
(Yield b p1) <+< p2 = Yield b (p1 <+< p2)
(M       m ) <+< p2 = M (m >>= \p1 -> return (p1 <+< p2))
(Pure    r ) <+< _  = Pure r
(Await   k ) <+< (Yield b p2) = k b <+< p2
p1 <+< (Await k) = Await (\a -> p1 <+< k a)
p1 <+< (M     m) = M (m >>= \p2 -> return (p1 <+< p2))
_  <+< (Pure  r) = Pure r

-- | Corresponds to ('>>>') from @Control.Category@
(>+>) :: (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
p2 >+> p1 = p1 <+< p2

{- These associativities might help performance since pipe evaluation is
   downstream-biased.  I set them to the same priority as (.). -}
infixr 9 <+<
infixl 9 >+>

-- | Corresponds to 'id' from @Control.Category@
idP :: (Monad m) => Pipe a a m r
idP = go where
    go = Await (\a -> Yield a go)

{- $runpipe
    Note that you can also unwrap a 'Pipe' a single step at a time using
    'runFreeT' (since 'Pipe' is just a type synonym for a free monad
    transformer).  This will take you to the next /external/ 'await' or 'yield'
    statement.  This means that a closed 'Pipeline' will unwrap to a single
    step, in which case you would have been better served by 'runPipe'.
-}
{-|
    Run the 'Pipe' monad transformer, converting it back into the base monad.

    'runPipe' imposes two conditions:

    * The pipe's input, if any, is trivially satisfiable (i.e. @()@)

    * The pipe does not 'yield' any output

    The latter restriction makes 'runPipe' less polymorphic than it could be,
    and I settled on the restriction for three reasons:

    * It prevents against accidental data loss.

    * It protects against silent failures

    * It prevents wastefully draining a scarce resource by gratuitously
      driving it to completion

    If you believe that discarding output is the appropriate behavior, you can
    specify this by explicitly feeding your output to a pipe that gratuitously
    discards it:

> runPipe $ forever await <+< p
-}
runPipe :: (Monad m) => Pipe () b m r -> m r
runPipe pl = go pl where
    go p = case p of
       Yield _ p' -> go p' 
       Await   k  -> go (k ())
       M       m  -> m >>= go
       Pure    r  -> return r
