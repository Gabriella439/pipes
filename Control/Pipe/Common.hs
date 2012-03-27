module Control.Pipe.Common (
    -- * Types
    Pipe(..),
    Producer,
    Consumer,
    Pipeline,
    -- * Create Pipes
    {-|
        'yield' and 'await' are the only two primitives you need to create
        'Pipe's.  Because 'Pipe' is a monad, you can assemble them using
        ordinary @do@ notation.  Since 'Pipe' is also a monad transformer, you
        can use 'lift' to invoke the base monad.  For example:

> check :: Pipe a a IO r
> check = forever $ do
>     x <- await
>     lift $ putStrLn $ "Can " ++ (show x) ++ " pass?"
>     ok <- lift $ read <$> getLine
>     when ok (yield x)
    -}
    await,
    yield,
    pipe,
    discard,
    -- * Compose Pipes
    {-|
        There are two possible category implementations for 'Pipe':

        ['Lazy' composition]

            * Use as little input as possible

            * Ideal for infinite input streams that never need finalization

        ['Strict' composition]

            * Use as much input as possible

            * Ideal for finite input streams that need finalization

        Both category implementations enforce the category laws:

        * Composition is associative (within each instance).  This is not
          merely associativity of monadic effects, but rather true
          associativity.  The result of composition produces identical
          composite 'Pipe's regardless of how you group composition.

        * 'id' is the identity 'Pipe'.  Composing a 'Pipe' with 'id' returns the
          original pipe.

        Both categories prioritize downstream effects over upstream effects.
    -}
    Lazy(..),
    -- ** Compose Pipes
    {-|
        I provide convenience functions for composition that take care of
        newtype wrapping and unwrapping.  For example:

> p1 <+< p2 = unLazy $ Lazy p1 <<< Lazy p2

        '<+<' and '<-<' correspond to '<<<' from @Control.Category@

        '>+>' and '>+>' correspond to '>>>' from @Control.Category@

        '<+<' and '>+>' use 'Lazy' composition (Mnemonic: + for optimistic
        evaluation)

        '<-<' and '>->' use 'Strict' composition (Mnemonic: - for pessimistic
        evaluation) 

        However, the above operators won't work with 'id' because they work on
        'Pipe's whereas 'id' is a newtype on a 'Pipe'.  However, both 'Category'
        instances share the same 'id' implementation:

> instance Category (Lazy m r) where
>     id = Lazy $ pipe id
>     ....
> instance Category (Strict m r) where
>     id = Strict $ pipe id
>     ...

        So if you need an identity 'Pipe' that works with the above convenience
        operators, you can use 'idP' which is just @pipe id@.
    -}
    (<+<),
    (>+>),
    idP,
    -- * Run Pipes
    runPipe
    ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans
import Data.Void
import Prelude hiding ((.), id)

{-|
    The base type for pipes

    [@a@] The type of input received from upstream pipes

    [@b@] The type of output delivered to downstream pipes

    [@m@] The base monad

    [@r@] The type of the monad's final result

    The Pipe type is partly inspired by Mario Blazevic's Coroutine in his
    concurrency article from Issue 19 of The Monad Reader and partly inspired by
    the Trace data type from \"A Language Based Approach to Unifying Events and
    Threads\".
-}

data FreeT f m r = FreeT { runFreeT :: m (Either r (f (FreeT f m r))) }

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return = FreeT . return . Left
    m >>= f = FreeT $ runFreeT m >>= \x -> case x of
        Left  r -> runFreeT $ f r
        Right a -> return $ Right $ fmap (>>= f) a

instance (Functor f, Monad m) => Functor (FreeT f m) where fmap = liftM

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure = return
    (<*>) = ap

instance MonadTrans (FreeT f) where lift = FreeT . liftM Left

wrap :: (Monad m) => f (FreeT f m r) -> FreeT f m r
wrap = FreeT . return . Right

data PipeF a b r = Await (a -> r) | Yield (b, r)

instance Functor (PipeF a b) where
    fmap f (Await a) = Await $ fmap f a
    fmap f (Yield y) = Yield $ fmap f y

type Pipe a b m r = FreeT (PipeF a b) m r

-- | A pipe that can only produce values
type Producer b m r = Pipe () b m r

-- | A pipe that can only consume values
type Consumer a m r = Pipe a Void m r

-- | A self-contained pipeline that is ready to be run
type Pipeline m r = Pipe () Void m r

{-|
    Wait for input from upstream within the 'Pipe' monad:

    'await' blocks until input is ready.
-}
await :: (Monad m) => Pipe a b m a
await = wrap $ Await pure

{-|
    Pass output downstream within the 'Pipe' monad:

    'yield' blocks until the output has been received.
-}
yield :: (Monad m) => b -> Pipe a b m ()
yield x = wrap $ Yield (x, pure ())

{-|
    Convert a pure function into a pipe

> pipe = forever $ do
>     x <- await
>     yield (f x)
-}
pipe :: (Monad m) => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

-- | The 'discard' pipe silently discards all input fed to it.
discard :: (Monad m) => Pipe a b m r
discard = forever await

newtype Lazy   m r a b = Lazy   { unLazy   :: Pipe a b m r}

idP :: (Monad m) => Pipe a a m r
idP = pipe id

(<+<) :: (Monad m) => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p1 <+< p2 = FreeT $ do
    e1 <- runFreeT p1
    let p1' = FreeT $ return e1
    case e1 of
        Right (Await f1) -> do
            e2 <- runFreeT p2
            case e2 of
                Right (Yield (x, p)) -> runFreeT $ f1 x <+< p
                Right (Await f2) -> return $ Right $ Await $ fmap (p1' <+<) f2
                Left r -> return $ Left r
        Right (Yield y) -> return $ Right $ Yield $ fmap (<+< p2) y
        Left r -> return $ Left r
        

(>+>) :: (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
(>+>) = flip (<+<)

-- These associativities help composition detect termination quickly
infixr 9 <+<
infixl 9 >+>

{- If you assume id = forever $ await >>= yield, then the below are the only two
   Category instances possible.  I couldn't find any other useful definition of
   id, but perhaps I'm not being creative enough. -}
instance (Monad m) => Category (Lazy m r) where
    id = Lazy $ pipe id
    Lazy p1 . Lazy p2 = Lazy $ p1 <+< p2

{-|
    Run the 'Pipe' monad transformer, converting it back into the base monad

    'runPipe' will not work on a pipe that has loose input or output ends.  If
    your pipe is still generating unhandled output, handle it.  I choose not to
    automatically 'discard' output for you, because that is only one of many
    ways to deal with unhandled output.
-}
runPipe :: (Monad m) => Pipeline m r -> m r
runPipe p' = do
    e <- runFreeT p'
    case e of
        Left r -> return r
        Right (Await f) -> runPipe $ f ()
        Right (Yield (_, p)) -> runPipe p
