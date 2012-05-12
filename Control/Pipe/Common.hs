module Control.Pipe.Common (
    -- * Introduction
    -- $summary

    -- * Types
    -- $types
    PipeF(..),
    Pipe,
    Producer,
    Consumer,
    Pipeline,
    -- * Create Pipes
    -- $create
    await,
    yield,
    pipe,
    -- * Compose Pipes
    -- $newtype
    Lazy(..),
    -- $convenience
    (<+<),
    (>+>),
    idP,
    -- $category

    -- * Run Pipes
    -- $runpipe
    runPipe
    ) where

import Control.Applicative
import Control.Category
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free
import Data.Void (Void)
import Prelude hiding ((.), id)

{- $summary
    I completely expose the 'Pipe' data type and internals in order to encourage
    people to write their own 'Pipe' functions.  This does not compromise the
    correctness or safety of the library whatsoever and you can feel free to
    use the constructors directly without violating any laws or invariants.

    I promote using the 'Monad' and 'Category' instances to build and compose
    pipes, but this does not mean that they are the only option.  In fact, any
    combinator provided by other iteratee libraries can be recreated for pipes,
    too.  However, I don't copy the functions found in other libraries in order
    to encourage people to find principled and theoretically grounded solutions
    rather than devise ad-hoc solutions characteristic of other libraries.

    For example, you can't create a pipe like @toList@ that folds a pipe it is
    composed with, but nothing prevents you from writing a function that folds a
    pipe without using composition:

> fold' :: (Monad m) => Producer a m r -> m [a]
> fold' p = do
>     x <- runFreeT p
>     case x of
>         Pure _ -> return []
>         Wrap (Await f)      -> fold' $ f ()
>         Wrap (Yield (a, p)) -> liftM (p:) (fold' p)
-}

{- $types
    The 'Pipe' type is strongly inspired by Mario Blazevic's @Coroutine@ type in
    his concurrency article from Issue 19 of The Monad Reader and is formulated
    in the exact same way.

    His @Coroutine@ type is actually a free monad transformer (i.e. 'FreeT')
    and his @InOrOut@ functor corresponds to 'PipeF'.
-}
data PipeF a b x = Await (a -> x) | Yield (b, x)

-- I could use the "DerivingFunctor" extension, but I want to remain portable
instance Functor (PipeF a b) where
    fmap f (Await a) = Await $ fmap f a
    fmap f (Yield y) = Yield $ fmap f y

{-|
    The base type for pipes

    * @a@ - The type of input received from upstream pipes

    * @b@ - The type of output delivered to downstream pipes

    * @m@ - The base monad

    * @r@ - The type of the return value
-}
type Pipe a b m r = FreeT (PipeF a b) m r

-- | A pipe that produces values
type Producer b m r = Pipe () b m r

-- | A pipe that consumes values
type Consumer b m r = Pipe b Void m r

-- | A self-contained pipeline that is ready to be run
type Pipeline m r = Pipe () Void m r

{- $create
    'yield' and 'await' are the only two primitives you need to create pipes.
    Since 'Pipe a b m' is a monad, you can assemble 'yield' and 'await'
    statements using ordinary @do@ notation.  Since 'Pipe a b' is also a monad
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
await :: (Monad m) => Pipe a b m a
await = wrap $ Await return

{-|
    Deliver output downstream.

    'yield' restores control back upstream and binds the result to 'await'.
-}
yield :: (Monad m) => b -> Pipe a b m ()
yield b = wrap $ Yield (b, return ())

{-|
    Convert a pure function into a pipe

> pipe = forever $ do
>     x <- await
>     yield (f x)
-}
pipe :: (Monad m) => (a -> b) -> Pipe a b m r
pipe f = forever $ await >>= yield . f

{- $newtype
    Pipes form a 'Category', but if you want to define a proper 'Category'
    instance you have to wrap the 'Pipe' type using a newtype in order to
    rearrange the type variables:
-}

newtype Lazy m r a b = Lazy { unLazy :: Pipe a b m r}

{- If you assume id = forever $ await >>= yield, then this is the only Category
   instance possible.  I couldn't find any other useful definition of id, but
   perhaps I'm not being creative enough. -}
instance (Monad m) => Category (Lazy m r) where
    id = Lazy idP
    Lazy p1 . Lazy p2 = Lazy $ p1 <+< p2

{- $convenience
    This means that if you want to compose pipes using ('.') from the 'Category'
    type class, you end up with a newtype mess: @unLazy (Lazy p1 . Lazy p2)@.

    You can avoid this by using convenient operators that do this newtype
    wrapping and unwrapping for you:

> p1 <+< p2 = unLazy $ Lazy p1 . Lazy p2
>
> idP = unLazy id
-}

-- | Corresponds to ('<<<')/('.') from @Control.Category@
(<+<) :: (Monad m) => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p1 <+< p2 = FreeT $ do
    x1 <- runFreeT p1
    let p1' = FreeT $ return x1
    runFreeT $ case x1 of
        Pure r         -> pure r
        Wrap (Yield y) -> wrap $ Yield $ fmap (<+< p2) y
        Wrap (Await f1) -> FreeT $ do
            x2 <- runFreeT p2
            runFreeT $ case x2 of
                Pure r              -> pure r
                Wrap (Yield (x, p)) -> f1 x <+< p
                Wrap (Await f2    ) -> wrap $ Await $ fmap (p1' <+<) f2

-- | Corresponds to ('>>>') from @Control.Category@
(>+>) :: (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
(>+>) = flip (<+<)

{- These associativities might help performance since pipe evaluation is
   downstream-biased.  I set them to the same priority as (.). -}
infixr 9 <+<
infixl 9 >+>

-- | Corresponds to 'id' from @Control.Category@
idP :: (Monad m) => Pipe a a m r
idP = pipe id

{- $category
    You can compose two pipes using @p1 <+< p2@, which binds the output of @p2@
    to the input of @p1@.  For example:

> (await >>= lift . print) <+< yield 0
> = lift (print 0)

    'idP' is the identity pipe which forwards all output untouched:

> idP = forever $ do
>   x <- await
>   yield x

    Pipes are lazy, meaning that control begins at the downstream pipe and
    control only transfers upstream when the downstream pipe 'await's input from
    upstream.  If a pipe never 'await's input, then pipes upstream of it will
    never run.

    Upstream pipes relinquish control back downstream whenever they 'yield' an
    output value.  This binds the 'yield'ed value to the return value of the
    downstream 'await'.  The upstream pipe does not regain control unless the
    downstream pipe requests input again.

    When a pipe terminates, it also terminates any pipes composed with it.

    The 'Category' instance obeys the 'Category' laws.  In other words:

    * Composition is truly associative.  The result of composition produces the
      exact same composite 'Pipe' regardless of how you group composition:

> (p1 <+< p2) <+< p3 = p1 <+< (p2 <+< p3)

    * 'idP' is a true identity pipe.  Composing a pipe with 'idP' returns the
      exact same original pipe:

> p <+< idP = p
> idP <+< p = p

    The 'Category' laws are \"correct by construction\", meaning that you cannot
    break them despite the library's internals being fully exposed.  The above
    equalities are true using the strongest denotational semantics possible in
    Haskell, namely that both sides of the equals sign correspond to the exact
    same value in Haskell, constructor-for-constructor, value-for-value.  You
    cannot create a function that can distinguish the results.

    Actually, all other class instances for 'Pipe's provide the same strong
    guarantees for their corresponding laws.  I only emphasize the guarantee for
    the 'Category' instance because it is one of the most distinguishing
    features of this library.
-}

{- $runpipe
    Note that you can also unwrap a 'Pipe' a single step at a time using
    'runFreeT' (since 'Pipe' is just a type synonym for a free monad
    transformer).  This will take you to the next /external/ 'await' or 'yield'
    statement.

    This means that a closed 'Pipeline' will unwrap to a single step, in which
    case you would have been better served by 'runPipe'.  This directly follows
    from the 'Category' laws, which guarantee that you cannot resolve a
    composite pipe into its component pipes.  When you compose two pipes, the
    internal await and yield statements fuse and completely disappear.

    'runFreeT' is ideal for more advanced users who wish to write their own
    'Pipe' functions while waiting for me to find more elegant solutions.
-}
{-|
    Run the 'Pipe' monad transformer, converting it back into the base monad.

    'runPipe' imposes two conditions:

    * The pipe's input, if any, is trivially satisfiable (i.e. @()@)

    * The pipe does not 'yield' any output

    The latter restriction makes 'runPipe' less polymorphic than it could be,
    and I settled on the restriction for three reasons:

    * It prevents against accidental data loss.

    * It prevents wastefully draining a scarce resource by gratuitously
      demanding values from it.

    * It encourages an idiomatic pipe programming style where input is consumed
      in a structured way using a 'Consumer'.

    If you believe that discarding output is the appropriate behavior, you can
    specify this by explicitly feeding your output to a pipe that gratuitously
    discards it:

> runPipe $ forever await <+< p
-}
runPipe :: (Monad m) => Pipeline m r -> m r
runPipe p = do
    e <- runFreeT p
    case e of
        Pure   r       -> return r
        Wrap (Await f) -> runPipe $ f ()
        Wrap (Yield y) -> runPipe $ snd y
