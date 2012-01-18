{-
    Copyright 2012 Gabriel Gonzalez

    This file is part of the Haskell Pipes Library.

    The Haskell Pipes Library is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or (at your
    option) any later version.

    The Haskell Pipes Library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
    Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the Haskell Pipes Library.  If not, see
    <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE Rank2Types #-}

module Control.Pipe.Common (
    -- * Types
    Pipe,
    Zero,
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
    Strict(..),
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
    (<-<),
    (>->),
    idP,
    -- * Run Pipes
    runPipe
    ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans
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
data Pipe a b m r =
    Pure r                     -- pure = Pure
  | M     (m   (Pipe a b m r)) -- Monad
  | Await (a -> Pipe a b m r ) -- ((->) a) Functor
  | Yield (b,   Pipe a b m r ) -- ((,)  b) Functor
{- I could have factored Pipe as:

data Computation f r = Pure r | F (f (Computation f r))
data PipeF a b m r = Await (a -> r) | Yield (b, r) | M (m r)
newtype Pipe a b m r = P { unP :: Computation (PipeF a b m) r }

   This makes the Functor, Applicative, and Monad instances much simpler at the
   expense of making the Category instances *much* harder to follow because of
   excessive newtype and constructor wrapping/unwrapping.  Since the Category
   instance is the meat of the library, I opted to in-line PipeF into
   computation to make it much simpler.  It's a shame, because the Computation
   type is very useful in its own right and I will probably create a separate
   library around it. -}

instance (Monad m) => Functor (Pipe a b m) where
    fmap f c = case c of
        Pure r   -> Pure $ f r
        M mc     -> M     $ liftM (fmap f) mc
        Await fc -> Await $ fmap  (fmap f) fc
        Yield fc -> Yield $ fmap  (fmap f) fc

instance (Monad m) => Applicative (Pipe a b m) where
    pure = Pure
    f <*> x = case f of
        Pure r   -> fmap r x
        M mc     -> M     $ liftM (<*> x) mc
        Await fc -> Await $ fmap  (<*> x) fc
        Yield fc -> Yield $ fmap  (<*> x) fc

instance (Monad m) => Monad (Pipe a b m) where
    return = pure
    m >>= f = case m of
        Pure r   -> f r
        M mc     -> M     $ liftM (>>= f) mc
        Await fc -> Await $ fmap  (>>= f) fc
        Yield fc -> Yield $ fmap  (>>= f) fc

instance MonadTrans (Pipe a b) where lift = M . liftM pure

-- | A data type with no exposed constructors
data Zero = Zero
{- I'm not quite sure that this is the correct approach.  I also considered
   using "()" or universal quantification (i.e. Producer b m r =
   forall a . Pipe a b m r).  What I really want is some way to provide runPipe
   some compile-time guarantee that its argument Pipe has no residual await or
   yield statements.  -}

-- | A pipe that can only produce values
type Producer b m r = Pipe Zero b m r

-- | A pipe that can only consume values
type Consumer a m r = Pipe a Zero m r

-- | A self-contained pipeline that is ready to be run
type Pipeline m r = Pipe Zero Zero m r

{-|
    Wait for input from upstream within the 'Pipe' monad:

    'await' blocks until input is ready.
-}
await :: Pipe a b m a
await = Await Pure 

{-|
    Pass output downstream within the 'Pipe' monad:

    'yield' blocks until the output has been received.
-}
yield :: b -> Pipe a b m ()
yield x = Yield (x, Pure ())

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
newtype Strict m r a b = Strict { unStrict :: Pipe a b m r}

idP :: (Monad m) => Pipe a a m r
idP = pipe id

(<+<), (<-<) :: (Monad m) => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
p1 <+< p2 = unLazy   (Lazy   p1 <<< Lazy   p2)
p1 <-< p2 = unStrict (Strict p1 <<< Strict p2)

(>+>), (>->) :: (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
p1 >+> p2 = unLazy   (Lazy   p1 >>> Lazy   p2)
p1 >-> p2 = unStrict (Strict p1 >>> Strict p2)

-- These associativities help composition detect termination quickly
infixr 9 <+<, >->
infixl 9 >+>, <-<

{- If you assume id = forever $ await >>= yield, then the below are the only two
   Category instances possible.  I couldn't find any other useful definition of
   id, but perhaps I'm not being creative enough. -}
instance (Monad m) => Category (Lazy m r) where
    id = Lazy $ pipe id
    Lazy p1' . Lazy p2' = Lazy $ case (p1', p2') of
        (Yield (x1, p1), p2            ) -> yield x1 >>         p1 <+< p2
        (M m1          , p2            ) -> lift m1  >>= \p1 -> p1 <+< p2
        (p1            , Await f2      ) -> await    >>= \x  -> p1 <+< f2 x
        (p1            , M m2          ) -> lift m2  >>= \p2 -> p1 <+< p2
        (Await f1      , Yield (x2, p2)) -> f1 x2 <+< p2
        (Pure r1       , _             ) -> Pure r1
        (_             , Pure r2       ) -> Pure r2

instance (Monad m) => Category (Strict m r) where
    id = Strict $ pipe id
    Strict p1 . Strict p2 = Strict $ (p1 >> discard) <+< p2

{-|
    Run the 'Pipe' monad transformer, converting it back into the base monad

    'runPipe' will not work on a pipe that has loose input or output ends.  If
    your pipe is still generating unhandled output, handle it.  I choose not to
    automatically 'discard' output for you, because that is only one of many
    ways to deal with unhandled output.
-}
runPipe :: (Monad m) => Pipeline m r -> m r
runPipe p' = case p' of
    Pure r          -> return r
    M mp            -> mp >>= runPipe
    -- Technically a blocked Pipe can still await
    Await f         -> runPipe $ f Zero
    -- A blocked Pipe can not yield, but I include this as a precaution
    Yield (_, p) -> runPipe p
