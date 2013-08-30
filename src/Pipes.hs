{-| This module is the recommended entry point to the @pipes@ library.

    Read "Pipes.Tutorial" if you want a tutorial explaining how to use this
    library.
-}

{-# LANGUAGE RankNTypes, CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{- The rewrite RULES require the 'TrustWorthy' annotation. -}

module Pipes (
    -- * The Proxy Monad Transformer
    Proxy,
    X,
    Effect,
    Effect',
    runEffect,

    -- ** Producers
    -- $producers
    Producer,
    Producer',
    yield,
    for,
    (~>),
    (<~),

    -- ** Consumers
    -- $consumers
    Consumer,
    Consumer',
    await,
    (>~),
    (~<),

    -- ** Pipes
    -- $pipes
    Pipe,
    cat,
    (>->),
    (<-<),

    -- * ListT
    ListT(..),
    Enumerable(..),

    -- * Utilities
    next,
    each,
    every,
    discard,

    -- * Re-exports
    -- $reexports
    module Control.Monad.Trans.Class,
    module Control.Monad.Morph,
    module Data.Foldable
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Error (ErrorT(runErrorT))
import Control.Monad.Trans.Identity (IdentityT(runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Pipes.Internal (Proxy(..))
import Pipes.Core

-- Re-exports
import Control.Monad.Morph (MFunctor(hoist))

infixl 4 <~
infixr 4 ~>
infixl 5 ~<
infixr 5 >~
infixl 7 >->
infixr 7 <-<

{- $producers
    Use 'yield' to produce output and ('~>') \/ 'for' to substitute 'yield's.

    'yield' and ('~>') obey the 'Control.Category.Category' laws:

@
\-\- Substituting \'yield\' with \'f\' gives \'f\'
'yield' '~>' f

\-\- Substituting every \'yield\' with another \'yield\' does nothing
f '~>' 'yield' = f

\-\- \'yield\' substitution is associative
(f '~>' g) '~>' h = f '~>' (g '~>' h)
@

    These are equivalent to the following \"for loop laws\":

@
\-\- Looping over a single yield simplifies to function application
'for' ('yield' x) f = f x

\-\- Re-yielding every element of a stream returns the original stream
'for' s 'yield' = s

\-\- Nested for loops can become a sequential 'for' loops if the inner loop
\-\- body ignores the outer loop variable
'for' s (\\a -\> 'for' (f a) g) = 'for' ('for' s f) g = 'for' s (f '~>' g)
@

-}

{-| Produce a value

@
'yield' :: 'Monad' m => a -> 'Pipe' x a m ()
@
-}
yield :: (Monad m) => a -> Producer' a m ()
yield = respond
{-# INLINABLE yield #-}

{-| @(for p body)@ loops over @p@ replacing each 'yield' with @body@.

@
'for' :: 'Monad' m => 'Producer' b m r -> (b -> 'Effect'       m ()) -> 'Effect'       m r
'for' :: 'Monad' m => 'Producer' b m r -> (b -> 'Producer'   c m ()) -> 'Producer'   c m r
'for' :: 'Monad' m => 'Pipe'   x b m r -> (b -> 'Effect'       m ()) -> 'Consumer' x   m r
'for' :: 'Monad' m => 'Pipe'   x b m r -> (b -> 'Producer'   c m ()) -> 'Pipe'     x c m r
'for' :: 'Monad' m => 'Pipe'   x b m r -> (b -> 'Consumer' x   m ()) -> 'Consumer' x   m r
'for' :: 'Monad' m => 'Pipe'   x b m r -> (b -> 'Pipe'     x c m ()) -> 'Pipe'     x c m r
@
-}
for :: (Monad m)
    =>       Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    ->       Proxy x' x c' c m a'
for = (//>)
{-# INLINABLE for #-}

{-# RULES
    "for cat f" forall f .
        for cat f =
            let go = do
                    x <- await
                    f x
                    go
            in  go
  ; "m >~ cat" forall m .
        m >~ cat =
            let go = do
                    x <- m
                    yield x
                    go
            in  go
  #-}

{-| Compose loop bodies

@
('~>') :: 'Monad' m => (a -> 'Producer' b m r) -> (b -> 'Effect'       m ()) -> (a -> 'Effect'       m r)
('~>') :: 'Monad' m => (a -> 'Producer' b m r) -> (b -> 'Producer'   c m ()) -> (a -> 'Producer'   c m r)
('~>') :: 'Monad' m => (a -> 'Pipe'   x b m r) -> (b -> 'Effect'       m ()) -> (a -> 'Consumer' x   m r)
('~>') :: 'Monad' m => (a -> 'Pipe'   x b m r) -> (b -> 'Producer'   c m ()) -> (a -> 'Pipe'     x c m r)
('~>') :: 'Monad' m => (a -> 'Pipe'   x b m r) -> (b -> 'Consumer' x   m ()) -> (a -> 'Consumer' x   m r)
('~>') :: 'Monad' m => (a -> 'Pipe'   x b m r) -> (b -> 'Pipe'     x c m ()) -> (a -> 'Pipe'     x c m r)
@
-}
(~>)
    :: (Monad m)
    => (a -> Proxy x' x b' b m a')
    -> (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x c' c m a')
(~>) = (/>/)
{-# INLINABLE (~>) #-}

-- | ('~>') with the arguments flipped
(<~)
    :: (Monad m)
    => (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x b' b m a')
    -> (a -> Proxy x' x c' c m a')
g <~ f = f ~> g
{-# INLINABLE (<~) #-}

{- $consumers
    Use 'await' to request input and ('>~') to substitute 'await's.

    'await' and ('>~') obey the 'Control.Category.Category' laws:

@
\-\- Substituting every \'await\' with another \'await\' does nothing
'await' '>~' f = f

\-\- Substituting \'await\' with \'f\' gives \'f\'
f '>~' 'await' = f

\-\- \'await\' substitution is associative
(f '>~' g) '>~' h = f '>~' (g '>~' h)
@

-}

{-| Consume a value

@
'await' :: 'Monad' m => 'Pipe' a y m a
@
-}
await :: (Monad m) => Consumer' a m a
await = request ()
{-# INLINABLE await #-}

{-| @(draw >~ p)@ loops over @p@ replacing each 'await' with @draw@

@
('>~') :: 'Monad' m => 'Effect'       m b -> 'Consumer' b   m c -> 'Effect'       m c
('>~') :: 'Monad' m => 'Consumer' a   m b -> 'Consumer' b   m c -> 'Consumer' a   m c
('>~') :: 'Monad' m => 'Effect'       m b -> 'Pipe'     b y m c -> 'Producer'   y m c
('>~') :: 'Monad' m => 'Consumer' a   m b -> 'Pipe'     b y m c -> 'Pipe'     a y m c
('>~') :: 'Monad' m => 'Producer'   y m b -> 'Pipe'     b y m c -> 'Producer'   y m c
('>~') :: 'Monad' m => 'Pipe'     a y m b -> 'Pipe'     b y m c -> 'Pipe'     a y m c
@
-}
(>~)
    :: (Monad m)
    => Proxy a' a y' y m b
    -> Proxy () b y' y m c
    -> Proxy a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2
{-# INLINABLE (>~) #-}

-- | ('>~') with the arguments flipped
(~<)
    :: (Monad m)
    => Proxy () b y' y m c
    -> Proxy a' a y' y m b
    -> Proxy a' a y' y m c
p2 ~< p1 = p1 >~ p2
{-# INLINABLE (~<) #-}

{- $pipes
    Use 'await' and 'yield' to build 'Pipe's and ('>->') to connect 'Pipe's.

    'cat' and ('>->') obey the 'Control.Category' laws:

@
\-\- Useless use of cat
'cat' '>->' f = f

\-\- Redirecting output to cat does nothing
f '>->' 'cat' = f

\-\- The pipe operator is associative
(f '>->' g) '>->' h = f '>->' (g '>->' h)
@

-}

-- | The identity 'Pipe', analogous to the Unix @cat@ program
cat :: (Monad m) => Pipe a a m r
cat = pull ()
{-# INLINABLE cat #-}

{-| 'Pipe' composition, analogous to the Unix pipe operator

@
('>->') :: 'Monad' m => 'Producer' b m r -> 'Consumer' b   m r -> 'Effect'       m r
('>->') :: 'Monad' m => 'Producer' b m r -> 'Pipe'     b c m r -> 'Producer'   c m r
('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Consumer' b   m r -> 'Consumer' a   m r
('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Pipe'     b c m r -> 'Pipe'     a c m r
@
-}
(>->)
    :: (Monad m)
    => Proxy a' a () b m r
    -> Proxy () b c' c m r
    -> Proxy a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2
{-# INLINABLE (>->) #-}

{-| The list monad transformer, which extends a monad with non-determinism

    'return' corresponds to 'yield', yielding a single value

    ('>>=') corresponds to 'for', calling the second computation once for each
    time the first computation 'yield's.
-}
newtype ListT m a = Select { enumerate :: Producer a m () }

instance (Monad m) => Functor (ListT m) where
    fmap f p = Select (for (enumerate p) (\a -> yield (f a)))

instance (Monad m) => Applicative (ListT m) where
    pure a = Select (yield a)
    mf <*> mx = Select (
        for (enumerate mf) (\f ->
        for (enumerate mx) (\x ->
        yield (f x) ) ) )

instance (Monad m) => Monad (ListT m) where
    return a = Select (yield a)
    m >>= f  = Select (for (enumerate m) (\a -> enumerate (f a)))

instance MonadTrans ListT where
    lift m = Select (do
        a <- lift m
        yield a )

instance (MonadIO m) => MonadIO (ListT m) where
    liftIO m = lift (liftIO m)

instance (Monad m) => Alternative (ListT m) where
    empty = Select (return ())
    p1 <|> p2 = Select (do
        enumerate p1
        enumerate p2 )

instance (Monad m) => MonadPlus (ListT m) where
    mzero = empty
    mplus = (<|>)

instance MFunctor ListT where
    hoist morph = Select . hoist morph . enumerate

{-| 'Enumerable' generalizes 'Data.Foldable.Foldable', converting effectful
    containers to 'ListT's.
-}
class Enumerable t where
    toListT :: (Monad m) => t m a -> ListT m a

instance Enumerable ListT where
    toListT = id

instance Enumerable IdentityT where
    toListT m = Select $ do
        a <- lift $ runIdentityT m
        yield a

instance Enumerable MaybeT where
    toListT m = Select $ do
        x <- lift $ runMaybeT m
        case x of
            Nothing -> return ()
            Just a  -> yield a

instance Enumerable (ErrorT e) where
    toListT m = Select $ do
        x <- lift $ runErrorT m
        case x of
            Left  _ -> return ()
            Right a -> yield a

{-| Consume the first value from a 'Producer'

    'next' either fails with a 'Left' if the 'Producer' terminates or succeeds
    with a 'Right' providing the next value and the remainder of the 'Producer'.
-}
next :: (Monad m) => Producer a m r -> m (Either r (a, Producer a m r))
next = go
  where
    go p = case p of
        Request _ fu -> go (fu ())
        Respond a fu -> return (Right (a, fu ()))
        M         m  -> m >>= go
        Pure    r    -> return (Left r)
{-# INLINABLE next #-}

-- | Convert a 'F.Foldable' to a 'Producer'
each :: (Monad m, F.Foldable f) => f a -> Producer' a m ()
each = F.mapM_ yield
{-# INLINABLE each #-}

-- | Convert an 'Enumerable' to a 'Producer'
every :: (Monad m, Enumerable t) => t m a -> Producer' a m ()
every it = discard >\\ enumerate (toListT it)
{-# INLINABLE every #-}

-- | Discards a value
discard :: (Monad m) => a -> m ()
discard _ = return ()
{-# INLINABLE discard #-}

-- | ('>->') with the arguments flipped
(<-<)
    :: (Monad m)
    => Proxy () b c' c m r
    -> Proxy a' a () b m r
    -> Proxy a' a c' c m r
p2 <-< p1 = p1 >-> p2
{-# INLINABLE (<-<) #-}

{- $reexports
    "Control.Monad.Trans.Class" re-exports 'MonadTrans'.

    "Control.Monad.Morph" re-exports 'MFunctor'.

    "Data.Foldable" re-exports 'Foldable' (the class name only)
-}
