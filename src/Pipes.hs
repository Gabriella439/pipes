{-| The core functionality for the 'Proxy' monad transformer

    Read "Pipes.Tutorial" if you want a beginners tutorial explaining how to use
    this library.  The documentation in this module targets more advanced users.
-}

{-# LANGUAGE CPP, RankNTypes, EmptyDataDecls #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{- The rewrite RULES require the 'TrustWorthy' annotation.  Their proofs are
   pretty trivial since they are just inlining the definition of their
   respective operators.  GHC doesn't do this inlining automatically for these
   functions because they are recursive.
-}

module Pipes (
    -- * Proxy Monad Transformer
    Proxy,
    run,

    -- * Synonyms
    X,
    Effect,
    Producer,
    Consumer,
    Pipe,


    -- ** Yield
    -- $yield
    yield,
    for,
    (/>/),

    -- ** Await
    -- $await
    await,
    (>~),

    -- ** Pull
    -- $pull
    cat,
    (>->),
    (<-<),

    -- * ListT
    ListT(..),

    -- * Iterable
    Iterable(..),

    -- * Utilities
    next,
    each,
    every,
    discard,
    tee,
    generalize,

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
import Control.Monad.Trans.State.Strict (get, put)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Pipes.Internal (Proxy(..))
import Pipes.Core
import Pipes.Lift (evalStateP)

-- Re-exports
import Control.Monad.Morph (MFunctor(hoist))

infixr 7 <-<
infixl 7 >->

{- $yield
    'yield' and ('/>/') obey the 'Control.Category.Category' laws:

@
\ \-\- Left identity
'yield' '/>/' f

\ \-\- Right identity
f '/>/' 'yield' = f

\ \-\- Associativity
(f '/>/' g) '/>/' h = f '/>/' (g '/>/' h)
@

    This is equivalent to the following \"for loop laws\":

@
\ \-\- Looping over a single yield simplifies to function application
\ 'for' ('yield' x) f = f x

\ \-\- Re-yielding every element of a stream returns the original stream
\ 'for' m 'yield' = m

\ \-\- Nested for loops can become a sequential 'for' loops if the inner loop
\ \-\- body ignores the outer loop variable
\ 'for' m (\\a -\> 'for' (f a) g) = 'for' ('for' m f) g = 'for' m (f '/>/' g)
@

-}

{-| Produce a value

@
 'yield' :: ('Monad' m) => b -> 'Producer' b m ()
 'yield' :: ('Monad' m) => b -> 'Pipe'   a b m ()
@
-}
yield :: (Monad m) => a -> Proxy x' x a' a m a'
yield = respond
{-# INLINABLE yield #-}

{-| @(for p body)@ loops over @p@ replacing each 'yield' with @body@.

@
 'for' :: ('Monad' m) => 'Producer' b m () -> (b -> 'Effect'       m ()) -> 'Effect'       m ()
 'for' :: ('Monad' m) => 'Producer' b m () -> (b -> 'Producer'   c m ()) -> 'Producer'   c m ()
 'for' :: ('Monad' m) => 'Pipe'   a b m () -> (b -> 'Effect'       m ()) -> 'Consumer' a   m ()
 'for' :: ('Monad' m) => 'Pipe'   a b m () -> (b -> 'Producer'   c m ()) -> 'Pipe'     a c m ()
 'for' :: ('Monad' m) => 'Pipe'   a b m () -> (b -> 'Consumer' a   m ()) -> 'Consumer' a   m ()
 'for' :: ('Monad' m) => 'Pipe'   a b m () -> (b -> 'Pipe'     a c m ()) -> 'Pipe'     a c m ()
@
-}
for
    :: (Monad m)
    =>       Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    ->       Proxy x' x c' c m a'
for = (//>)
{-# INLINABLE for #-}

{- $await
    The 'await' category closely corresponds to the iteratee design pattern.  In
    this category, 'await' is the identity and ('\>\') is the composition
    operator.  You can think of them as having the following simpler types when
    you specialize them to unidirectional communication:

    'await' and ('>~') obey the 'Control.Category.Category' laws:

@
\ \-\- Feeding with an await is the same as not feeding at all
\ 'await' '>~' f = f

\ \-\- Feeding an await just replaces the await
\ f '>~' 'await' = f

\ \-\- (>~) is associative
\ (f '>~' g) '>~' h = f '>~' (g '>~' h)
@

-}

{-| Consume a value

@
 'await' :: ('Monad' m) => 'Consumer' a   m a
 'await' :: ('Monad' m) => 'Pipe'     a b m a
@
-}
await :: (Monad m) => Proxy () a y' y m a
await = request ()
{-# INLINABLE await #-}

{-| @(draw >~ p)@ loops over @p@ replacing each 'await' with @draw@

@
 ('>~') :: ('Monad' m) => 'Effect'       m b -> 'Consumer' b   m d -> 'Effect'       m d
 ('>~') :: ('Monad' m) => 'Consumer' a   m b -> 'Consumer' b   m d -> 'Consumer' a   m d
 ('>~') :: ('Monad' m) => 'Effect'       m b -> 'Pipe'     b c m d -> 'Producer'   c m d
 ('>~') :: ('Monad' m) => 'Consumer' a   m b -> 'Pipe'     b c m d -> 'Pipe'     a c m d
 ('>~') :: ('Monad' m) => 'Producer'   c m b -> 'Pipe'     b c m d -> 'Producer'   c m d
 ('>~') :: ('Monad' m) => 'Pipe'     a c m b -> 'Pipe'     b c m d -> 'Pipe'     a c m d
@
-}
(>~)
    :: (Monad m)
    => Proxy a' a y' y m b
    -> Proxy () b y' y m c
    -> Proxy a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2
{-# INLINABLE (>~) #-}

{- $pull
    'cat' and ('>->') obey the 'Control.Category' laws:

@
\ \-\- Useless use of cat
\ 'cat' '>->' f = f

\ \-\- Redirecting output to cat does nothing
\ f '>->' 'cat' = f

\ \-\- The pipe operator is associative
\ (f '>->' g) '>->' h = f '>->' (g '>->' h)
@

-}

-- | The identity 'Pipe', analogous the the Unix @cat@ program
cat :: (Monad m) => Pipe a a m r
cat = pull ()
{-# INLINABLE cat #-}

{-| 'Pipe' composition, analogous to the Unix pipe operator

@
('>->') :: ('Monad' m) => 'Producer' b m r -> 'Consumer' b   m r -> 'Effect'       m r
('>->') :: ('Monad' m) => 'Producer' b m r -> 'Pipe'     b c m r -> 'Producer'   c m r
('>->') :: ('Monad' m) => 'Pipe'   a b m r -> 'Consumer' b   m r -> 'Consumer' a   m r
('>->') :: ('Monad' m) => 'Pipe'   a b m r -> 'Pipe'     b c m r -> 'Pipe'     a c m r
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
newtype ListT m a = Select { list :: Producer a m () }

instance (Monad m) => Functor (ListT m) where
    fmap f p = Select (for (list p) (\a -> yield (f a)))

instance (Monad m) => Applicative (ListT m) where
    pure a = Select (yield a)
    mf <*> mx = Select (
        for (list mf) (\f ->
        for (list mx) (\x ->
        yield (f x) ) ) )

instance (Monad m) => Monad (ListT m) where
    return a = Select (yield a)
    m >>= f  = Select (for (list m) (\a -> list (f a)))

instance MonadTrans ListT where
    lift m = Select (do
        a <- lift m
        yield a )

instance (MonadIO m) => MonadIO (ListT m) where
    liftIO m = lift (liftIO m)

instance (Monad m) => Alternative (ListT m) where
    empty = Select (return ())
    p1 <|> p2 = Select (do
        list p1
        list p2 )

instance (Monad m) => MonadPlus (ListT m) where
    mzero = empty
    mplus = (<|>)

instance MFunctor ListT where
    hoist morph = Select . hoist morph . list

{-| 'Iterable' generalizes 'Data.Foldable.Foldable', converting effectful
    containers to 'ListT's.
-}
class Iterable t where
    toListT :: (Monad m) => t m a -> ListT m a

instance Iterable ListT where
    toListT = id

instance Iterable IdentityT where
    toListT m = Select $ do
        a <- lift $ runIdentityT m
        yield a

instance Iterable MaybeT where
    toListT m = Select $ do
        x <- lift $ runMaybeT m
        case x of
            Nothing -> return ()
            Just a  -> yield a

instance Iterable (ErrorT e) where
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

-- | Convert a 'F.Foldable' to a 'Producer'
each :: (Monad m, F.Foldable f) => f a -> Producer' a m ()
each = F.mapM_ yield
{-# INLINABLE each #-}

-- | Convert an 'Iterable' to a 'Producer'
every :: (Monad m, Iterable t) => t m a -> Producer' a m ()
every it = discard >\\ list (toListT it)
{-# INLINABLE every #-}

-- | Discards all values
discard :: (Monad m) => a -> Effect' m ()
discard _ = return ()
{-# INLINABLE discard #-}

{-| Transform a 'Consumer' to a 'Pipe' that reforwards all values further
    downstream
-}
tee :: (Monad m) => Consumer a m r -> Pipe a a m r
tee p = evalStateP Nothing $ do
    r <- up >\\ (hoist lift p //> dn)
    ma <- lift get
    case ma of
        Nothing -> return ()
        Just a  -> yield a
    return r
  where
    up () = do
        ma <- lift get
        case ma of
            Nothing -> return ()
            Just a  -> yield a
        a <- await
        lift $ put (Just a)
        return a
    dn _ = return ()
{-# INLINABLE tee #-}

{-| Transform a unidirectional 'Pipe' to a bidirectional 'Pipe'

> generalize (f >-> g) = generalize f >+> generalize g
>
> generalize cat = pull
-}
generalize :: (Monad m) => Pipe a b m r -> x -> Proxy x a x b m r
generalize p x0 = evalStateP x0 $ up >\\ hoist lift p //> dn
  where
    up () = do
        x <- lift get
        request x
    dn a = do
        x <- respond a
        lift $ put x
{-# INLINABLE generalize #-}

-- | Equivalent to ('>->') with the arguments flipped
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
