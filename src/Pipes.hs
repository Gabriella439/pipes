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

    -- * Categories
    -- $categories

    -- ** Yield
    -- $yield
    yield,
    (/>/),
    (\<\),
    for,

    -- ** Await
    -- $await
    await,
    feed,

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

    -- * Concrete Type Synonyms
    X,
    Effect,
    Producer,
    Pipe,
    Consumer,
    Client,
    Server,

    -- * Polymorphic Type Synonyms
    Effect',
    Producer',
    Consumer',
    Client',
    Server',

    -- * Re-exports
    -- $reexports
    module Control.Monad,
    module Control.Monad.Trans.Class,
    module Control.Monad.Morph,
    module Data.Foldable
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad ((>=>), (<=<))
import qualified Control.Monad as M
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
    The 'yield' category closely corresponds to the generator design pattern.
    In this category, 'yield' is the identity and ('/>/') is the composition
    operator.  You can think of them as having the following simpler types when
    you specialize them to unidirectional communication:

> -- Produce one output value
> yield :: (Monad m) =>  a -> Producer a m ()
>
> -- Connect unfolds, optionally terminating with an 'Effect'
> (/>/) :: (Monad m) => (a -> Producer b m ()) -> (b -> Producer c m ()) -> (a -> Producer c m ())
> (/>/) :: (Monad m) => (a -> Producer b m ()) -> (b -> Effect     m ()) -> (a -> Effect     m ())

    The 'yield' category obeys the category laws, where 'yield' is the
    identity and ('/>/') is composition:

> -- Left identity
> yield />/ f = f
>
> -- Right identity
> f />/ yield = f
>
> -- Associativity
> (f />/ g) />/ h = f />/ (g />/ h)

    When you write these laws in terms of 'for', you get the \"for loop laws\":

> -- Looping over a single yield simplifies to function application
> for (yield x) f = f x
>
> -- Re-'yield'ing every element returns the original stream
> for m yield = m
>
> -- Nested for loops can become sequential for loops if the inner loop body
> -- ignores the outer loop variable
> for m (\a -> for (f a) g) = for (for m f) g

    In the fully general case, 'yield' can return a value and connected
    components share the same upstream interface:

> yield :: (Monad m)
>       =>  a -> Proxy x' x a' a m a'
>
>           a
>           |
>      +----|----+
>      |    |    |
>  x' <==   \ /==== a'
>      |     X   |
>  x  ==>   / \===> a
>      |    |    |
>      +----|----+
>           v 
>           a'
>
> (/>/) :: (Monad m)
>       => (a -> Proxy x' x b' b m a')
>       -> (b -> Proxy x' x c' c m b')
>       -> (a -> Proxy x' x b' b m a')
>
>           a                 /=====> b                      a
>           |                //       |                      |
>      +----|----+          //   +----|----+            +----|----+
>      |    v    |         //    |    v    |            |    v    |
>  x' <==       <== b' <=\// x' <==       <== c'    x' <==       <== c'
>      |    f    |       \\      |    g    |     =      | f />/ g |
>  x  ==>       ==> b  ==/\\ x  ==>       ==> c     x  ==>       ==> c'
>      |    |    |         \\    |    |    |            |    |    |
>      +----|----+          \\   +----|----+            +----|----+
>           v                \\       v                      v
>           a'                \====== b'                     a'

-}

{-| Send a value of type @b@ downstream and block waiting for a reply of type
    @b'@

    'yield' is the identity of the yield category.
-}
yield :: (Monad m) => a -> Proxy x' x a' a m a'
yield = respond
{-# INLINABLE yield #-}

{-| @(for p f)@ replaces each 'yield' in @p@ with @f@.

    Synonym for ('//>')
-}
for
    :: (Monad m)
    =>       Proxy x' x b' b m a'
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    ->       Proxy x' x c' c m a'
    -- ^
for = (//>)
{-# INLINABLE for #-}

{- $await
    The 'await' category closely corresponds to the iteratee design pattern.  In
    this category, 'await' is the identity and ('\>\') is the composition
    operator.  You can think of them as having the following simpler types when
    you specialize them to unidirectional communication:

> -- Consume one input value
> await :: (Monad m) =>  () -> Consumer a m a
>
> -- Connect folds, optionally beginning with an 'Effect'
> (\>\) :: (Monad m) => (() -> Consumer a m b) -> (() -> Consumer b m c) -> (() -> Consumer a m c)
> (\>\) :: (Monad m) => (() -> Effect     m a) -> (() -> Consumer a m b) -> (() -> Effect     m b)

    'feed' is like ('\>\'), except that the arguments are flipped and they don't
    require the unnecessary @()@ parameters:

> feed :: (Monad m) => Consumer b m c -> Consumer a m b -> Consumer a m c
> feed :: (Monad m) => Consumer a m b -> Effect     m a -> Effect     m b

    The 'await' category obeys the category laws, where 'await' is the
    identity and ('\>\') is composition:

> -- Left identity
> await \>\ f = f
>
> -- Right identity
> f \>\ await = f
>
> -- Associativity
> (f \>\ g) \>\ h = f \>\ (g \>\ h)

    When you write these laws in terms of 'feed', you get the \"feed loop
    laws\":

> -- Feeding a single 'await' just replaces the 'await'
> feed (await ()) m = m
>
> -- Feeding with an 'await' is the same as not feeding at all
> feed m (await ()) = m
>
> -- Nested feed loops can always be rewritten to sequential feed loops
> feed m (feed n o) = feed (feed m n) o

    In the fully general case, 'await' can send an argument upstream and
    connected components share the same downstream interface:

> await :: (Monad m)
>       =>  a' -> Proxy a' a y' y m a
>
>           a'
>           |
>      +----|----+
>      |    |    |
>  a' <====/    <== y'
>      |         |
>  a  =====\    ==> y
>      |    |    |
>      +----|----+
>           v 
>           a 
>
> (\>\) :: (Monad m)
>       => (b' -> Proxy a' a y' y m b)
>       -> (c' -> Proxy b' b y' y m c)
>       -> (c' -> Proxy a' a y' y m c)
>
>           b'<======\               c'                     c'
>           |        \\              |                      |
>      +----|----+    \\        +----|----+            +----|----+
>      |    v    |     \\       |    v    |            |    v    |
>  a' <==       <== y'  \== b' <==       <== y'    a' <==       <== y'
>      |    f    |              |    g    |     =      | f \>\ g |
>  a  ==>       ==> y   /=> b  ==>       ==> y     a  ==>       ==> y
>      |    |    |     //       |    |    |            |    |    |
>      +----|----+    //        +----|----+            +----|----+
>           v        //              v                      v
>           b =======/               c                      c
-}

{-| Send a value of type @a'@ upstream and block waiting for a reply of type @a@

    'await' is the identity of the await category.
-}
await :: (Monad m) => Proxy () a y' y m a
await = request ()
{-# INLINABLE await #-}

-- | @(feed p1 p2)@ replaces each @(await ())@ in @p1@ with @p2@
feed
    :: (Monad m)
    => Proxy () b y' y m c
    -> Proxy a' a y' y m b
    -> Proxy a' a y' y m c
feed p1 p2 = (\() -> p2) >\\ p1
{-# INLINABLE feed #-}

{- $pull
    The 'pull' category closely corresponds to pull-based Unix pipes and
    consists of three operations, which you can think of as having the following
    types:

> -- 'pull' retransmits all values
> pull  :: (Monad m)
>       =>  () -> Pipe a a m r
>
> -- '+>>' transforms a 'Consumer' by applying a 'Pipe' or 'Producer' upstream
> (+>>) :: (Monad m)               |  (+>>) :: (Monad m)
>       => (() -> Pipe   a b m r)  |        => (() -> Producer b m r)
>       ->        Consumer b m r   |        ->        Consumer b m r
>       ->        Consumer a m r   |        ->        Effect     m r
>
> -- '>+>' connects two 'Pipe's or 'Producer's
> (>+>) :: (Monad m)               |  (>+>) :: (Monad m)
>       => (() -> Pipe   a b m r)  |        -> (() -> Producer b m r)
>       -> (() -> Pipe   b c m r)  |        -> (() -> Pipe   b c m r)
>       -> (() -> Pipe   a c m r)  |        -> (() -> Producer c m r)

    The 'pull' category obeys the category laws, where 'pull' is the identity
    and ('>+>') is composition:

> -- Left identity
> pull >+> f = f
>
> -- Right identity
> f >+> pull = f
>
> -- Associativity
> (f >+> g) >+> h = f >+> (g >+> h)

    For unidirectional Unix-like pipes, you can use the following simpler
    operations, which you can think of as having the following types:

> cat   :: (Monad m) => Pipe   a a m r
>
> (>->) :: (Monad m) => Producer a m r -> Consumer a m r -> Effect     m r
> (>->) :: (Monad m) => Producer a m r -> Pipe   a b m r -> Producer b m r
> (>->) :: (Monad m) => Pipe   a b m r -> Consumer b m r -> Consumer a m r
> (>->) :: (Monad m) => Pipe   a b m r -> Pipe   b c m r -> Pipe   a c m r

    When you write the 'pull' category laws in terms of ('>->') and 'cat', you
    get the category laws for Unix pipes:

> -- Useless use of 'cat'
> cat >-> f = f
>
> -- Redirecting stdout to 'cat' does nothing
> f >-> cat = f
>
> -- The Unix pipe operator is associative
> (f >-> g) >-> h = f >-> (g >-> h)

    In the fully general case, you can also send information upstream by
    invoking 'await' with a non-@()@ argument.  The upstream 'Proxy' will
    receive the first value through its initial argument and bind each
    subsequent value through the return value of 'yield':

> pull  :: (Monad m)
>       =>  a' -> Proxy a' a a' a m r
>
>           a'
>           |
>      +----|----+
>      |    v    |
>  a' <============ a'
>      |         |
>  a  ============> a
>      |    |    |
>      +----|----+
>           v
>           r
>
> (>+>) :: (Monad m)
>       -> (b' -> Proxy a' a b' b m r)
>       -> (c' -> Proxy b' b c' c m r)
>       -> (c' -> Proxy a' a c' c m r)
>
>           b'               c'                     c'
>           |                |                      |
>      +----|----+      +----|----+            +----|----+
>      |    v    |      |    v    |            |    v    |
>  a' <==       <== b' <==       <== c'    a' <==       <== c'
>      |    f    |      |    g    |     =      | f >+> g |
>  a  ==>       ==> b  ==>       ==> c     a  ==>       ==> c
>      |    |    |      |    |    |            |    |    |
>      +----|----+      +----|----+            +----|----+
>           v                v                      v
>           r                r                      r

-}

-- | Unidirectional identity, named after the Unix @cat@ program
cat :: (Monad m) => Pipe a a m r
cat = pull ()
{-# INLINABLE cat #-}

-- | Unidirectional composition, analogous to the Unix pipe operator: @|@.
(>->)
    :: (Monad m)
    => Proxy a' a () b m r
    -- ^
    -> Proxy () b c' c m r
    -- ^
    -> Proxy a' a c' c m r
    -- ^
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

instance (Monad m) => M.MonadPlus (ListT m) where
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
    -- ^
    -> Proxy a' a () b m r
    -- ^
    -> Proxy a' a c' c m r
    -- ^
p2 <-< p1 = p1 >-> p2
{-# INLINABLE (<-<) #-}

{- $reexports
    "Control.Monad" re-exports ('>=>') and ('<=<').

    "Control.Monad.Trans.Class" re-exports 'MonadTrans'.

    "Control.Monad.Morph" re-exports 'MFunctor'.

    "Data.Foldable" re-exports 'Foldable' (the class name only)
-}
