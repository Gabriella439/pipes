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
    runEffect,

    -- * Categories
    -- $categories

    -- ** Pull
    -- $pull
    pull,
    (>->),
    (->>),

    -- ** Push
    -- $push
    push,
    (>~>),
    (>>~),

    -- ** Request
    -- $request
    request,
    (\>\),
    (>\\),

    -- ** Respond
    -- $respond
    respond,
    (/>/),
    (//>),

    -- ** Reflect
    -- $reflect
    reflect,

    -- * ListT
    ListT(..),

    -- * Iterable
    Iterable(..),

    -- * Utilities
    next,
    for,
    each,
    every,
    select,
    discard,

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

    -- ** Flipped operators
    (<-<),
    (<~<),
    (/</),
    (\<\),
    (<<-),
    (~<<),
    (//<),
    (<\\),

    -- * Re-exports
    -- $reexports
    module Control.Monad,
    module Control.Monad.Trans.Class,
    module Control.Monad.Morph
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (void, (>=>), (<=<))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Identity (IdentityT(runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Trans.Error (ErrorT(runErrorT))
import qualified Data.Foldable as F
import Pipes.Internal

-- Re-exports
import Control.Monad.Morph (MFunctor(hoist))

-- | Run a self-contained 'Effect', converting it back to the base monad
runEffect :: (Monad m) => Effect m r -> m r
runEffect = go
  where
    go p = case p of
        Request _ fa  -> go (fa  ())
        Respond _ fb' -> go (fb' ())
        M         m   -> m >>= go
        Pure      r   -> return r
{-# INLINABLE runEffect #-}

{- * Keep proxy composition lower in precedence than function composition, which
     is 9 at the time of of this comment, so that users can write things like:

> lift . k >-> p
>
> hoist f . k >-> p

   * Keep the priorities different so that users can mix composition operators
     like:

> up \>\ p />/ dn
>
> up >~> p >-> dn

   * Keep 'request' and 'respond' composition lower in precedence than 'pull'
     and 'push' composition, so that users can do:

> read \>\ pull >-> writer

   * I arbitrarily choose a lower priority for downstream operators so that lazy
     pull-based computations need not evaluate upstream stages unless absolutely
     necessary.
-}
infixr 5 <-<, ->>
infixl 5 >->, <<-
infixr 6 >~>, ~<<
infixl 6 <~<, >>~
infixl 7 \<\, //>
infixr 7 />/, <\\ -- GHC will raise a parse error if 
infixr 8 /</, >\\ -- either of these lines end with '\'
infixl 8 \>\, //<

{- $categories
    A 'Control.Category.Category' is a set of components that you can connect
    with a composition operator, ('Control.Category..'), that has an identity,
    'Control.Category.id'.  The ('Control.Category..') and 'Control.Category.id'    must satisfy the following three 'Control.Category.Category' laws:

> -- Left identity 
> id . f = f
>
> -- Right identity
> f . id = f
>
> -- Associativity
> (f . g) . h = f . (g . h)

    The 'Proxy' type sits at the intersection of five separate categories, four
    of which are named after their identity:

@
                     Identity   | Composition |  Point-ful
                  +-------------+-------------+-------------+
    pull category |    'pull'     |     '>->'     |     '->>'     |
    push category |    'push'     |     '>~>'     |     '>>~'     |
 request category |   'respond'   |     '\>\'     |     '>\\'     |
 respond category |   'request'   |     '/>/'     |     '//>'     |
 Kleisli category |   'return'    |     '>=>'     |     '>>='     |
                  +-------------+-------------+-------------+
@

    Each composition operator has a \"point-ful\" version, analogous to how
    ('>>=') is the point-ful version of ('>=>').

-}

{- $pull
    The 'pull' category interleaves pull-based streams, where control begins
    from the most downstream component.  You can more easily understand 'pull'
    and ('>->') by studying their types when specialized to 'Pipe's:

> pull  :: (Monad m)
>       =>  () -> Pipe a a m r
>
> (>->) :: (Monad m)
>       => (() -> Pipe a b m r)
>       => (() -> Pipe b c m r)
>       => (() -> Pipe a c m r)

    The 'pull' category provides the most beginner-friendly composition
    operator, because all pull-based 'Pipe's require an initial argument of
    type @()@.  This category also most closely matches Haskell's lazy and
    functional semantics since control begins from downstream and this category
    favors linear pipelines resembling function composition chains.  For this
    reason, all the utilities in "Pipes.Prelude" are built for the 'pull'
    category.

    In the fully general case, you can also send information upstream by
    invoking 'request' with a non-@()@ argument.  The upstream 'Proxy' will
    receive the first value through its initial argument and bind each
    subsequent value through the return value of 'respond':

>           b'               c'                     c'
>           |                |                      |
>      +----|----+      +----|----+            +----|----+
>      |    v    |      |    v    |            |    v    |
>  a' <==       <== b' <==       <== c'    a' <==       <== c'
>      |    f    |      |    g    |     =      | f >-> g |
>  a  ==>       ==> b  ==>       ==> c     a  ==>       ==> c
>      |    |    |      |    |    |            |    |    |
>      +----|----+      +----|----+            +----|----+
>           v                v                      v
>           r                r                      r
>
>  f        :: b' -> Proxy a' a b' b m r
>  g        :: c' -> Proxy b' b c' c m r
> (f >-> g) :: c' -> Proxy a' a c' c m r

    The 'pull' category obeys the category laws, where 'pull' is the identity
    and ('>->') is composition:

> -- Left identity
> pull >-> f = f
>
> -- Right identity
> f >-> pull = f
>
> -- Associativity
> (f >-> g) >-> h = f >-> (g >-> h)
-}

{-| Forward requests followed by responses:

> pull = request >=> respond >=> pull

    'pull' is the identity of the pull category.
-}
pull :: (Monad m) => a' -> Proxy a' a a' a m r
pull = go
  where
    go a' = Request a' (\a -> Respond a go)
{-# INLINABLE pull #-}

{-| Compose two proxies blocked on a 'respond', creating a new proxy blocked on
    a 'respond'

> (f >-> g) x = f ->> g x

    ('>->') is the composition operator of the pull category.
-}
(>->)
    :: (Monad m)
    => ( b' -> Proxy a' a b' b m r)
    -> (_c' -> Proxy b' b c' c m r)
    -> (_c' -> Proxy a' a c' c m r)
(fb' >-> fc') c' = fb' ->> fc' c'
{-# INLINABLE (>->) #-}

{-| @(f ->> p)@ pairs each 'request' in @p@ with a 'respond' in @f@.

    Point-ful version of ('>->')
-}
(->>)
    :: (Monad m)
    => (b' -> Proxy a' a b' b m r)
    ->        Proxy b' b c' c m r
    ->        Proxy a' a c' c m r
fb' ->> p = case p of
    Request b' fb  -> fb' b' >>~ fb
    Respond c  fc' -> Respond c (\c' -> fb' ->> fc' c')
    M          m   -> M (m >>= \p' -> return (fb' ->> p'))
    Pure       r   -> Pure r
{-# INLINABLE (->>) #-}

{- $push
    The 'push' category interleaves push-based streams, where control begins
    from the most upstream component.  You can more easily understand 'push' and
    ('>~>') by studying their types when specialized to 'Pipe's:

> push  :: (Monad m)
>       =>  a -> Pipe a a m r
>
> (>~>) :: (Monad m)
>       => (a -> Pipe a b m r)
>       => (b -> Pipe b c m r)
>       => (a -> Pipe a c m r)

    Push-based 'Pipe's require an extra argument of the same type as their input
    since they cannot begin until upstream pushes the first value.  You can
    think of these 'Pipe's as being initially blocked on a 'request'.

    The 'push' category is the best suited for directed acyclic graphs because
    it is the only category that permits an 'Control.Arrow.Arrow' instance when
    specialized to 'Pipe's.  The upcoming @pipes-arrows@ package will provide
    this 'Control.Arrow.Arrow' instance and corresponding utilities.

    In the fully general case, you can also send information upstream.  In fact,
    upstream flow in the 'push' category behaves identically to downstream flow
    in the 'pull' category.  Vice versa, downstream flow in the 'push' category
    behaves identically to upstream flow in the 'pull' category.  These two
    categories are duals of each other:

>           a                b                      a
>           |                |                      |
>      +----|----+      +----|----+            +----|----+
>      |    v    |      |    v    |            |    v    |
>  a' <==       <== b' <==       <== c'    a' <==       <== c'
>      |    f    |      |    g    |     =      | f >~> g |
>  a  ==>       ==> b  ==>       ==> c     a  ==>       ==> c
>      |    |    |      |    |    |            |    |    |
>      +----|----+      +----|----+            +----|----+
>           v                v                      v
>           r                r                      r
>
>  f        :: a -> Proxy a' a b' b m r
>  g        :: b -> Proxy b' b c' c m r
> (f >~> g) :: a -> Proxy a' a c' c m r

    The 'push' category obeys the category laws, where 'push' is the identity
    and ('>~>') is composition:

> -- Left identity
> push >~> f = f
>
> -- Right identity
> f >~> push = f
>
> -- Associativity
> (f >~> g) >~> h = f >~> (g >~> h)
-}

{-| Forward responses followed by requests

> push = respond >=> request >=> push

    'push' is the identity of the push category.
-}
push :: (Monad m) => a -> Proxy a' a a' a m r
push = go
  where
    go a = Respond a (\a' -> Request a' go)
{-# INLINABLE push #-}

{-| Compose two proxies blocked on a 'request', creating a new proxy blocked on
    a 'request'

> (f >~> g) x = f x >>~ g

    ('>~>') is the composition operator of the push category.
-}
(>~>)
    :: (Monad m)
    => (_a -> Proxy a' a b' b m r)
    -> ( b -> Proxy b' b c' c m r)
    -> (_a -> Proxy a' a c' c m r)
(fa >~> fb) a = fa a >>~ fb
{-# INLINABLE (>~>) #-}

{-| @(p >>~ f)@ pairs each 'respond' in @p@ with a 'request' in @f@.

    Point-ful version of ('>~>')
-}
(>>~)
    :: (Monad m)
    =>       Proxy a' a b' b m r
    -> (b -> Proxy b' b c' c m r)
    ->       Proxy a' a c' c m r
p >>~ fb = case p of
    Request a' fa  -> Request a' (\a -> fa a >>~ fb)
    Respond b  fb' -> fb' ->> fb b
    M          m   -> M (m >>= \p' -> return (p' >>~ fb))
    Pure       r   -> Pure r
{-# INLINABLE (>>~) #-}

{- $request
    The 'request' category lets you substitute 'request's with 'Proxy's.  You
    can more easily understand 'request' and ('\>\') by studying their types
    when specialized to 'Consumer's:

> request :: (Monad m)
>         =>  () -> Consumer a m a
>
> (\>\)   :: (Monad m)
>         => (() -> Consumer a m b)
>         => (() -> Consumer b m c)
>         => (() -> Consumer a m c)

    The composition operator, ('\>\'), composes folds, but in a way that is
    significantly more optimal than the equivalent code written in the 'pull' or
   'push' categories.  The disadvantage is that pipes that are not folds are
    awkward to write and use in this category (such as 'Pipes.Prelude.take').
    Instead, you should use the pull category if you want to compose pipes that
    are not folds.

    In the fully general case, composed folds can share the same downstream
    interface and can also parametrize each 'request' for additional input:

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
>
> 
>
>  f        :: b' -> Proxy a' a y' y m b
>  g        :: c' -> Proxy b' b y' y m c
> (f \>\ g) :: c' -> Proxy a' a y' y m c

    The 'request' category obeys the category laws, where 'request' is the
    identity and ('\>\') is composition:

> -- Left identity
> request \>\ f = f
>
> -- Right identity
> f \>\ request = f
>
> -- Associativity
> (f \>\ g) \>\ h = f \>\ (g \>\ h)
-}

{-| Send a value of type @a'@ upstream and block waiting for a reply of type @a@

    'request' is the identity of the request category.
-}
request :: (Monad m) => a' -> Proxy a' a y' y m a
request a' = Request a' Pure
{-# INLINABLE request #-}

{-| Compose two folds, creating a new fold

> (f \>\ g) x = f >\\ g x

    ('\>\') is the composition operator of the request category.
-}
(\>\)
    :: (Monad m)
    => (b' -> Proxy a' a y' y m b)
    -> (c' -> Proxy b' b y' y m c)
    -> (c' -> Proxy a' a y' y m c)
(fb' \>\ fc') c' = fb' >\\ fc' c'
{-# INLINABLE (\>\) #-}

{-| @(f >\\\\ p)@ replaces each 'request' in @p@ with @f@.

    Point-ful version of ('\>\')
-}
(>\\)
    :: (Monad m)
    => (b' -> Proxy a' a y' y m b)
    ->        Proxy b' b y' y m c
    ->        Proxy a' a y' y m c
fb' >\\ p0 = go p0
  where
    go p = case p of
        Request b' fb  -> fb' b' >>= \b -> go (fb b)
        Respond x  fx' -> Respond x (\x' -> go (fx' x'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       a   -> Pure a
{-# INLINABLE (>\\) #-}

{-# RULES
    "fb' >\\ (Request b' fb )" forall fb' b' fb  .
        fb' >\\ (Request b' fb ) = fb' b' >>= \b -> fb' >\\ fb b;
    "fb' >\\ (Respond x  fx')" forall fb' x  fx' .
        fb' >\\ (Respond x  fx') = Respond x (\x' -> fb' >\\ fx' x');
    "fb' >\\ (M          m  )" forall fb'    m   .
        fb' >\\ (M          m  ) = M (m >>= \p' -> return (fb' >\\ p'));
    "fb' >\\ (Pure    a     )" forall fb' a      .
        fb' >\\ (Pure    a     ) = Pure a;
  #-}

{- $respond
    The respond category lets you substitute 'respond's with 'Proxy's.  You can
    more easily understand 'respond' and ('/>/') by studying their types when
    specialized to 'Producer's:

> respond :: (Monad m)
>         =>  a -> Producer a m ()
>
> (/>/)   :: (Monad m)
>         => (a -> Producer b m ())
>         => (b -> Producer c m ())
>         => (a -> Producer c m ())

    The composition operator, ('/>/'), composes unfolds, but in a way that is
    significantly more optimal than the equivalent code written in the 'pull' or
   'push' categories.  The disadvantage is that pipes that are not unfolds are
    awkward to write and use in this category (such as 'Pipes.Prelude.take').
    Instead, you should use the push category to compose pipes that are not
    unfolds.

    In the fully general case, composed unfolds can share the same upstream
    interface and can also bind values from each 'respond':

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
>
>  f        :: a -> Proxy x' x b' b m a'
>  g        :: b -> Proxy x' x c' c m b'
> (f />/ g) :: a -> Proxy x' x b' b m a'

    The 'respond' category obeys the category laws, where 'respond' is the
    identity and ('/>/') is composition:

> -- Left identity
> respond />/ f = f
>
> -- Right identity
> f />/ respond = f
>
> -- Associativity
> (f />/ g) />/ h = f />/ (g />/ h)

    Note that when written using 'for', these laws summarize our intuition for
   how 'for' should iterate over 'Producer's:

> for (respond x) f = f x
>
> for m respond = m
>
> for (for m f) g = for m (\a -> for (f a) g)
-}

{-| Send a value of type @b@ downstream and block waiting for a reply of type
    @b'@

    'respond' is the identity of the respond category.
-}
respond :: (Monad m) => a -> Proxy x' x a' a m a'
respond a = Respond a Pure
{-# INLINABLE respond #-}

{-| Compose two unfolds, creating a new unfold

> (f />/ g) x = f x //> g

    ('/>/') is the composition operator of the respond category.
-}
(/>/)
    :: (Monad m)
    => (a -> Proxy x' x b' b m a')
    -> (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x c' c m a')
(fa />/ fb) a = fa a //> fb
{-# INLINABLE (/>/) #-}

{-| @(p \/\/> f)@ replaces each 'respond' in @p@ with @f@.

    Point-ful version of ('/>/')
-}
(//>)
    :: (Monad m)
    =>       Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    ->       Proxy x' x c' c m a'
p0 //> fb = go p0
  where
    go p = case p of
        Request x' fx  -> Request x' (\x -> go (fx x))
        Respond b  fb' -> fb b >>= \b' -> go (fb' b')
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       a   -> Pure a
{-# INLINABLE (//>) #-}

{-# RULES
    "(Request x' fx ) //> fb" forall x' fx  fb .
        (Request x' fx ) //> fb = Request x' (\x -> fx x //> fb);
    "(Respond b  fb') //> fb" forall b  fb' fb .
        (Respond b  fb') //> fb = fb b >>= \b' -> fb' b' //> fb;
    "(M          m  ) //> fb" forall    m   fb .
        (M          m  ) //> fb = M (m >>= \p' -> return (p' //> fb));
    "(Pure    a     ) //> fb" forall a      fb .
        (Pure    a     ) //> fb = Pure a;
  #-}

{- $reflect
    @(reflect .)@ transforms each streaming category into its dual:

    * The request category is the dual of the respond category

> reflect . request = respond
>
> reflect . (f \>\ g) = reflect . f \<\ reflect . g

> reflect . respond = request
>
> reflect . (f />/ g) = reflect . f /</ reflect . g

    * The pull category is the dual of the push category

> reflect . pull = push
>
> reflect . (f >-> g) = reflect . f <~< reflect . g

> reflect . push = pull
>
> reflect . (f >~> g) = reflect . f <-< reflect . g
-}

-- | Switch the upstream and downstream ends
reflect :: (Monad m) => Proxy a' a b' b m r -> Proxy b b' a a' m r
reflect = go
  where
    go p = case p of
        Request a' fa  -> Respond a' (\a  -> go (fa  a ))
        Respond b  fb' -> Request b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure       r   -> Pure r
{-# INLINABLE reflect #-}

{-| The list monad transformer, which extends a monad with non-determinism

    'return' corresponds to 'respond', yielding a single value

    ('>>=') corresponds to 'for', calling the second computation once for each
    time the first computation 'respond's.
-}
newtype ListT m a = ListT { runListT :: Producer a m () }

instance (Monad m) => Functor (ListT m) where
    fmap f p = ListT (runListT p //> \a -> respond (f a))

instance (Monad m) => Applicative (ListT m) where
    pure a = ListT (respond a)
    mf <*> mx = ListT (
        runListT mf //> \f ->
        runListT mx //> \x ->
        respond (f x) )

instance (Monad m) => Monad (ListT m) where
    return a = ListT (respond a)
    m >>= f  = ListT (runListT m //> \a -> runListT (f a))

instance MonadTrans ListT where
    lift m = ListT (do
        a <- lift m
        respond a )

instance (MonadIO m) => MonadIO (ListT m) where
    liftIO m = lift (liftIO m)

instance (Monad m) => Alternative (ListT m) where
    empty = ListT (return ())
    p1 <|> p2 = ListT (do
        runListT p1
        runListT p2 )

instance (Monad m) => MonadPlus (ListT m) where
    mzero = empty
    mplus = (<|>)

instance MFunctor ListT where
    hoist morph = ListT . hoist morph . runListT

{-| 'Iterable' generalizes 'Data.Foldable.Foldable', converting effectful
    containers to 'Producer's.
-}
class Iterable t where
    toListT :: (Monad m) => t m a -> ListT m a

instance Iterable ListT where
    toListT = id

instance Iterable IdentityT where
    toListT m = ListT $ do
        a <- lift $ runIdentityT m
        respond a

instance Iterable MaybeT where
    toListT m = ListT $ do
        x <- lift $ runMaybeT m
        case x of
            Nothing -> return ()
            Just a  -> respond a

instance Iterable (ErrorT e) where
    toListT m = ListT $ do
        x <- lift $ runErrorT m
        case x of
            Left  _ -> return ()
            Right a -> respond a

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
        Pure      r  -> return (Left r)

{-| @(for p f)@ replaces each 'respond' in @p@ with @f@.

    Synonym for ('//>')
-}
for
    :: (Monad m)
    =>       Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    ->       Proxy x' x c' c m a'
for = (//>)
{-# INLINE for #-}

-- | Convert a 'F.Foldable' to a 'Producer'
each :: (Monad m, F.Foldable f) => f a -> Producer a m ()
each = F.mapM_ respond
{-# INLINABLE each #-}

-- | Convert an 'Iterable' to a 'Producer'
every :: (Monad m, Iterable t) => t m a -> Producer a m ()
every= runListT . toListT
{-# INLINABLE every #-}

{-| Convert a 'Producer' to a 'ListT'

    Synonym for 'ListT'
-}
select :: Producer b m () -> ListT m b
select = ListT
{-# INLINE select #-}

-- | Discards all input values
discard :: (Monad m) => a -> Effect' m ()
discard _ = return ()
{-# INLINABLE discard #-}

-- | The empty type, denoting a closed output
data X

{-| An effect in the base monad

    'Effect's never 'request' or 'respond'.
-}
type Effect = Proxy X () () X

-- | 'Producer's only 'respond' and never 'request'.
type Producer b = Proxy X () () b

-- | A unidirectional 'Proxy'.
type Pipe a b = Proxy () a () b

{-| 'Consumer's only 'request' and never 'respond'.
-}
type Consumer a = Proxy () a () X

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's never 'respond'.
-}
type Client a' a = Proxy a' a () X

{-| @Server b' b@ receives requests of type @b'@ and sends responses of type
    @b@.

    'Server's never 'request'.
-}
type Server b' b = Proxy X () b' b

-- | Like 'Effect', but with a polymorphic type
type Effect' m r = forall x' x y' y . Proxy x' x y' y m r

-- | Like 'Producer', but with a polymorphic type
type Producer' b m r = forall x' x . Proxy x' x () b m r

-- | Like 'Consumer', but with a polymorphic type
type Consumer' a m r = forall y' y . Proxy () a y' y m r

-- | Like 'Server', but with a polymorphic type
type Server' b' b m r = forall x' x . Proxy x' x b' b m r

-- | Like 'Client', but with a polymorphic type
type Client' a' a m r = forall y' y . Proxy a' a y' y m r

-- | Equivalent to ('>->') with the arguments flipped
(<-<)
    :: (Monad m)
    => (c' -> Proxy b' b c' c m r)
    -> (b' -> Proxy a' a b' b m r)
    -> (c' -> Proxy a' a c' c m r)
p1 <-< p2 = p2 >-> p1
{-# INLINABLE (<-<) #-}

-- | Equivalent to ('>~>') with the arguments flipped
(<~<)
    :: (Monad m)
    => (b -> Proxy b' b c' c m r)
    -> (a -> Proxy a' a b' b m r)
    -> (a -> Proxy a' a c' c m r)
p1 <~< p2 = p2 >~> p1
{-# INLINABLE (<~<) #-}

-- | Equivalent to ('\>\') with the arguments flipped
(/</)
    :: (Monad m)
    => (c' -> Proxy b' b x' x m c)
    -> (b' -> Proxy a' a x' x m b)
    -> (c' -> Proxy a' a x' x m c)
p1 /</ p2 = p2 \>\ p1
{-# INLINABLE (/</) #-}

-- | Equivalent to ('/>/') with the arguments flipped
(\<\)
    :: (Monad m)
    => (b -> Proxy x' x c' c m b')
    -> (a -> Proxy x' x b' b m a')
    -> (a -> Proxy x' x c' c m a')
p1 \<\ p2 = p2 />/ p1
{-# INLINABLE (\<\) #-}

-- | Equivalent to ('->>') with the arguments flipped
(<<-)
    :: (Monad m)
    =>         Proxy b' b c' c m r
    -> (b'  -> Proxy a' a b' b m r)
    ->         Proxy a' a c' c m r
k <<- p = p ->> k
{-# INLINABLE (<<-) #-}

-- | Equivalent to ('>>~') with the arguments flipped
(~<<)
    :: (Monad m)
    => (b  -> Proxy b' b c' c m r)
    ->        Proxy a' a b' b m r
    ->        Proxy a' a c' c m r
k ~<< p = p >>~ k
{-# INLINABLE (~<<) #-}

-- | Equivalent to ('>\\') with the arguments flipped
(//<)
    :: (Monad m)
    =>        Proxy b' b x' x m c
    -> (b' -> Proxy a' a x' x m b)
    ->        Proxy a' a x' x m c
p //< f = f >\\ p
{-# INLINABLE (//<) #-}

-- | Equivalent to ('//>') with the arguments flipped
(<\\)
    :: (Monad m)
    => (b -> Proxy x' x c' c m b')
    ->       Proxy x' x b' b m a'
    ->       Proxy x' x c' c m a'
f <\\ p = p //> f
{-# INLINABLE (<\\) #-}

{- $reexports
    "Control.Monad" re-exports 'void', ('>=>'), and ('<=<').

    "Control.Monad.Trans.Class" re-exports 'MonadTrans'.

    "Control.Monad.Morph" re-exports 'MFunctor'.
-}
