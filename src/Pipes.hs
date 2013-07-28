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
    for,

    -- ** Await
    -- $await
    await,
    (\>\),
    feed,

    -- ** Push
    -- $push
    push,
    (>~>),
    (>>~),

    -- ** Pull
    -- $pull
    pull,
    (>->),
    (->>),
    cat,
    (~>),

    -- ** Reflect
    -- $reflect
    reflect,

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

    -- * Flipped operators
    (\<\),
    (/</),
    (<~<),
    (~<<),
    (<-<),
    (<<-),
    (<~),

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
import Pipes.Lift (evalStateP)

-- Re-exports
import Control.Monad.Morph (MFunctor(hoist))

-- | Run a self-contained 'Effect', converting it back to the base monad
run :: (Monad m) => Effect m r -> m r
run = go
  where
    go p = case p of
        Await _ fa  -> go (fa  ())
        Yield _ fb' -> go (fb' ())
        M       m   -> m >>= go
        Pure    r   -> return r
{-# INLINABLE run #-}

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

   * Keep 'await' and 'yield' composition lower in precedence than 'pull'
     and 'push' composition, so that users can do:

> read \>\ pull >-> writer

   * I arbitrarily choose a lower priority for downstream operators so that lazy
     pull-based computations need not evaluate upstream stages unless absolutely
     necessary.
-}
infixr 4 />/
infixl 4 \<\ -- GHC will raise a parse error if either of these lines ends
infixl 5 \>\ -- with '\', which is why this comment is here
infixr 5 /</
infixr 6 ->>
infixl 6 <<-
infixr 7 <-<, ~<< 
infixl 7 >->, >>~
infixr 8 >~>
infixl 8 <~<

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
   yield category |   'yield'     |     '/>/'     |     'for'     |
   await category |   'await'     |     '\>\'     |     'feed'    |
    push category |   'push'      |     '>~>'     |     '>>~'     |
    pull category |   'pull'      |     '>->'     |     '->>'     |
 Kleisli category |   'return'    |     '>=>'     |     '>>='     |
                  +-------------+-------------+-------------+
@

    Each composition operator has a \"point-ful\" version, analogous to how
    ('>>=') is the point-ful version of ('>=>').  For example, 'for' is the
    point-ful version of ('\>\').
-}

{- $yield
    The 'yield' category closely corresponds to the generator design pattern and
    consists of three operations, which you can think of as having the following
    types:

> -- Produces one output value
> yield :: (Monad m)
>       =>  a -> Producer a m ()
>
> -- Loops over a 'Producer', handling outputs with a 'Producer' or 'Effect'
> for   :: (Monad m)               |  for   :: (Monad m)
>       =>       Producer a m ()   |        =>       Producer a m ()
>       -> (a -> Producer b m ())  |        -> (a -> Effect     m ())
>       ->       Producer b m ()   |        ->       Effect     m ()
>
> -- Composes unfolds or handlers
> (/>/) :: (Monad m)               |  (/>/) :: (Monad m)
>       => (a -> Producer b m ())  |        => (a -> Producer b m ())
>       => (b -> Producer c m ())  |        -> (b -> Effect     m ())
>       => (a -> Producer c m ())  |        -> (a -> Effect     m ())

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
yield a = Yield a Pure
{-# INLINABLE yield #-}

{-| Compose two unfolds, creating a new unfold

> (f />/ g) x = for (f x) g

    ('/>/') is the composition operator of the yield category.
-}
(/>/)
    :: (Monad m)
    => (a -> Proxy x' x b' b m a')
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x c' c m a')
    -- ^
(fa />/ fb) a = for (fa a) fb
{-# INLINABLE (/>/) #-}

{-| @(for p f)@ replaces each 'yield ' in @p@ with @f@.

    Point-ful version of ('/>/')
-}
for
    :: (Monad m)
    =>       Proxy x' x b' b m a'
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    ->       Proxy x' x c' c m a'
    -- ^
for p0 fb = go p0
  where
    go p = case p of
        Await x' fx  -> Await x' (\x -> go (fx x))
        Yield b  fb' -> fb b >>= \b' -> go (fb' b')
        M        m   -> M (m >>= \p' -> return (go p'))
        Pure     a   -> Pure a
{-# INLINABLE for #-}

{-# RULES
    "for (Await x' fx ) fb" forall x' fx  fb .
        for (Await x' fx ) fb = Await x' (\x -> for (fx x) fb);
    "for (Yield b  fb') fb" forall b  fb' fb .
        for (Yield b  fb') fb = fb b >>= \b' -> for (fb' b') fb;
    "for (M        m  ) fb" forall    m   fb .
        for (M        m  ) fb = M (m >>= \p' -> return (for p' fb));
    "for (Pure    a   ) fb" forall a      fb .
        for (Pure  a     ) fb = Pure a;
  #-}

{- $await
    The 'await' category closely corresponds to the iteratee design pattern and
    consists of three operations, which you can think of as having the following
    types:

> -- Consumes one input value
> await :: (Monad m)
>       =>  () -> Consumer a m a
>
> -- Loops over a 'Consumer', supplying inputs with a 'Consumer' or 'Effect'
> feed  :: (Monad m)               |  feed  :: (Monad m)
>       =>        Consumer b m c   |        =>        Consumer b m c
>       -> (() -> Consumer a m b)  |        -> (() -> Effect     m b)
>       ->        Consumer a m c   |        ->        Effect     m c
>
> -- Composes folds or suppliers
> (\>\) :: (Monad m)               |  (\>\) :: (Monad m)
>       => (() -> Consumer a m b)  |        => (() -> Effect     m a)
>       => (() -> Consumer b m c)  |        -> (() -> Consumer a m b)
>       => (() -> Consumer a m c)  |        -> (() -> Effect     m b)

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

> -- Feeding a single 'await' simplifies to function application
> feed (await x) f = f x
>
> -- Feeding with an 'await' is the same as not feeding at all
> feed m await = m
>
> -- Nested feed loops can become sequential feed loops if the inner loop body
> -- ignores the outer loop variable
> feed m (\a -> feed (f a) g) = feed (feed m f) g

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
await :: (Monad m) => a' -> Proxy a' a y' y m a
await a' = Await a' Pure
{-# INLINABLE await #-}

{-| Compose two folds, creating a new fold

> (f \>\ g) x = feed (g x) f

    ('\>\') is the composition operator of the await category.
-}
(\>\)
    :: (Monad m)
    => (b' -> Proxy a' a y' y m b)
    -- ^
    -> (c' -> Proxy b' b y' y m c)
    -- ^
    -> (c' -> Proxy a' a y' y m c)
    -- ^
(fb' \>\ fc') c' = feed (fc' c') fb'
{-# INLINABLE (\>\) #-}

{-| @(feed p f)@ replaces each 'await' in @p@ with @f@.

    Point-ful version of ('\>\')
-}
feed
    :: (Monad m)
    =>        Proxy b' b y' y m c
    -- ^
    -> (b' -> Proxy a' a y' y m b)
    -- ^
    ->        Proxy a' a y' y m c
    -- ^
feed p0 fb' = go p0
  where
    go p = case p of
        Await b' fb  -> fb' b' >>= \b -> go (fb b)
        Yield x  fx' -> Yield x (\x' -> go (fx' x'))
        M        m   -> M (m >>= \p' -> return (go p'))
        Pure     a   -> Pure a
{-# INLINABLE feed #-}

{-# RULES
    "feed (Await b' fb ) fb'" forall fb' b' fb  .
        feed (Await b' fb ) fb' = fb' b' >>= \b -> feed (fb b) fb';
    "feed (Yield x  fx') fb'" forall fb' x  fx' .
        feed (Yield x  fx') fb' = Yield x (\x' -> feed (fx' x') fb');
    "feed (M          m  ) fb'" forall fb'    m   .
        feed (M        m  ) fb' = M (m >>= \p' -> return (feed p' fb'));
    "feed (Pure    a     ) fb'" forall fb' a      .
        feed (Pure  a     ) fb' = Pure a;
  #-}

{- $push
    The 'push' category closely corresponds to push-based Unix pipes and
    consists of three operations, which you can think of as having the following
    types:

> -- 'push' retransmits every values
> push  :: (Monad m)
>       =>  a -> Pipe a a m r
>
> -- '~>' transforms a 'Producer' by applying a 'Pipe' or 'Consumer' downstream
> (>>~) :: (Monad m)              |  (>>~) :: (Monad m)
>       =>       Producer a m r   |        =>       Producer a m r
>       -> (a -> Pipe   a b m r)  |        -> (a -> Consumer a m r)
>       ->       Producer b m r   |        ->       Effect     m r
>
> -- '>~>' connects 'Pipe's and 'Consumer's
> (>~>) :: (Monad m)              |  (>~>) :: (Monad m)
>       => (a -> Pipe   a b m r)  |        => (a -> Pipe   a b m r)
>       => (b -> Pipe   b c m r)  |        -> (b -> Consumer b m r)
>       => (a -> Pipe   a c m r)  |        -> (a -> Consumer a m r)

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

    In the fully general case, you can also send information upstream by
    invoking 'await' with a non-@()@ argument.  The upstream 'Proxy' will
    receive this argument through the return value of 'yield':

> push  :: (Monad m)
>       =>  a -> Proxy a' a a' a m r
>
>           a
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
> (>~>) :: (Monad m)
>       => (a -> Proxy a' a b' b m r)
>       -> (b -> Proxy b' b c' c m r)
>       -> (a -> Proxy a' a c' c m r)
>
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

-}

{-| Forward responses followed by requests

> push = yield >=> await >=> push

    'push' is the identity of the push category.
-}
push :: (Monad m) => a -> Proxy a' a a' a m r
push = go
  where
    go a = Yield a (\a' -> Await a' go)
{-# INLINABLE push #-}

{-| Compose two proxies blocked 'await'ing data, creating a new proxy blocked
    'await'ing data

> (f >~> g) x = f x ~> g

    ('>~>') is the composition operator of the push category.
-}
(>~>)
    :: (Monad m)
    => (_a -> Proxy a' a b' b m r)
    -- ^
    -> ( b -> Proxy b' b c' c m r)
    -- ^
    -> (_a -> Proxy a' a c' c m r)
    -- ^
(fa >~> fb) a = fa a >>~ fb
{-# INLINABLE (>~>) #-}

{-| @(p >>~ f)@ pairs each 'yield' in @p@ with an 'await' in @f@.

    Point-ful version of ('>~>')
-}
(>>~)
    :: (Monad m)
    =>       Proxy a' a b' b m r
    -- ^
    -> (b -> Proxy b' b c' c m r)
    -- ^
    ->       Proxy a' a c' c m r
    -- ^
p >>~ fb = case p of
    Await a' fa  -> Await a' (\a -> fa a >>~ fb)
    Yield b  fb' -> fb' ->> fb b
    M        m   -> M (m >>= \p' -> return (p' >>~ fb))
    Pure     r   -> Pure r
{-# INLINABLE (>>~) #-}

{- $pull
    The 'pull' category closely corresponds to pull-based Unix pipes and
    consists of three operations, which you can think of as having the following
    types:

> -- 'pull' retransmits all values
> pull  :: (Monad m)
>       =>  () -> Pipe a a m r
>
> -- '->>' transforms a 'Consumer' by applying a 'Pipe' or 'Producer' upstream
> (->>) :: (Monad m)               |  (->>) :: (Monad m)
>       => (() -> Pipe   a b m r)  |        => (() -> Producer b m r)
>       ->        Consumer b m r   |        ->        Consumer b m r
>       ->        Consumer a m r   |        ->        Effect     m r
>
> -- '>->' connects two 'Pipe's or 'Producer's
> (>->) :: (Monad m)               |  (>->) :: (Monad m)
>       => (() -> Pipe   a b m r)  |        -> (() -> Producer b m r)
>       -> (() -> Pipe   b c m r)  |        -> (() -> Pipe   b c m r)
>       -> (() -> Pipe   a c m r)  |        -> (() -> Producer c m r)

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

    For unidirectional Unix-like pipes, you can use the following simpler
    operations, which you can think of as having the following types:

> cat  :: (Monad m)
>      => Pipe a a m r
>
> (~>) :: (Monad m)     |  (~>) :: (Monad m)       |  (~>) :: (Monad m) 
>      => Pipe a b m r  |       => Producer b m r  |       => Pipe   a b m r
>      -> Pipe b c m r  |       -> Pipe   b c m r  |       -> Consumer a m r
>      -> Pipe a c m r  |       -> Producer c m r  |       -> Consumer b m r

    When you write the 'pull' category laws in terms of ('~>') and 'cat', you
    get the category laws for Unix pipes:

> -- Useless use of 'cat'
> cat ~> f = f
>
> -- Redirecting stdout to 'cat' does nothing
> f ~> cat = f
>
> -- The Unix pipe operator is associative
> (f ~> g) ~> h = f ~> (g ~> h)

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
> (>->) :: (Monad m)
>       -> (b' -> Proxy a' a b' b m r)
>       -> (c' -> Proxy b' b c' c m r)
>       -> (c' -> Proxy a' a c' c m r)
>
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

-}

{-| Forward requests followed by responses:

> pull = await >=> yield >=> pull

    'pull' is the identity of the pull category.
-}
pull :: (Monad m) => a' -> Proxy a' a a' a m r
pull = go
  where
    go a' = Await a' (\a -> Yield a go)
{-# INLINABLE pull #-}

{-| Compose two proxies blocked 'yield'ing data, creating a new proxy blocked
    'yield'ing data

> (f >-> g) x = f ->> g x

    ('>->') is the composition operator of the pull category.
-}
(>->)
    :: (Monad m)
    => ( b' -> Proxy a' a b' b m r)
    -- ^
    -> (_c' -> Proxy b' b c' c m r)
    -- ^
    -> (_c' -> Proxy a' a c' c m r)
    -- ^
(fb' >-> fc') c' = fb' ->> fc' c'
{-# INLINABLE (>->) #-}

{-| @(f ->> p)@ pairs each 'await' in @p@ with a 'yield' in @f@.

    Point-ful version of ('>->')
-}
(->>)
    :: (Monad m)
    => (b' -> Proxy a' a b' b m r)
    -- ^
    ->        Proxy b' b c' c m r
    -- ^
    ->        Proxy a' a c' c m r
    -- ^
fb' ->> p = case p of
    Await b' fb  -> fb' b' >>~ fb
    Yield c  fc' -> Yield c (\c' -> fb' ->> fc' c')
    M        m   -> M (m >>= \p' -> return (fb' ->> p'))
    Pure     r   -> Pure r
{-# INLINABLE (->>) #-}

-- | Unidirectional identity, named after the Unix @cat@ program
cat :: (Monad m) => Pipe a a m r
cat = pull ()
{-# INLINABLE cat #-}

-- | Unidirectional composition, analogous to the Unix pipe operator: @|@.
(~>)
    :: (Monad m)
    => Proxy a' a () b m r
    -- ^
    -> Proxy () b c' c m r
    -- ^
    -> Proxy a' a c' c m r
    -- ^
p1 ~> p2 = (\() -> p1) ->> p2
{-# INLINABLE (~>) #-}

{- $reflect
    @(reflect .)@ transforms each streaming category into its dual:

    * The await category is the dual of the yield category

> reflect . yield = await
>
> reflect . (f />/ g) = reflect . f /</ reflect . g

> reflect . await = yield
>
> reflect . (f \>\ g) = reflect . f \<\ reflect . g

    * The pull category is the dual of the push category

> reflect . push = pull
>
> reflect . (f >~> g) = reflect . f <-< reflect . g

> reflect . pull = push
>
> reflect . (f >-> g) = reflect . f <~< reflect . g
-}

-- | Switch the upstream and downstream ends
reflect :: (Monad m) => Proxy a' a b' b m r -> Proxy b b' a a' m r
reflect = go
  where
    go p = case p of
        Await a' fa  -> Yield a' (\a  -> go (fa  a ))
        Yield b  fb' -> Await b  (\b' -> go (fb' b'))
        M        m   -> M (m >>= \p' -> return (go p'))
        Pure  r      -> Pure r
{-# INLINABLE reflect #-}

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
        Await _ fu -> go (fu ())
        Yield a fu -> return (Right (a, fu ()))
        M       m  -> m >>= go
        Pure  r    -> return (Left r)

-- | Convert a 'F.Foldable' to a 'Producer'
each :: (Monad m, F.Foldable f) => f a -> Producer' a m ()
each = F.mapM_ yield
{-# INLINABLE each #-}

-- | Convert an 'Iterable' to a 'Producer'
every :: (Monad m, Iterable t) => t m a -> Producer' a m ()
every it = feed (list (toListT it)) discard
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
    r <- feed (for (hoist lift p) dn) up
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
        a <- await ()
        lift $ put (Just a)
        return a
    dn _ = return ()
{-# INLINABLE tee #-}

{-| Transform a unidirectional 'Pipe' to a bidirectional 'Pipe'

> generalize (f ~> g) = generalize f >-> generalize g
>
> generalize cat = pull
-}
generalize :: (Monad m) => Pipe a b m r -> x -> Proxy x a x b m r
generalize p x0 = evalStateP x0 $ feed (for (hoist lift p) dn) up
  where
    up () = do
        x <- lift get
        await x
    dn a = do
        x <- yield a
        lift $ put x
{-# INLINABLE generalize #-}

-- | The empty type, denoting a closed output
data X

{-| An effect in the base monad

    'Effect's never 'await' nor 'yield'.
-}
type Effect = Proxy X () () X

-- | 'Producer's only 'yield' and never 'await'.
type Producer b = Proxy X () () b

-- | A unidirectional 'Proxy'.
type Pipe a b = Proxy () a () b

{-| 'Consumer's only 'await' and never 'yield'.
-}
type Consumer a = Proxy () a () X

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's only 'await' and never 'yield'.
-}
type Client a' a = Proxy a' a () X

{-| @Server b' b@ receives requests of type @b'@ and sends responses of type
    @b@.

    'Server's only 'yield' and never 'await'.
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

-- | Equivalent to ('/>/') with the arguments flipped
(\<\)
    :: (Monad m)
    => (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x b' b m a')
    -- ^
    -> (a -> Proxy x' x c' c m a')
    -- ^
p1 \<\ p2 = p2 />/ p1
{-# INLINABLE (\<\) #-}

-- | Equivalent to ('\>\') with the arguments flipped
(/</)
    :: (Monad m)
    => (c' -> Proxy b' b x' x m c)
    -- ^
    -> (b' -> Proxy a' a x' x m b)
    -- ^
    -> (c' -> Proxy a' a x' x m c)
    -- ^
p1 /</ p2 = p2 \>\ p1
{-# INLINABLE (/</) #-}

-- | Equivalent to ('>~>') with the arguments flipped
(<~<)
    :: (Monad m)
    => (b -> Proxy b' b c' c m r)
    -- ^
    -> (a -> Proxy a' a b' b m r)
    -- ^
    -> (a -> Proxy a' a c' c m r)
    -- ^
p1 <~< p2 = p2 >~> p1
{-# INLINABLE (<~<) #-}

-- | Equivalent to ('>>~') with the arguments flipped
(~<<)
    :: (Monad m)
    => (b  -> Proxy b' b c' c m r)
    -- ^
    ->        Proxy a' a b' b m r
    -- ^
    ->        Proxy a' a c' c m r
    -- ^
k ~<< p = p >>~ k
{-# INLINABLE (~<<) #-}

-- | Equivalent to ('>->') with the arguments flipped
(<-<)
    :: (Monad m)
    => (c' -> Proxy b' b c' c m r)
    -- ^
    -> (b' -> Proxy a' a b' b m r)
    -- ^
    -> (c' -> Proxy a' a c' c m r)
    -- ^
p1 <-< p2 = p2 >-> p1
{-# INLINABLE (<-<) #-}

-- | Equivalent to ('->>') with the arguments flipped
(<<-)
    :: (Monad m)
    =>         Proxy b' b c' c m r
    -- ^
    -> (b'  -> Proxy a' a b' b m r)
    -- ^
    ->         Proxy a' a c' c m r
    -- ^
k <<- p = p ->> k
{-# INLINABLE (<<-) #-}

-- | Equivalent to ('~>') with the arguments flipped
(<~)
    :: (Monad m)
    => Proxy () b c' c m r
    -- ^
    -> Proxy a' a () b m r
    -- ^
    -> Proxy a' a c' c m r
    -- ^
p2 <~ p1 = p1 ~> p2
{-# INLINABLE (<~) #-}

{- $reexports
    "Control.Monad" re-exports ('>=>') and ('<=<').

    "Control.Monad.Trans.Class" re-exports 'MonadTrans'.

    "Control.Monad.Morph" re-exports 'MFunctor'.

    "Data.Foldable" re-exports 'Foldable' (the class name only)
-}
