{-| The core functionality for the 'Proxy' monad transformer

    Read "Pipes.Tutorial" if you want a practical tutorial explaining how to use
    this library.  The documentation in this module focuses more on the theory
    behind the type.
-}

{-# LANGUAGE CPP, RankNTypes #-}

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

    -- * ListT Monad Transformers
    -- $listT
    RespondT(..),
    RequestT(..),

    -- * Polymorphic Type Synonyms
    Pipe,
    Producer,
    Consumer,
    Effect,
    ListT,
    Client,
    Server,
    CoPipe,
    CoProducer,
    CoConsumer,
    CoListT,

    -- * Concrete Type Synonyms
    C,
    Producer',
    Consumer',
    Effect',
    ListT',
    Client',
    Server',
    CoProducer',
    CoConsumer',
    CoListT',

    -- ** Flipped operators
    (<-<),
    (<~<),
    (/</),
    (\<\),
    (<<-),
    (~<<),
    (//<),
    (<\\),

    -- * Laws
    -- $laws

    -- * Re-exports
    -- $reexports
    module Control.Monad,
    module Control.Monad.Trans.Class,
    module Control.Monad.Morph
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (forever, (>=>), (<=<))
import qualified Control.Monad as M
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Monoid (Monoid(mempty, mappend))
import Pipes.Internal

import Control.Monad.Morph (MFunctor(hoist))

-- | Run a self-contained 'Effect', converting it back to the base monad
runEffect :: (Monad m) => Effect' m r -> m r
runEffect p = go p
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
    The 'Proxy' type sits at the intersection of five categories:

    * The pull category

    * The push category

    * The request category

    * The respond category

    * The Kleisli category
-}

{- $pull
    The pull category lets you interleave pull-based streams, beginning from the
    most downstream component:

>         b'           c'                   c'
>         |            |                    |
>      +--|--+      +--|--+            +----|----+
>      |  v  |      |  v  |            |    v    |
>  a' <==   <== b' <==   <== c'    a' <==       <== c'
>      |  f  |      |  g  |     =      | f >-> g |
>  a  ==>   ==> b  ==>   ==> c     a  ==>       ==> c
>      |  |  |      |  |  |            |    |    |
>      +--|--+      +--|--+            +----|----+
>         v            v                    v
>         r            r                    r

    The pull category obeys the category laws:

> pull >-> f = f
>
> f >-> pull = f
>
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
    The push category lets you interleave push-based streams, beginning from the
    most upstream component:

>         a            b                    a
>         |            |                    |
>      +--|--+      +--|--+            +----|----+
>      |  v  |      |  v  |            |    v    |
>  a' <==   <== b' <==   <== c'    a' <==       <== c'
>      |  f  |      |  g  |     =      | f >~> g |
>  a  ==>   ==> b  ==>   ==> c     a  ==>       ==> c
>      |  |  |      |  |  |            |    |    |
>      +--|--+      +--|--+            +----|----+
>         v            v                    v
>         r            r                    r

    The pull category obeys the category laws:

> push >~> f = f
>
> f >~> push = f
>
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
    The request category lets you substitute 'request's with proxies.

>         b' <===\             c'                   c'
>         |      \\            |                    |
>      +--|--+    \\        +--|--+            +----|----+
>      |  v  |     \\       |  v  |            |    v    |
>  a' <==   <== y'  \== b' <==   <== y'    a' <==       <== y'
>      |  f  |              |  g  |     =      | f \>\ g |
>  a  ==>   ==> y   /=> b  ==>   ==> y     a  ==>       ==> y
>      |  |  |     //       |  |  |            |    |    |
>      +--|--+    //        +--|--+            +----|----+
>         v      //            v                    v
>         b =====/             c                    c

    The request category obeys the category laws:

> request \>\ f = f
>
> f \>\ request = f
>
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
    The respond category lets you substitute 'respond's with proxies.

>         a               /===> b                    a
>         |              //     |                    |
>      +--|--+          //   +--|--+            +----|----+
>      |  v  |         //    |  v  |            |    v    |
>  x' <==   <== b' <=\// x' <==   <== c'    x' <==       <== c'
>      |  f  |       \\      |  g  |     =      | f />/ g |
>  x  ==>   ==> b  ==/\\ x  ==>   ==> c     x  ==>       ==> c'
>      |  |  |         \\    |  |  |            |    |    |
>      +--|--+          \\   +--|--+            +----|----+
>         v              \\     v                    v
>         a'              \==== b'                   a'

    The respond category obeys the category laws:

> respond />/ f = f
>
> f />/ respond = f
>
> (f />/ g) />/ h = f />/ (g />/ h)
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

{- $listT
    The 'RespondT' monad transformer is equivalent to 'ListT' over the
    downstream output.  The 'RespondT' Kleisli category corresponds to the
    respond category.

    The 'RequestT' monad transformer is equivalent to 'ListT' over the upstream
    output.  The 'RequestT' Kleisli category corresponds to the request
    category.

    Unlike 'ListT' from @transformers@, these monad transformers are correct by
    construction and always satisfy the monad and monad transformer laws.
-}

-- | A monad transformer over a proxy's downstream output
newtype RespondT a' a b' m b = RespondT { runRespondT :: Proxy a' a b' b m b' }

instance (Monad m) => Functor (RespondT a' a b' m) where
    fmap f p = RespondT (runRespondT p //> \a -> respond (f a))

instance (Monad m) => Applicative (RespondT a' a b' m) where
    pure a = RespondT (respond a)
    mf <*> mx = RespondT (
        runRespondT mf //> \f ->
        runRespondT mx //> \x ->
        respond (f x) )

instance (Monad m) => Monad (RespondT a' a b' m) where
    return a = RespondT (respond a)
    m >>= f  = RespondT (runRespondT m //> \a -> runRespondT (f a))

instance MonadTrans (RespondT a' a b') where
    lift m = RespondT (do
        a <- lift m
        respond a )

instance (MonadIO m) => MonadIO (RespondT a' a b' m) where
    liftIO m = lift (liftIO m)

instance (Monad m, Monoid b') => Alternative (RespondT a' a b' m) where
    empty = RespondT (return mempty)
    p1 <|> p2 = RespondT (do
        r1 <- runRespondT p1
        r2 <- runRespondT p2
        return (mappend r1 r2) )

instance (Monad m, Monoid b') => M.MonadPlus (RespondT a' a b' m) where
    mzero = empty
    mplus = (<|>)

-- | A monad transformer over a proxy's upstream output
newtype RequestT a b' b m a' = RequestT { runRequestT :: Proxy a' a b' b m a }

instance (Monad m) => Functor (RequestT a b' b m) where
    fmap f p = RequestT (runRequestT p //< \a -> request (f a))

instance (Monad m) => Applicative (RequestT a b' b m) where
    pure a = RequestT (request a)
    mf <*> mx = RequestT (
        runRequestT mf //< \f ->
        runRequestT mx //< \x ->
        request (f x) )

instance (Monad m) => Monad (RequestT a b' b m) where
    return a = RequestT (request a)
    m >>= f  = RequestT (runRequestT m //< \a -> runRequestT (f a))

instance MonadTrans (RequestT a' a b') where
    lift m = RequestT (do
        a <- lift m
        request a )

instance (MonadIO m) => MonadIO (RequestT a b' b m) where
    liftIO m = lift (liftIO m)

instance (Monad m, Monoid a) => Alternative (RequestT a b' b m) where
    empty = RequestT (return mempty)
    p1 <|> p2 = RequestT (do
        r1 <- runRequestT p1
        r2 <- runRequestT p2
        return (mappend r1 r2) )

instance (Monad m, Monoid a) => M.MonadPlus (RequestT a b' b m) where
    mzero = empty
    mplus = (<|>)

-- | A unidirectional 'Proxy'.
type Pipe a b = Proxy () a () b

{-| A 'Pipe' that produces values

    'Producer's never 'request'.
-}
type Producer b m r = forall x' x . Proxy x' x () b m r

{-| A 'Pipe' that consumes values

    'Consumer's never 'respond'.
-}
type Consumer a m r = forall y' y . Proxy () a y' y m r

{-| An effect in the base monad

    'Effect's never 'request' or 'respond'.
-}
type Effect m r = forall x' x y' y . Proxy x' x y' y m r

-- | The list monad transformer
type ListT m b = forall x' x . RespondT x' x () m b

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's never 'respond'.
-}
type Client a' a m r = forall y' y . Proxy a' a y' y m r

{-| @Server b' b@ receives requests of type @b'@ and sends responses of type
    @b@.

    'Server's never 'request'.
-}
type Server b' b m r = forall x' x . Proxy x' x b' b m r

-- | A 'Pipe' where everything flows upstream
type CoPipe a' b' = Proxy a' () b' ()

{-| A 'CoPipe' that produces values flowing upstream

    'CoProducer's never 'respond'.
-}
type CoProducer a' = Proxy a' () () C

{-| A 'CoPipe' that consumes values flowing upstream

    'CoConsumer's never 'request'
-}
type CoConsumer b' = Proxy C () b' ()

-- | The list monad transformer for values flowing upstream
type CoListT = RequestT () () C

-- | The empty type, denoting a \'@C@\'losed end
data C = C -- Constructor not exported, but I include it to avoid EmptyDataDecls

-- | Like 'Producer', but with concrete types
type Producer' b = Proxy C () () b

-- | Like 'Consumer', but with concrete types
type Consumer' a = Proxy () a () C

-- | Like 'Effect', but with concrete types
type Effect' = Proxy C () () C

-- | Like 'ListT', but with concrete types
type ListT' = RespondT C () ()

-- | Like 'Server', but with concrete types
type Server' b' b = Proxy C () b' b

-- | Like 'Client', but with concrete types
type Client' a' a = Proxy a' a () C

-- | Like 'CoProducer', but with concrete types
type CoProducer' a' = Proxy a' () () C

-- | Like 'CoConsumer', but with concrete types
type CoConsumer' b' = Proxy C () b' ()

-- | Like 'CoListT', but with concrete types
type CoListT' = RequestT () () C

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

{- $laws
    In addition to the category laws, proxies also satisfy the following laws.

    ('\>\') and ('/>/') both define functors between Kleisli categories

> a \>\ (b >=> c) = (a \>\ b) >=> (a \>\ c)
>
> a \>\ return = return

> (b >=> c) />/ a = (b />/ a) >=> (c />/ a)
>
> return />/ a = return

-}

{- $reexports
    "Control.Monad" re-exports 'forever', ('>=>'), and ('<=<').

    "Control.Monad.Trans.Class" re-exports 'MonadTrans'.

    "Control.Monad.Morph" re-exports 'MFunctor'.
-}
