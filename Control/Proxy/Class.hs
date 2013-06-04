-- | This module defines the theoretical framework underpinning this library

{-# LANGUAGE Rank2Types, KindSignatures #-}

module Control.Proxy.Class (
    -- * The Proxy Class
    Proxy(..),

    -- * Composition operators
    (>->),
    (>~>),
    (\>\),
    (/>/),

    -- ** Flipped operators
    (<-<),
    (<~<),
    (/</),
    (\<\),
    (<<-),
    (~<<),
    (//<),
    (<\\),

    -- * ListT Monad Transformers
    -- $listT
    RespondT(..),
    RequestT(..),

    -- * Synonyms
    C,
    Pipe,
    Producer,
    Producer',
    Consumer,
    Consumer',
    CoPipe,
    CoProducer,
    CoConsumer,
    Client,
    Client',
    Server,
    Server',
    Session,
    Session',
    ProduceT,
    ProduceT',
    CoProduceT,

    -- * Laws
    -- $laws

    -- * Polymorphic proxies
    -- $poly
    ProxyInternal(..),
    MonadPlusP(..),

    -- * Deprecated
    -- $deprecate
    idT,
    coidT,
    ListT,
    runRespondK,
    runRequestK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Monoid (Monoid(mempty, mappend))

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
infixr 7 />/, <\\
infixr 8 /</, >\\
infixl 8 \>\, //<
infixl 1 ?>=  -- This should match the fixity of >>=

{-| The 'Proxy' class defines a 'Monad' that intersects four streaming
    categories:

    * The \"request\" category: 'request' and ('\>\')

    * The \"respond\" category: 'respond' and ('/>/')

    * The \"pull\" category: 'pull' and ('>->')

    * The \"push\" category: 'push' and ('>~>')

    This class requires the \"point-ful\" version of each category's composition
    operator for efficiency.

    Minimal definition:

    * 'request'

    * 'respond'

    * ('->>')

    * ('>>~')

    * ('>\\')

    * ('//>')

    * 'turn'
-}
class (ProxyInternal p) => Proxy p where
    {-| 'request' sends a value of type @a'@ upstream and receives a value of
        type @a@.
    -}
    request :: (Monad m) => a' -> p a' a b' b m a

    -- | @(f >\\\\ p)@ replaces each 'request' in @p@ with @f@.
    (>\\)
        :: (Monad m)
        => (b' -> p a' a x' x m b)
        ->        p b' b x' x m c
        ->        p a' a x' x m c

    {-| 'respond' sends a value of type @b@ downstream and receives a value of
        type @b'@.
    -}
    respond :: (Monad m) => b -> p a' a b' b m b'

    -- | @(p \/\/> f)@ replaces each 'respond' in @p@ with @f@.
    (//>)
        :: (Monad m)
        =>       p x' x b' b m a'
        -> (b -> p x' x c' c m b')
        ->       p x' x c' c m a'

    -- | @pull = request >=> respond >=> pull@
    pull :: (Monad m, Proxy p) => a' -> p a' a a' a m r
    pull = go where
      go a' =
        request a' ?>= \a   ->
        respond a  ?>= \a'2 ->
        go a'2
    {- DO NOT replace 'go' with 'push' or ghc-7.4.2 will not terminate while
       compiling `pipes` -}

    -- | @(f ->> p)@ pairs each 'request' in @p@ with a 'respond' in @f@.
    (->>)
        :: (Monad m)
        => (b'  -> p a' a b' b m r)
        ->         p b' b c' c m r
        ->         p a' a c' c m r

    -- | @push = respond >=> request >=> push@
    push :: (Monad m, Proxy p) => a -> p a' a a' a m r
    push = go where
      go a =
        respond a  ?>= \a' ->
        request a' ?>= \a2 ->
        go a2
    {- DO NOT replace 'go' with 'push' or ghc-7.4.2 will not terminate while
       compiling `pipes` -}

    -- | @(p >>~ f)@ pairs each 'respond' in @p@ with a 'request' in @f@.
    (>>~)
        :: (Monad m)
        =>        p a' a b' b m r
        -> (b  -> p b' b c' c m r)
        ->        p a' a c' c m r

    -- | 'turn' swaps 'request's and 'respond's
    turn :: (Monad m) => p a' a b' b m r -> p b b' a a' m r

{-| \"pull\" composition

> (f >-> g) x = f ->> g x

    Compose two proxies blocked on a 'respond', generating a new proxy blocked
    on a 'respond'
-}
(>->)
    :: (Monad m, Proxy p)
    => (b'  -> p a' a b' b m r)
    -> (c'_ -> p b' b c' c m r)
    -> (c'_ -> p a' a c' c m r)
f >-> g = \c' -> f ->> g c'
{-# INLINABLE (>->) #-}

{-| \"push\" composition

> (f >~> g) x = f x >>~ g

    Compose two proxies blocked on a 'request', generating a new proxy blocked
    on a 'request'
-}
(>~>)
    :: (Monad m, Proxy p)
    => (a_ -> p a' a b' b m r)
    -> (b  -> p b' b c' c m r)
    -> (a_ -> p a' a c' c m r)
k1 >~> k2 = \a -> k1 a >>~ k2
{-# INLINABLE (>~>) #-}

{-| \"request\" composition

> (f \>\ g) x = f >\\ g x

    Compose two folds, generating a new fold
-}
(\>\)
    :: (Monad m, Proxy p)
    => (b' -> p a' a x' x m b)
    -> (c' -> p b' b x' x m c)
    -> (c' -> p a' a x' x m c)
f \>\ g = \c' -> f >\\ g c'
{-# INLINABLE (\>\) #-}

{-| \"respond\" composition

> (f />/ g) x = f x //> g

    Compose two unfolds, generating a new unfold
-}
(/>/)
    :: (Monad m, Proxy p)
    => (a -> p x' x b' b m a')
    -> (b -> p x' x c' c m b')
    -> (a -> p x' x c' c m a')
f />/ g = \a -> f a //> g
{-# INLINABLE (/>/) #-}

-- | Equivalent to ('>->') with the arguments flipped
(<-<)
    :: (Monad m, Proxy p)
    => (c' -> p b' b c' c m r)
    -> (b' -> p a' a b' b m r)
    -> (c' -> p a' a c' c m r)
p1 <-< p2 = p2 >-> p1
{-# INLINABLE (<-<) #-}

-- | Equivalent to ('>~>') with the arguments flipped
(<~<)
    :: (Monad m, Proxy p)
    => (b -> p b' b c' c m r)
    -> (a -> p a' a b' b m r)
    -> (a -> p a' a c' c m r)
p1 <~< p2 = p2 >~> p1
{-# INLINABLE (<~<) #-}

-- | Equivalent to ('\>\') with the arguments flipped
(/</)
    :: (Monad m, Proxy p)
    => (c' -> p b' b x' x m c)
    -> (b' -> p a' a x' x m b)
    -> (c' -> p a' a x' x m c)
p1 /</ p2 = p2 \>\ p1
{-# INLINABLE (/</) #-}

-- | Equivalent to ('/>/') with the arguments flipped
(\<\)
    :: (Monad m, Proxy p)
    => (b -> p x' x c' c m b')
    -> (a -> p x' x b' b m a')
    -> (a -> p x' x c' c m a')
p1 \<\ p2 = p2 />/ p1
{-# INLINABLE (\<\) #-}

-- | Equivalent to ('->>') with the arguments flipped
(<<-)
    :: (Monad m, Proxy p)
    =>         p b' b c' c m r
    -> (b'  -> p a' a b' b m r)
    ->         p a' a c' c m r
k <<- p = p ->> k
{-# INLINABLE (<<-) #-}

-- | Equivalent to ('>>~') with the arguments flipped
(~<<)
    :: (Monad m, Proxy p)
    => (b  -> p b' b c' c m r)
    ->        p a' a b' b m r
    ->        p a' a c' c m r
k ~<< p = p >>~ k
{-# INLINABLE (~<<) #-}

-- | Equivalent to ('>\\') with the arguments flipped
(//<)
    :: (Monad m, Proxy p)
    =>        p b' b x' x m c
    -> (b' -> p a' a x' x m b)
    ->        p a' a x' x m c
p //< f = f >\\ p
{-# INLINABLE (//<) #-}

-- | Equivalent to ('//>') with the arguments flipped
(<\\)
    :: (Monad m, Proxy p)
    => (b -> p x' x c' c m b')
    ->       p x' x b' b m a'
    ->       p x' x c' c m a'
f <\\ p = p //> f
{-# INLINABLE (<\\) #-}

-- | A monad transformer over a proxy's downstream output
newtype RespondT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a b' m b =
    RespondT { runRespondT :: p a' a b' b m b' }

instance (Monad m, Proxy p) => Functor (RespondT p a' a b' m) where
    fmap f p = RespondT (runRespondT p //> \a -> respond (f a))

instance (Monad m, Proxy p) => Applicative (RespondT p a' a b' m) where
    pure a = RespondT (respond a)
    mf <*> mx = RespondT (
        runRespondT mf //> \f ->
        runRespondT mx //> \x ->
        respond (f x) )

instance (Monad m, Proxy p) => Monad (RespondT p a' a b' m) where
    return a = RespondT (respond a)
    m >>= f  = RespondT (runRespondT m //> \a -> runRespondT (f a))

instance (Proxy p) => MonadTrans (RespondT p a' a b') where
    lift m = RespondT (lift_P m ?>= \a -> respond a)

instance (MonadIO m, Proxy p) => MonadIO (RespondT p a' a b' m) where
    liftIO m = lift (liftIO m)

instance (Monad m, Proxy p, Monoid b')
       => Alternative (RespondT p a' a b' m) where
    empty = RespondT (return_P mempty)
    p1 <|> p2 = RespondT (
        runRespondT p1 ?>= \r1 ->
        runRespondT p2 ?>= \r2 ->
        return_P (mappend r1 r2) )

instance (Monad m, Proxy p, Monoid b') => MonadPlus (RespondT p a' a b' m) where
    mzero = empty
    mplus = (<|>)

{- $listT
    The 'RespondT' monad transformer is equivalent to 'ListT' over the
    downstream output.  The 'RespondT' Kleisli category corresponds to the
    \"respond\" category.

    The 'RequestT' monad transformer is equivalent to 'ListT' over the upstream
    output.  The 'RequestT' Kleisli category corresponds to the \"request\"
    category.

    Unlike 'ListT' from @transformers@, these monad transformers are correct by
    construction and always satisfy the monad and monad transformer laws.
-}


-- | A monad transformer over a proxy's upstream output
newtype RequestT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b' b m a' =
    RequestT { runRequestT :: p a' a b' b m a }

instance (Monad m, Proxy p) => Functor (RequestT p a b' b m) where
    fmap f p = RequestT (runRequestT p //< \a -> request (f a))

instance (Monad m, Proxy p) => Applicative (RequestT p a b' b m) where
    pure a = RequestT (request a)
    mf <*> mx = RequestT (
        runRequestT mf //< \f ->
        runRequestT mx //< \x ->
        request (f x) )

instance (Monad m, Proxy p) => Monad (RequestT p a b' b m) where
    return a = RequestT (request a)
    m >>= f  = RequestT (runRequestT m //< \a -> runRequestT (f a))

instance (Proxy p) => MonadTrans (RequestT p a' a b') where
    lift m = RequestT (lift_P m ?>= \a -> request a)

instance (MonadIO m, Proxy p) => MonadIO (RequestT p a b' b m) where
    liftIO m = lift (liftIO m)

instance (Monad m, Proxy p, Monoid a)
       => Alternative (RequestT p a b' b m) where
    empty = RequestT (return_P mempty)
    p1 <|> p2 = RequestT (
        runRequestT p1 ?>= \r1 ->
        runRequestT p2 ?>= \r2 ->
        return_P (mappend r1 r2) )

instance (Monad m, Proxy p, Monoid a) => MonadPlus (RequestT p a b' b m) where
    mzero = empty
    mplus = (<|>)

-- | The empty type, denoting a \'@C@\'losed end
data C = C -- Constructor not exported, but I include it to avoid EmptyDataDecls

-- | A unidirectional 'Proxy'.
type Pipe (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b = p () a () b

{-| A 'Pipe' that produces values

    'Producer's never 'request'.
-}
type Producer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b m r
    = forall a' a . p a' a () b m r

-- | Like 'Producer', but with concrete types to improve type inference
type Producer' (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b = p C () () b

{-| A 'Pipe' that consumes values

    'Consumer's never 'respond'.
-}
type Consumer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a m r
    = forall b' b . p () a b' b m r

-- | Like a 'Consumer', but with concrete types to improve type inference
type Consumer' (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a = p () a () C

-- | A 'Pipe' where everything flows upstream
type CoPipe (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' b' = p a' () b' ()

{-| A 'CoPipe' that produces values flowing upstream

    'CoProducer's never 'respond'.
-}
type CoProducer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' = p a' () () C

{-| A 'CoConsumer' that consumes values flowing upstream

    'CoConsumer's never 'request'.
-}
type CoConsumer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b' = p C () b' ()

{-| @Server b' b@ receives requests of type @b'@ and sends responses of type
    @b@.

    'Server's never 'request'.
-}
type Server (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b' b m r
    = forall a' a . p a' a b' b m r

-- | Like 'Server', but with concrete types to improve type inference
type Server' (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b' b = p C () b' b

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's never 'respond'.
-}
type Client (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a m r
    = forall b' b . p a' a b' b m r

-- | Like 'Client', but with concrete types to improve type inference
type Client' (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a = p a' a () C

{-| A self-contained 'Session', ready to be run by 'runProxy'

    'Session's never 'request' or 'respond'.
-}
type Session (p :: * -> * -> * -> * -> (* -> *) -> * -> *) m r
    = forall a' a b' b . p a' a b' b m r

-- | Like 'Session', but with concrete types to improve type inference
type Session' (p :: * -> * -> * -> * -> (* -> *) -> * -> *) = p C () () C

-- | 'ProduceT' is 'ListT' over the downstream output
type ProduceT p m b = forall a' a . RespondT p a' a () m b

-- | Like 'ProduceT', but with concrete types to improve type inference
type ProduceT' p = RespondT p C () ()

-- | 'CoProduceT' is 'ListT' over the upstream output
type CoProduceT p = RequestT p () () C

{- $laws
    First, all proxies sit at the intersection of five categories:

    * The Kleisli category (all proxies are monads)

> return >=> f = f
>
> f >=> return = f
>
> (f >=> g) >=> h = f >=> (g >=> h)

    * The \"request\" category

> request \>\ f = f
>
> f \>\ request = f
>
> (f \>\ g) \>\ h = f \>\ (g \>\ h)

    * The \"respond\" category

> respond />/ f = f
>
> f />/ respond = f
>
> (f />/ g) />/ h = f />/ (g />/ h)

    * The \"pull\" category

> pull >-> f = f
>
> f >-> pull = f
>
> (f >-> g) >-> h = (f >-> g) >-> h

    * The \"push\" category

> push >~> f = f
>
> f >~> push = f
>
> (f >~> g) >~> h = f >~> (g >~> h)

    Second, @(turn .)@ transforms each streaming category into its dual:

    * The \"request\" category

> turn . request = respond
>
> turn . (f \>\ g) = turn . f \<\ turn . g

    * The \"respond\" category

> turn . respond = request
>
> turn . (f />/ g) = turn . f /</ turn. g

    * The \"pull\" category

> turn . pull = push
>
> turn . (f >-> g) = turn . f <~< turn . g

    * The \"push\" category

> turn . push = pull
>
> turn . (f >~> g) = turn . f <-< turn . g

    Third, all proxies are monad transformers and must satisfy the monad
    transformer laws, using:

    * @lift = lift_P@

    Fourth, all proxies are functors in the category of monads and must satisfy
    the functor laws, using:

    *  @hoist = hoist_P@

    Fifth, ('\>\') and ('/>/') both define functors between Kleisli categories

> a \>\ (b >=> c) = (a \>\ b) >=> (a \>\ c)
>
> a \>\ return = return

> (b >=> c) />/ a = (b />/ a) >=> (c />/ a)
>
> return />/ a = return

    Sixth, all proxies must satisfy these additional 'Proxy' laws:

> p \>\ lift . f = lift . f
>
> p \>\ respond  = respond
>
> lift . f />/ p = lift . f
>
> request />/  p = request
>
> pull = request >=> respond >=> pull
>
> push = respond >=> request >=> push
>
> p1 >-> lift . f = lift . f
>
> p1 >-> (lift . f >=> respond >=> p2) = lift . f >=> respond >=> (p1 >-> p2)
>
> (lift . g >=> respond >=> p1) >-> (lift . f >=> request >=> lift . h >=> p2)
>     = lift . (f >=> g >=> h) >=> (p1 >-> p2)
>
> (lift . g >=> request >=> p1) >-> (lift . f >=> request >=> p2)
>     = lift . (f >=> g) >=> request >=> (p1 >~> p2)
>
> lift . f >~> p2 = lift . f
>
> (lift . f >=> request >=> p1) >~> p2 = lift . f >=> request >=> (p1 >~> p2)
>
> (lift . f >=> respond >=> lift . h >=> p1) >~> (lift . g >=> request >=> p2)
>     = lift . (f >=> g >=> h) >=> (p1 >~> p2)
>
> (lift . f >=> respond >=> p1) >~> (lift . g >=> respond >=> p2)
>     = lift . (f >=> g) >=> (p1 >-> p2)

-}

{- $poly
    The 'ProxyInternal' and 'MonadPlusP' type classes duplicate methods from
    more familiar type classes.  These duplicate methods serve two purposes.

    First, this library requires type class instances that would otherwise be
    impossible to define without providing higher-kinded constraints.  Rather
    than use the following illegal polymorphic constraint:

> instance (forall a' a b' b . MonadTrans (p a' a b' b)) => ...

    ... the instance can instead use the following Haskell98 constraint:

> instance (Proxy p) => ...

    Second, these type classes don't require the @FlexibleContexts@ extension
    to use and substantially clean up constraints in type signatures.  They
    convert messy constraints like this:

> p :: (MonadP (p a' a b' b m), MonadTrans (p a' a b' b)) => ...

      .. into cleaner and more general constraints like this:

> p :: (Proxy p) => ...

    'ProxyInternal' and 'MonadPlusP' exist solely for internal type class
    plumbing and I discourage you from using the methods in these classes
    unless you enjoy making your code less readable.  Instead, you can use all
    the original type classes as long as you embed your proxy code within at
    least one proxy transformer (or 'IdentityP' if don't use any transformers).
    The type-class machinery will then automatically convert the messier and
    less polymorphic constraints to the simpler and more general constraints.

    For example, consider the following almost-correct definition for @mapMD@
    (from "Control.Proxy.Prelude.Base"):

> import Control.Monad.Trans.Class
> import Control.Proxy
>
> mapMD f = foreverK $ \a' -> do
>     a <- request a'
>     b <- lift (f a)
>     respond b

    The compiler infers the following messy constraint:

> mapMD
>  :: (Monad m, Monad (p x a x b m), MonadTrans (p x a x b), Proxy p)
>  => (a -> m b) -> x -> p x a x b m r

    Instead, you can embed the code in the @IdentityP@ proxy transformer by
    wrapping it in 'runIdentityK':

> --        |difference|  
> mapMD f = runIdentityK $ foreverK $ \a' -> do
>     a <- request a'
>     b <- lift (f a)
>     respond b

    ... and now the compiler collapses all the constraints into the 'Proxy'
    constraint:

> mapMD :: (Monad m, Proxy p) => (a -> m b) -> x -> p x a x b m r

    You do not incur any performance penalty for writing polymorphic code or
    embedding it in 'IdentityP'.  This library employs several rewrite @RULES@
    which transform your polymorphic code into the equivalent type-specialized
    hand-tuned code.  These rewrite rules fire very robustly and they do not
    require any assistance on your part from compiler pragmas like @INLINE@,
    @NOINLINE@ or @SPECIALIZE@.

    If you nest proxies within proxies:

> example () = do
>     request ()
>     lift $ request ()
>     lift $ lift $ request ()

    ... then you can still keep the nice constraints using:

> example () = runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ do
>     request ()
>     lift $ request ()
>     lift $ lift $ request ()

    You don't need to use 'runIdentityP' \/ 'runIdentityK' if you use any other
    proxy transformers (In fact you can't, it's a type error).  The following
    code example illustrates this, where the 'throw' command (from the 'EitherP'
    proxy transformer) suffices to guide the compiler to the cleaner type
    signature:

> import Control.Monad
> import Control.Proxy
> import qualified Control.Proxy.Trans.Either as E
>
> example :: (Monad m, Proxy p) => () -> Producer (EitherP String p) Char m ()
> example () = do
>     c <- request ()
>     when (c == ' ') $ E.throw "Error: received space"
>     respond c
-}

{-| The @(ProxyInternal p)@ constraint is (basically) equivalent to the
    following polymorphic constraint:

> (forall a' a b' b m . (Monad m)
>     => Monad      (p a' a b' b m)
>     ,  MonadTrans (p a' a b' b  )
>     ,  MFunctor   (p a' a b' b m)
>     ,  MonadIO    (p a' a b' b m)
>     ) => ...
-}
class ProxyInternal p where
    return_P :: (Monad m) => r -> p a' a b' b m r
    (?>=)
        :: (Monad m)
        => p a' a b' b m r -> (r -> p a' a b' b m r') -> p a' a b' b m r'

    lift_P :: (Monad m) => m r -> p a' a b' b m r

    hoist_P
        :: (Monad m)
        => (forall r . m r  -> n r) -> (p a' a b' b m r' -> p a' a b' b n r')

    liftIO_P :: (MonadIO m) => IO r -> p a' a b' b m r

    thread_P
        :: (Monad m)
        => p a' a b' b m r -> s -> p (a', s) (a, s) (b', s) (b, s) m (r, s)

{-| The @(MonadPlusP p)@ constraint is equivalent to the following polymorphic
    constraint:

> (forall a' a b' b m . (Monad m) => MonadPlus (p a' a b' b m)) => ...
-}
class (Proxy p) => MonadPlusP p where
    mzero_P :: (Monad m) => p a' a b' b m r
    mplus_P
        :: (Monad m) => p a' a b' b m r -> p a' a b' b m r -> p a' a b' b m r

{- $deprecate
    These will be removed in version @4.0.0@
-}

idT :: (Monad m, Proxy p) => a' -> p a' a a' a m r
idT = pull
{-# INLINABLE idT #-}
{-# DEPRECATED idT "Use 'pull' instead" #-}

coidT :: (Monad m, Proxy p) => a -> p a' a a' a m r
coidT = push
{-# INLINABLE coidT #-}
{-# DEPRECATED coidT "Use 'push' instead" #-}

class (Proxy p) => ListT p where
{-# DEPRECATED ListT "Use 'Proxy' instead" #-}

runRespondK :: (q -> RespondT p a' a b' m b) -> (q -> p a' a b' b m b')
runRespondK k q = runRespondT (k q)
{-# INLINABLE runRespondK #-}
{-# DEPRECATED runRespondK "Use '(runRespondT .)' instead" #-}

runRequestK :: (q -> RequestT p a b' b m a') -> (q -> p a' a b' b m a)
runRequestK k q = runRequestT (k q)
{-# INLINABLE runRequestK #-}
{-# DEPRECATED runRequestK "Use '(runRequestK .)' instead" #-}
