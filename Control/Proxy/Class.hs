{-| The 'Proxy' class defines the library's core API.  Everything else in this
    library builds on top of the 'Proxy' type class so that all proxy
    implementations and extensions can share the same standard library.
-}

{-# LANGUAGE Rank2Types, KindSignatures #-}

module Control.Proxy.Class (
    -- * Core proxy class
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

    -- * Synonyms
    C,
    Pipe,
    Producer,
    Consumer,
    CoPipe,
    CoProducer,
    CoConsumer,
    Client,
    Server,
    Session,

    -- * Laws
    -- $laws

    -- * Polymorphic proxies
    -- $poly
    ProxyInternal(..),
    MonadPlusP(..)
    ) where

import Control.Monad.IO.Class (MonadIO)

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

> read \>\ idPull >-> writer

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

{-| The core API for the @pipes@ library

    Minimal definition:

    * 'request'

    * 'respond'

    * ('->>')

    * ('>>~')

    * ('>\\')

    * ('//>')
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

    -- | @idPull = request >=> respond >=> idT@
    idPull :: (Monad m, Proxy p) => a' -> p a' a a' a m r
    idPull = go where
      go a' =
        request a' ?>= \a   ->
        respond a  ?>= \a'2 ->
        go a'2
    {- DO NOT replace 'go' with 'idPull' or ghc-7.4.2 will not terminate while
       compiling `pipes` -}

    -- | @(f ->> p)@ pairs each 'request' in @p@ with a 'respond' in @f@.
    (->>)
        :: (Monad m)
        => (b'  -> p a' a b' b m r)
        ->         p b' b c' c m r
        ->         p a' a c' c m r

    -- | @coidT = respond >=> request >=> idT@
    idPush :: (Monad m, Proxy p) => a -> p a' a a' a m r
    idPush = go where
      go a =
        respond a  ?>= \a' ->
        request a' ?>= \a2 ->
        go a2
    {- DO NOT replace 'go' with 'idPush' or ghc-7.4.2 will not terminate while
       compiling `pipes` -}

    -- | @(p >>~ f)@ pairs each 'respond' in @p@ with a 'request' in @f@.
    (>>~)
        :: (Monad m)
        =>        p a' a b' b m r
        -> (b  -> p b' b c' c m r)
        ->        p a' a c' c m r

{-| Pull-based composition

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

{-| Push-based composition

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

-- | The empty type, denoting a \'@C@\'losed end
data C = C -- Constructor not exported, but I include it to avoid EmptyDataDecls

-- | A unidirectional 'Proxy'.
type Pipe (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b = p () a () b

{-| A 'Pipe' that produces values

    'Producer's never 'request'.
-}
type Producer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b = p C () () b

{-| A 'Pipe' that consumes values

    'Consumer's never 'respond'.
-}
type Consumer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a = p () a () C

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
type Server (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b' b = p C () b' b

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's never 'respond'.
-}
type Client (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a = p a' a () C

{-| A self-contained 'Session', ready to be run by 'runProxy'

    'Session's never 'request' or 'respond'.
-}
type Session (p :: * -> * -> * -> * -> (* -> *) -> * -> *) = p C () () C

{- $laws
    First, all proxies are monads and must satify the monad laws using:
    
    * @(>>=) = (?>=)@

    * @return = return_P@

    Second, all proxies are monad transformers and must satisfy the monad
    transformer laws, using:

    * @lift = lift_P@

    Third, all proxies are functors in the category of monads and must satisfy
    the functor laws, using:

    *  @hoist = hoist_P@

    Fourth , all proxies form a \"pull-based\" category and must satisfy the
    category laws using:

    * @(.) = (>->)@

    * @id = idPull@


    Fifth, all proxies form a \"push-based\" category and must satisfy the
    category laws using:

    * @(.) = (>~>)@

    * @id = idPush@

    Sixth, all proxies form a \"request\" category and must satisfy the category
    laws using:

    * @(.) = (\\>\\)@

    * @id = request@

    Seventh, all proxies form a \"respond\" category and must satisfy the
    category laws using:

    * @(.) = (\/>\/)@

    * @id = respond@

    Eighth, ('\>\') and ('/>/') both define functors between Proxy Kleisli
    categories:

> a \>\ (b >=> c) = (a \>\ b) >=> (a \>\ c)
>
> a \>\ return = return

> (b >=> c) />/ a = (b />/ a) >=> (c />/ a)
>
> return />/ a = return

    Finally, all proxies must satisfy the following 'Proxy' laws:

> idPull = request >=> respond >=> idPull
>
> idPush = respond >=> request >=> idPush
>
> p1 >-> liftK f = liftK f
>
> p1 >-> (liftK f >=> respond >=> p2) = liftK f >=> respond >=> (p1 >-> p2)
>
> (liftK g >=> respond >=> p1) >-> (liftK f >=> request >=> liftK h >=> p2)
>     = liftK (f >=> g >=> h) >=> (p1 >-> p2)
>
> (liftK g >=> request >=> p1) >-> (liftK f >=> request >=> p2)
>     = liftK (f >=> g) >=> request >=> (p1 >~> p2)
>
> liftK f >~> p2 = liftK f
>
> (liftK f >=> request >=> p1) >~> p2 = liftK f >=> request >=> (p1 >~> p2)
>
> (liftK f >=> respond >=> liftK h >=> p1) >~> (liftK g >=> request >=> p2)
>     = liftK (f >=> g >=> h) >=> (p1 >~> p2)
>
> (liftK f >=> respond >=> p1) >~> (liftK g >=> respond >=> p2)
>     = liftK (f >=> g) >=> (p1 >-> p2)

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
