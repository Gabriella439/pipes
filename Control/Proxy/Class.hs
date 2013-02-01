{-| The 'Proxy' class defines the library's core API.  Everything else in this
    library builds exclusively on top of the 'Proxy' type class so that all
    proxy implementations and extensions can share the same standard library.

    Several of these type classes duplicate methods from familiar type-classes
    (such as ('?>=') duplicating ('>>=')).  You do NOT need to use these
    duplicate methods.  Instead, read the \"Polymorphic proxies\" section below
    which explains their purpose and how they help clean up type signatures. -}

{-# LANGUAGE Rank2Types #-}

module Control.Proxy.Class (
    -- * Core proxy class
    Proxy(..),
    idT,
    coidT,
    (>->),
    (<-<),
    (>~>),
    (<~<),
    (<<-),
    (~<<),

    -- * request/respond substitution
    Interact(..),
    (\>\),
    (/</),
    (\<\),
    (/>/),
    (//<),
    (<\\),

    -- * Laws
    -- $laws

    -- * Polymorphic proxies
    -- $poly
    MonadP(..),
    MonadTransP(..),
    MFunctorP(..),
    MonadPlusP(..),
    MonadIOP(..)
    ) where

import Control.Monad.IO.Class (MonadIO)

-- Documentation imports
import Control.Monad.Trans.Class (lift)
import Control.MFunctor(hoist)

{- * I make educated guesses about which associativy is most efficient for each
     operator.
   * Keep proxy composition lower in precedence than function composition, which
     is 9 at the time of of this comment, so that users can write things like:

> lift . k >-> p
>
> hoist f . k >-> p
-}
infixr 7 <-<, ->>
infixl 7 >->, <<-
infixr 7 >~>, ~<<
infixl 7 <~<, >>~
infixr 8 /</, >\\
infixl 8 \>\, //<
infixl 8 \<\, //>
infixr 8 />/, <\\
infixl 1 ?>= -- This should match the fixity of >>=

-- | The core API for the @pipes@ library
class (MonadP p, MonadTransP p, MFunctorP p) => Proxy p where
    {-| 'request' input from upstream, passing an argument with the request

        @request a'@ passes @a'@ as a parameter to upstream that upstream may
        use to decide what response to return.  'request' binds the upstream's
        response of type @a@ to its own return value. -}
    request :: (Monad m) => a' -> p a' a b' b m a

    {-| 'respond' with an output for downstream and bind downstream's next
        'request'
          
        @respond b@ satisfies a downstream 'request' by supplying the value @b@.
        'respond' blocks until downstream 'request's a new value and binds the
        argument of type @b'@ from the next 'request' as its return value. -}
    respond :: (Monad m) => b -> p a' a b' b m b'

    -- | Connect an upstream 'Proxy' that handles all 'request's
    (->>)
     :: (Monad m)
     => (b'  -> p a' a b' b m r)
     ->         p b' b c' c m r
     ->         p a' a c' c m r

    -- | Connect a downstream 'Proxy' that handles all 'respond's
    (>>~)
     :: (Monad m)
     =>        p a' a b' b m r
     -> (b  -> p b' b c' c m r)
     ->        p a' a c' c m r

{-| Compose two proxies blocked on a 'respond', generating a new proxy blocked
    on a 'respond'

    Begins from the downstream end and satisfies every 'request' with a
    'respond'

> k1 >-> k2 = \c' -> k1 ->> k2 c'

    ('>->') accepts arbitary Kleisli arrows for its downstream argument because
    its inferred type is more general than composition requires.  This still
    gives correct behavior.
-}
(>->)
 :: (Monad m, Proxy p)
 => (b'  -> p a' a b' b m r)
 -> (c'_ -> p b' b c' c m r)
 -> (c'_ -> p a' a c' c m r)
k1 >-> k2 = \c' -> k1 ->> k2 c'

-- | Equivalent to ('>->') with the arguments flipped
(<-<)
 :: (Monad m, Proxy p)
 => (c' -> p b' b c' c m r)
 -> (b' -> p a' a b' b m r)
 -> (c' -> p a' a c' c m r)
p1 <-< p2 = p2 >-> p1

{-| Compose two proxies blocked on a 'request', generating a new proxy blocked
    on a 'request'

    Begins from the upstream end and satisfies every 'respond' with a
    'request'

> k1 >~> k2 = \a -> k1 a >>~ k2

    ('>~>') accepts arbitary Kleisli arrows for its upstream argument because
    its inferred type is more general than composition requires.  This still
    gives correct behavior. -}
(>~>)
 :: (Monad m, Proxy p)
 => (a_ -> p a' a b' b m r)
 -> (b  -> p b' b c' c m r)
 -> (a_ -> p a' a c' c m r)
k1 >~> k2 = \a -> k1 a >>~ k2

-- | Equivalent to ('>~>') with the arguments flipped
(<~<)
 :: (Monad m, Proxy p)
 => (b -> p b' b c' c m r)
 -> (a -> p a' a b' b m r)
 -> (a -> p a' a c' c m r)
p1 <~< p2 = p2 >~> p1

-- | Equivalent to ('->>') with the arguments flipped
(<<-)
 :: (Monad m, Proxy p)
 =>         p b' b c' c m r
 -> (b'  -> p a' a b' b m r)
 ->         p a' a c' c m r
k <<- p = p ->> k

-- | Equivalent to ('>>~') with the arguments flipped
(~<<)
 :: (Monad m, Proxy p)
 => (b  -> p b' b c' c m r)
 ->        p a' a b' b m r
 ->        p a' a c' c m r
k ~<< p = p >>~ k

{-| 'idT' forwards requests followed by responses

> idT = request >=> respond >=> idT
-}
idT :: (Monad m, Proxy p) => a' -> p a' a a' a m r
idT = go where
    go a' =
        request a' ?>= \a   ->
        respond a  ?>= \a'2 ->
        go a'2
-- idT = foreverK $ request >=> respond

{-| 'coidT' forwards responses followed by requests

> coidT = respond >=> request >=> coidT
-}
coidT :: (Monad m, Proxy p) => a -> p a' a a' a m r
coidT = go where
    go a =
        respond a  ?>= \a' ->
        request a' ?>= \a2 ->
        go a2
-- coidT = foreverK $ respond >=> request

-- | Two extra Proxy categories
class (Proxy p) => Interact p where
    -- | @f >\\\\ p@ replaces all 'request's in @p@ with @f@
    (>\\) :: (Monad m)
          => (b' -> p a' a x' x m b)
          ->        p b' b x' x m c
          ->        p a' a x' x m c

    -- | @p \/\/> f@ replaces all 'respond's in @p@ with @f@
    (//>) :: (Monad m)
          =>       p x' x b' b m a'
          -> (b -> p x' x c' c m b')
          ->       p x' x c' c m a'

{-| @f \\>\\ g@ replaces all 'request's in 'g' with 'f'.

    Point-free version of ('>\\')
-}
(\>\) :: (Monad m, Interact p)
      => (b' -> p a' a x' x m b)
      -> (c' -> p b' b x' x m c)
      -> (c' -> p a' a x' x m c)
f \>\ g = \c' -> f >\\ g c'

-- | Equivalent to ('\>\') with the arguments flipped
(/</) :: (Monad m, Interact p)
      => (c' -> p b' b x' x m c)
      -> (b' -> p a' a x' x m b)
      -> (c' -> p a' a x' x m c)
p1 /</ p2 = p2 \>\ p1

{-| @f \/>\/ g@ replaces all 'respond's in 'f' with 'g'.

    Point-free version of ('//>')
-}
(/>/) :: (Monad m, Interact p)
      => (a -> p x' x b' b m a')
      -> (b -> p x' x c' c m b')
      -> (a -> p x' x c' c m a')
f />/ g = \a -> f a //> g

-- | @f \\<\\ g@ replaces all 'respond's in 'g' with 'f'.
(\<\) :: (Monad m, Interact p)
      => (b -> p x' x c' c m b')
      -> (a -> p x' x b' b m a')
      -> (a -> p x' x c' c m a')
p1 \<\ p2 = p2 />/ p1

-- | Equivalent to ('>\\') with the arguments flipped
(//<) :: (Monad m, Interact p)
      =>        p b' b x' x m c
      -> (b' -> p a' a x' x m b)
      ->        p a' a x' x m c
p //< f = f >\\ p

-- | Equivalent to ('//>') with the arguments flipped
(<\\) :: (Monad m, Interact p)
      => (b -> p x' x c' c m b')
      ->       p x' x b' b m a'
      ->       p x' x c' c m a'
f <\\ p = p //> f

{- $laws
    The 'Proxy' class defines an interface to all core proxy capabilities that
    all proxy-like types must implement.

    First, all proxies must support a bidirectional flow of information, defined
    by:

    * ('>->')

    * ('>~>')

    * 'request'

    * 'respond'

    Intuitively, both @p1 >-> p2@ and @p1 >~> p2@ pair each 'request' in @p2@
    with a 'respond' in @p1@.  ('>->') accepts proxies blocked on 'respond' and
    begins from the downstream end, whereas ('>~>') accepts proxies blocked on
    'request' and begins from the upstream end.

    Second, all proxies are monads, defined by their 'MonadP' instance, which
    must satify the monad laws using @(>>=) = (?>=)@ and @return = return_P@.

    Third, all proxies are monad transformers, defined by their 'MonadTransP'
    instance, which must satisfy the monad transformer laws, using
    @lift = lift_P@.

    Fourth, all proxies are functors in the category of monads, defined by
    their 'MFunctorP' instance, which must satisfy the functor laws, using
    @hoist = hoist_P@.

    Fifth, all proxies form two streaming categories:

    * ('>->') and 'idT' form the \"pull-based\" category:

> Define: idT = request >=> respond >=> idT
> Define: k1 >-> k2 = \c' -> k1 ->> k2 c'
>
> idT >-> p = p
>
> p >-> idT = p
>
> (p1 >-> p2) >-> p3 = p1 >-> (p2 >-> p3)

    * ('>~>') and 'coidT' form the \"push-based\" category:

> Define: coidT = respond >=> request >=> coidT
> Define: k1 >~> k2 = \a -> k1 a >>~ k2
>
> coidT >~> p = p
>
> p >~> coidT = p
>
> (p1 >~> p2) >~> p3 = p1 >~> (p2 >~> p3)

    Also, all proxies must satisfy the following 'Proxy' laws:

> -- Define: liftK = (lift .)
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

    The 'Interact' class defines the ability to:
    
    * Replace existing 'request' commands using ('\>\')

    * Replace existing 'respond' commands using ('/>/')
    
    Laws:

    * ('\>\') and 'request' form a Kleisli category:

> return = request
>
> (>>=) = (//<)
>
> fmap f p = p //< (request . f)
>
> join p = p //< id
>
> request \>\ f = f
>
> f \>\ request = f
>
> (f \>\ g) \>\ h = f \>\ (g \>\ h)

    * ('/>/') and 'respond' form a Kleisli category:

> return = respond
>
> (>>=) = (//>)
>
> fmap f p = p //> (respond . f)
>
> join p = p //< id
>
> respond />/ f = f
>
> f />/ respond = f
>
> (f />/ g) />/ h = f />/ (g />/ h)

    Additionally, ('\>\') and ('/>/') both define functors between Kleisli
    categories:

> a \>\ (b >=> c) = (a \>\ b) >=> (a \>\ c)
>
> a \>\ return = return

> (b >=> c) />/ a = (b />/ a) >=> (c />/ a)
>
> return />/ a = return
-}

{- $poly
    Many of these type classes contain methods which copy methods from more
    familiar type classes.  These duplicate methods serve two purposes.

    First, this library requires type class instances that would otherwise be
    impossible to define without providing higher-kinded constraints.  Rather
    than use the following illegal polymorphic constraint:

> instance (forall a' a b' b . MonadTrans (p a' a b' b)) => ...

      ... the instance can instead use the following Haskell98 constraint:

> instance (MonadTransP p) => ...

    Second, these type classes don't require the @FlexibleContexts@ extension
    to use and substantially clean up constraints in type signatures.  They
    convert messy constraints like this:

> p :: (MonadP (p a' a b' b m), MonadTrans (p a' a b' b)) => ...

      .. into cleaner and more general constraints like this:

> P :: (Proxy p) => ...

    These type classes exist solely for internal plumbing and you should never
    directly use the duplicate methods from them.  Instead, you can use all the
    original type classes as long as you embed your proxy code within at least
    one proxy transformer (or 'IdentityP' if don't use any transformers).  The
    type-class machinery will then automatically convert the messier and less
    polymorphic constraints to the simpler and more general constraints.

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

{-| The @(MonadP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . (Monad m) => Monad (p a' a b' b m)) => ...
-}
class MonadP p where
    return_P :: (Monad m) => r -> p a' a b' b m r
    (?>=)
     :: (Monad m)
     => p a' a b' b m r -> (r -> p a' a b' b m r') -> p a' a b' b m r'

{-| The @(MonadTransP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b . MonadTrans (p a' a b' b)) => ...
-}
class MonadTransP p where
    lift_P :: (Monad m) => m r -> p a' a b' b m r

{-| The @(MFunctorP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . (Monad m) => MFunctor (p a' a b' b m)) => ...
-}
class MFunctorP p where
    hoist_P
     :: (Monad m)
     => (forall r . m r  -> n r) -> (p a' a b' b m r' -> p a' a b' b n r')

{-| The @(MonadPlusP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . (Monad m) => MonadPlus (p a' a b' b m)) => ...
-}
class (Proxy p) => MonadPlusP p where
    mzero_P :: (Monad m) => p a' a b' b m r
    mplus_P
     :: (Monad m) => p a' a b' b m r -> p a' a b' b m r -> p a' a b' b m r

{-| The @(MonadIOP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . (MonadIO m) => MonadIO (p a' a b' b m)) => ...
-}
class (Proxy p) => MonadIOP p where
    liftIO_P :: (MonadIO m) => IO r -> p a' a b' b m r
