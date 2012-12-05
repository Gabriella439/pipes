{-# LANGUAGE Rank2Types #-}

{-| This module provides an abstract interface to 'Proxy'-like behavior, so that
    multiple proxy implementations and proxy transformers can share the same
    library of utility proxies.

    Several of these type classes, including 'Proxy', duplicate methods from
    other type-classes (such as ('?>=') duplicating ('>>=')) in order to work
    around Haskell's lack of polymorphic constraints.  You do NOT need
    to use these duplicate methods, which exist solely to plumb internal
    type class machinery and clean up type signatures.  Instead, read the
    \"Polymorphic proxies\" section below to learn how to write clean proxy
    code. -}

module Control.Proxy.Class (
    -- * Core proxy class
    Proxy(..),
    idPush,
    idPull,
    (<-<),
    (<~<),
    -- * request/respond substitution
    Interact(..),
    (/</),
    (\<\),
    -- * Polymorphic proxies
    -- $poly
    MonadPlusP(..),
    MonadIOP(..),
    MFunctorP(..),
    ) where

import Control.Monad.IO.Class (MonadIO)

{- * I make educated guesses about which associativy is most efficient for each
     operator.
   * Keep proxy composition lower in precedence than function composition, which
     is 9 at the time of of this comment, so that users can write things like:

> lift . k >-> p
>
> hoist f . k >-> p
-}
infixr 7 <-<
infixl 7 >->
infixr 8 /</
infixl 8 \>\
infixl 8 \<\
infixr 8 />/
infixl 1 ?>= -- This should match the fixity of >>=

{-| The 'Proxy' class defines an interface to all core proxy capabilities that
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

    Second, all proxies are monads, defined by:

    * 'return_P'

    * ('?>=')

    These must satify the monad laws using @(>>=) = (?>=)@ and
    @return = return_P@.

    Third, all proxies are monad transformers, defined by:

    * 'lift_P'

    This must satisfy the monad transformer laws, using @lift = lift_P@.

    Additionally, all 'Proxy' instances must satisfy the 'Proxy' laws:

    * ('>->') and 'idPull' form a category:

> Define: idPull = request >=> respond >=> idPull
>
> idPull >-> p = p
>
> p >-> idPull = p
>
> (p1 >-> p2) >-> p3 = p1 >-> (p2 >-> p3)

    * ('>~>') and 'idPush' form a category:

> Define: idPush = respond >=> request >=> idPush
>
> idPush >~> p = p
>
> p >~> idPush = p
>
> (p1 >~> p2) >~> p3 = p1 >~> (p2 >~> p3)

    * Additionally:

> -- Define: mapK = (lift .)
>
> p1 >-> mapK f = mapK f
>
> p1 >-> (mapK f >=> respond >=> p2) = mapK f >=> respond >=> (p1 >-> p2)
>
> (mapK g >=> respond >=> p1) >-> (mapK f >=> request >=> mapK h >=> p2)
>     = mapK (f >=> g >=> h) >=> (p1 >-> p2)
>
> (mapK g >=> request >=> p1) >-> (mapK f >=> request >=> p2)
>     = mapK (f >=> g) >=> request >=> (p1 >~> p2)
>
> mapK f >~> p2 = mapK f
>
> (mapK f >=> request >=> p1) >~> p2 = mapK f >=> request >=> (p1 >~> p2)
>
> (mapK f >=> respond >=> mapK h >=> p1) >~> (mapK g >=> request >=> p2)
>     = mapK (f >=> g >=> h) >=> (p1 >~> p2)
>
> (mapK f >=> respond >=> p1) >~> (mapK g >=> respond >=> p2)
>     = mapK (f >=> g) >=> (p1 >-> p2)
-}
class Proxy p where
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

    {-| Compose two proxies blocked on a 'respond'

        Begins from the downstream end and satisfies every 'request' with a
        'respond' -}
    (>->)
     :: (Monad m)
     => (b' -> p a' a b' b m r)
     -> (c' -> p b' b c' c m r)
     -> (c' -> p a' a c' c m r)

    {-| Compose two proxies blocked on a 'request'

        Begins from the upstream end and satisfies every 'respond' with a
        'request' -}
    (>~>)
     :: (Monad m)
     => (a -> p a' a b' b m r)
     -> (b -> p b' b c' c m r)
     -> (a -> p a' a c' c m r)

    {-| 'return_P' is identical to 'return', except with a more polymorphic
        constraint. -}
    return_P :: (Monad m) => r -> p a' a b' b m r

    {-| ('?>=') is identical to ('>>='), except with a more polymorphic
        constraint. -}
    (?>=)
     :: (Monad m)
     => p a' a b' b m r -> (r -> p a' a b' b m r') -> p a' a b' b m r'

    {-| 'lift_P' is identical to 'lift', except with a more polymorphic
        constraint. -}
    lift_P :: (Monad m) => m r -> p a' a b' b m r

{-| 'idPull' forwards requests followed by responses

> idPull = request >=> respond >=> idPull
-}
idPull :: (Monad m, Proxy p) => a' -> p a' a a' a m r
idPull = go where
    go a' =
        request a' ?>= \a   ->
        respond a  ?>= \a'2 ->
        go a'2
-- idPull = foreverK $ request >=> respond

{-| 'idPush' forwards responses followed by requests

> idPush = respond >=> request >=> idPush
-}
idPush :: (Monad m, Proxy p) => a -> p a' a a' a m r
idPush = go where
    go a =
        respond a  ?>= \a' ->
        request a' ?>= \a2 ->
        go a2
-- idPush = foreverK $ respond >=> request

{-| Compose two proxies blocked on a 'respond'

    Begins from the downstream end and satisfies every 'request' with a
    'respond' -}
(<-<)
 :: (Monad m, Proxy p)
 => (c' -> p b' b c' c m r)
 -> (b' -> p a' a b' b m r)
 -> (c' -> p a' a c' c m r)
p1 <-< p2 = p2 >-> p1

{-| Compose two proxies blocked on a 'request'

    Begins from the upstream end and satisfies every 'respond' with a
    'request' -}
(<~<)
 :: (Monad m, Proxy p)
 => (b -> p b' b c' c m r)
 -> (a -> p a' a b' b m r)
 -> (a -> p a' a c' c m r)
p1 <~< p2 = p2 >~> p1

{-| This class exists primarily for theoretical interest and to justify some of
    the functor laws for the 'ProxyTrans' type class.  You probably do not need
    to use these operators.

    The 'Interact' class defines the ability to:
    
    * Replace existing 'request' commands using ('\>\')

    * Replace existing 'respond' commands using ('/>/')
    
    Minimal definition:

    * ('\>\')

    * ('/>/')

    Laws:

    * ('\>\') and 'request' form a category:

> request \>\ f = f
>
> f \>\ request = f
>
> (f \>\ g) \>\ h = f \>\ (g \>\ h)

    * ('/>/') and 'respond' form a category:

> respond />/ f = f
>
> f />/ respond = f
>
> (f />/ g) />/ h = f />/ (g />/ h)
-}
class Interact p where
    -- | @f \\>\\ g@ replaces all 'request's in 'g' with 'f'.
    (\>\) :: (Monad m)
          => (b' -> p a' a x' x m b)
          -> (c' -> p b' b x' x m c)
          -> (c' -> p a' a x' x m c)

    -- | @f \/>\/ g@ replaces all 'respond's in 'f' with 'g'.
    (/>/) :: (Monad m)
          => (a -> p x' x b' b m a')
          -> (b -> p x' x c' c m b')
          -> (a -> p x' x c' c m a')

-- | @f \/<\/ g@ replaces all 'request's in 'f' with 'g'.
(/</) :: (Monad m, Interact p)
      => (c' -> p b' b x' x m c)
      -> (b' -> p a' a x' x m b)
      -> (c' -> p a' a x' x m c)
p1 /</ p2 = p2 \>\ p1

-- | @f \\<\\ g@ replaces all 'respond's in 'g' with 'f'.
(\<\) :: (Monad m, Interact p)
      => (b -> p x' x c' c m b')
      -> (a -> p x' x b' b m a')
      -> (a -> p x' x c' c m a')
p1 \<\ p2 = p2 />/ p1

{- $poly
    All of the below type classes contain methods which copy methods from more
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
    polymorphic constraints to the smaller and more general constraints.

    For example, consider the following almost-correct definition for
    @mapMD@ (from "Control.Proxy.Prelude.Base"):

> import Control.Monad.Trans.Class
> import Control.Proxy
>
> mapMD f = foreverK $ \a' -> do
>     a <- request a'
>     b <- lift (f a)
>     respond b

    The compiler infers the following messy signature:

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

    ... and now the compiler infers the following cleaner type:

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

{-| The @(MFunctorP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b . MFunctor (p a' a b' b)) => ...
-}
class MFunctorP p where
    hoist_P
     :: (Monad m)
     => (forall r . m r  -> n r) -> (p a' a b' b m r' -> p a' a b' b n r')
