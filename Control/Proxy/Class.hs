{-# LANGUAGE Rank2Types #-}

{-| This module provides an abstract interface to 'Proxy'-like behavior, so that
    multiple proxy implementations and proxy transformers can share the same
    library of utility proxies.

    Several of these type classes, including 'ProxyP', duplicate methods from
    other type-classes (such as ('?>=') duplicating ('>>=')) in order to work
    around Haskell's lack of polymorphic constraints.  You do NOT need
    to use these duplicate methods, which exist solely to plumb internal
    type class machinery and clean up type signatures.  Instead, read the
    \"Polymorphic proxies\" section below to learn how to write clean proxy
    code. -}

module Control.Proxy.Class (
    -- * Core proxy class
    ProxyP(..),
    -- * request/respond substitution
    InteractP(..),
    -- * Proxy-specialized classes
    MonadPlusP(..),
    MonadIOP(..),
    MFunctorP(..),
    -- * Polymorphic proxies
    -- $poly
    ) where

import Control.Monad.IO.Class (MonadIO)

{- * I use educated guesses about which associativy is most efficient for each
     operator.
   * Keep proxy composition lower in precedence than function composition, which
     is 9 at the time of of this comment, so that users can write things like:

> lift . k >-> p
>
> mapT . k >-> p
-}
infixr 7 <-<
infixl 7 >->
infixr 8 /</
infixl 8 \>\
infixl 8 \<\
infixr 8 />/
infixl 1 ?>= -- This should match the fixity of >>=

{-| The 'ProxyP' class defines an interface to all core proxy capabilities that
    all 'Proxy'-like types must implement.

    First, all proxies are monads.  Minimal definition:

    * 'return_P'

    * ('?>=')

    These must satify the monad laws using @(>>=) = (?>=)@ and
    @return_P = return@.

    Second, all proxies must support a bidirectional flow of information.
    Minimal definition:

    * 'idT'

    * ('>->') or ('<-<').

    * 'request'

    * 'respond'

    Intuitively, @p1 <-< p2@ satisfies all 'request's in @p1@ with 'respond's in
    @p2@.

    These must satisfy the following laws:

    * ('>->') and 'idT' form a category:

> idT >-> f = f
>
> f >-> idT = f
>
> (f >-> g) >-> h = f >-> (g >-> h)

    Additionally:

> idT = request >=> respond >=> idT
>
> f >-> (respond >=> g) = respond >=> (f >-> g)
>
> (respond >=> f) >-> (request >=> g) = f >-> g
>
> (request >=> f) >-> (request >=> g) = request >=> (f >-> (request >=> g))
>
> f >-> return = return
>
> return >-> (request >=> f) = return

    Third, all proxies are monad transformers.  Minimal definition:

    * 'lift_P'

    This must satisfy the monad transformer laws, using @lift = lift_P@.

    Additionally:

> f >=> (lift . k >=> g) = lift . k >=> (f >-> g)
>
> (lift . k >=> f) >-> (request >=> g) = lift . k >=> (f >-> (request >=> g))
-}
class ProxyP p where
    {-| 'idT' acts like a \'T\'ransparent proxy, passing all requests further
        upstream, and passing all responses further downstream. -}
    idT :: (Monad m) => a' -> p a' a a' a m r

    {-| Compose two proxies, satisfying all requests from downstream with
        responses from upstream. -}
    (>->) :: (Monad m)
          => (b' -> p a' a b' b m r)
          -> (c' -> p b' b c' c m r)
          -> (c' -> p a' a c' c m r)
    p1 >-> p2 = p2 <-< p1

    {-| Compose two proxies, satisfying all requests from downstream with
        responses from upstream. -}
    (<-<) :: (Monad m)
          => (c' -> p b' b c' c m r)
          -> (b' -> p a' a b' b m r)
          -> (c' -> p a' a c' c m r)
    p1 <-< p2 = p2 >-> p1

    {-| 'request' input from upstream, passing an argument with the request

        @request a'@ passes @a'@ as a parameter to upstream that upstream may
        use to decide what response to return.  'request' binds the upstream's
        response of type @a@ to its own return value. -}
    request :: (Monad m) => a' -> p a' a x' x m a

    {-| 'respond' with an output for downstream and bind downstream's next
        'request'
          
        @respond b@ satisfies a downstream 'request' by supplying the value @b@
        'respond' blocks until downstream 'request's a new value and binds the
        argument of type @b'@ from the next 'request' as its return value. -}
    respond :: (Monad m) => b -> p x' x b' b m b'

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

{-| The 'InteractP' class defines the ability to:

    * Replace existing 'request' commands using ('\>\')

    * Replace existing 'respond' commands using ('/>/')
    
    Minimal complete definition:

    * ('\>\') or ('/</')

    * ('/>/') or ('\<\')

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
class InteractP p where
    -- | @f \\>\\ g@ replaces all 'request's in 'g' with 'f'.
    (\>\) :: (Monad m)
          => (b' -> p a' a x' x m b)
          -> (c' -> p b' b x' x m c)
          -> (c' -> p a' a x' x m c)
    p1 \>\ p2 = p2 /</ p1

    -- | @f \/<\/ g@ replaces all 'request's in 'f' with 'g'.
    (/</) :: (Monad m)
          => (c' -> p b' b x' x m c)
          -> (b' -> p a' a x' x m b)
          -> (c' -> p a' a x' x m c)
    p1 /</ p2 = p2 \>\ p1

    -- | @f \/>\/ g@ replaces all 'respond's in 'f' with 'g'.
    (/>/) :: (Monad m)
          => (a -> p x' x b' b m a')
          -> (b -> p x' x c' c m b')
          -> (a -> p x' x c' c m a')
    p1 />/ p2 = p2 \<\ p1

    -- | @f \\<\\ g@ replaces all 'respond's in 'g' with 'f'.
    (\<\) :: (Monad m)
          => (b -> p x' x c' c m b')
          -> (a -> p x' x b' b m a')
          -> (a -> p x' x c' c m a')
    p1 \<\ p2 = p2 />/ p1
{-| The @(MonadPlusP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . MonadPlus (p a' a b' b m) => ...
-}
class (ProxyP p) => MonadPlusP p where
    mzero_P :: (Monad m) => p a' a b' b m r
    mplus_P
     :: (Monad m) => p a' a b' b m r -> p a' a b' b m r -> p a' a b' b m r

{-| The @(MonadIOP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . MonadIO (p a' a b' b m) => ...
-}
class (ProxyP p) => MonadIOP p where
    liftIO_P :: (MonadIO m) => IO r -> p a' a b' b m r

{-| The @(MFunctorP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b . MFunctor (p a' a b' b)) => ...
-}
class MFunctorP p where
    mapT_P
     :: (Monad m, Monad n)
     => (forall r . m r  -> n r)
     -> (p a' a b' b m r' -> p a' a b' b n r')

{- $poly
    All of these type classes contain methods which copy methods from more
    familiar type classes.  These duplicate methods serve two purposes.

    First, this library requires type class instances that would otherwise be
    impossible to define without providing higher-kinded constraints.  Rather
    than use the following illegal polymorphic constraint:

> instance (forall a' a b' b . MonadTrans (p a' a b' b)) => ...

      ... the instance can instead use the following Haskell98 constraint:

> instance (MonadTransP p) => ...

    Second, these type classes don't require the @FlexibleContexts@ extension
    to use and substantially clean up constraints type signatures.  They convert
    messy constraints like this:

> p :: (MonadP (p a' a b' b m), MonadTrans (p a' a b' b)) => ...

      .. into cleaner and more general constraints like this:

> P :: (ProxyP p) => ...

    These type classes exist solely for internal plumbing and you should never
    directly use the duplicate methods from them.  Instead, you can use all the
    original type classes as long as you embed your proxy code within at least
    one proxy transformer (or 'IdentityP' if don't use any extensions).  The
    type-class machinery will then automatically convert the messier and less
    polymorphic constraints to the smaller and more general constraints.

    For example, consider the following definition for @mapMD@:

> import Control.Monad.Trans.Class
> import Control.Proxy
>
> mapMD f = foreverK $ \a' -> do
>     a <- request a'
>     b <- lift (f a)
>     respond b

    The compiler infers the following messy signature:

> mapMD
>  :: (Monad m, Monad (p x a x b m), MonadTrans (p x a x b), ProxyP p)
>  => (a -> m b) -> x -> p x a x b m r

    Instead, you can cast the code to 'IdentityP p' by wrapping it in
    'runIdentityK':

> --        |difference|  
> mapMD f = runIdentityK $ foreverK $ \a' -> do
>     a <- request a'
>     b <- lift (f a)
>     respond b

    ... and now the compiler collapses all the constraints into 'ProxyP':

> mapMD :: (Monad m, ProxyP p) => (a -> m b) -> x -> p x a x b m r

    The compiler also automatically infers the correct 'ProxyP' constraint for
    any code that uses extensions:

> import Control.Monad
> import Control.Proxy
> import qualified Control.Proxy.Trans.Either as E
>
> example :: (Monad m, ProxyP p) => () -> Producer (EitherP p) String m ()
> example () = do
>     c <- request ()
>     when (c == ' ') $ E.throw "Error: received space"
>     respond c
-}
