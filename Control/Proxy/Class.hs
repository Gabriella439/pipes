{-# LANGUAGE Rank2Types #-}

{-| This module provides an abstract interface to 'Proxy'-like behavior, so that
    multiple proxy implementations can share the same library of utility
    proxies. -}

module Control.Proxy.Class (
    -- * Proxy composition
    Channel(..),
    -- * Proxy request and respond
    -- $interact
    InteractId(..),
    InteractComp(..),
    -- * Proxy-specialized classes
    -- $hacks
    MonadP(..),
    MonadTransP(..),
    MonadPlusP(..),
    MonadIOP(..),
    MFunctorP(..),
    ) where

import Control.Monad.IO.Class (MonadIO)

{- * I use educated guesses about which associativy is optimal for each operator
   * Keep precedence lower than function composition, which is 9 at the time of
     of this comment -}
infixr 7 <-<
infixl 7 >->
infixr 8 /</
infixl 8 \>\
infixl 8 \<\
infixr 8 />/
infixl 1 ?>= -- This should match the fixity of >>=

{-| The 'Channel' class defines an interface to a bidirectional flow of
    information.

    Laws:

    * ('>->') and 'idT' form a category:

> idT >-> f = f
>
> f >-> idT = f
>
> (f >-> g) >-> h = f >-> (g >-> h)

    Minimal complete definition:

    * 'idT'

    * ('>->') or ('<-<').
-}
class Channel p where
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

{- $interact
    The 'InteractId' and 'InteractComp' classes defines the ability to:

    * Request input using the 'request' command

    * Replace existing 'request' commands using ('\>\')

    * Respond with output using the 'respond' command

    * Replace existing 'respond' commands using ('/>/')
    
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

    I split them into two separate classes because some proxy transformers lift
    'InteractId' but not 'InteractComp'.
-}

-- | Identities of the \"request\" and \"respond\" categories
class InteractId p where
    {-| 'request' input from upstream, passing an argument with the request

        @request a'@ passes @a'@ as a parameter to upstream that upstream may
        use to decide what response to return.  'request' binds the upstream's
        response to its own return value. -}
    request :: (Monad m) => a' -> p a' a x' x m a

    {-| 'respond' with an output for downstream and bind downstream's next
        'request'
          
        @respond b@ satisfies a downstream 'request' by supplying the value @b@
        'respond' blocks until downstream 'request's a new value and binds the
        argument from the next 'request' as its return value. -}
    respond :: (Monad m) => a -> p x' x a' a m a'

{-| Composition operators of the \"request\" and \"respond\" categories

    Minimal complete definition:

    * ('\>\') or ('/</')

    * ('/>/') or ('\<\') -}
class InteractComp p where
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

{- $hacks
    The following type classes serve three purposes:

    * They work around the lack of polymorphic constraints in Haskell

    * They remove the need for the @FlexibleContexts@ extension

    * They substantially clean up inferred type signatures

    You don't need to use the methods from these type-classes.  Every type that
    implements one of these \'@P@\'-suffixed class also implements the
    corresponding non-\'@P@\'-suffixed class.  For example, 'Proxy' implements
    both 'MonadP' and 'Monad':

> instance MonadP Proxy where ...
> instance (Monad m) => Monad (Proxy a' a b' b m) where ...

    You would always use the 'Monad' class and ignore the 'MonadP' class.
-}

{-| A @(MonadProxy p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . Monad (p a' a b' b m)) => ...
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

{-| The @(MonadPlusP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . MonadPlus (p a' a b' b m) => ...
-}
class (MonadP p) => MonadPlusP p where
    mzero_P :: (Monad m) => p a' a b' b m r
    mplus_P
     :: (Monad m) => p a' a b' b m r -> p a' a b' b m r -> p a' a b' b m r

{-| The @(MonadIOP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b m . MonadIO (p a' a b' b m) => ...
-}
class (MonadP p) => MonadIOP p where
    liftIO_P :: (MonadIO m) => IO r -> p a' a b' b m r

{-| The @(MFunctorP p)@ constraint is equivalent to the following constraint:

> (forall a' a b' b . MFunctor (p a' a b' b)) => ...
-}
class MFunctorP p where
    mapT_P
     :: (Monad m, Monad n)
     => (forall r . m r  -> n r)
     -> (p a' a b' b m r' -> p a' a b' b n r')
