{-| This module implements \"ListT done right\" in terms of proxies.

    The 'RespondT' monad transformer is the 'ListT' monad transformer over the
    downstream output type.  Each 'respond' corresponds to an element of the
    list.  The monad bind operation non-deterministically selects one of the
    previous 'respond's as input.  The 'RespondT' Kleisli category corresponds
    to the \"respond\" category of the 'ListT' class.

    Symmetrically, the 'RequestT' monad transformer is the 'ListT' monad
    transformer over the upstream output type.  Each 'request' corresponds to an
    element of the list.  The monad bind operation non-deterministically selects
    one of the previous 'request's as input.  The 'RequestT' Kleisli category
    corresponds to the \"request\" category of the 'ListT' class.

    Unlike 'ListT' from @transformers@, these monad transformers are correct by
    construction and do not assume the base monad is commutative.
-}

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.ListT (
    -- * Respond Monad Transformer
    RespondT(..),
    ProduceT,
    runProduceT,

    -- * Request Monad Transformer
    RequestT(..),
    CoProduceT,
    runCoProduceT,

    -- * ListT
    ListT(..),
    (\>\),
    (/>/),

    -- ** Flipped operators
    (/</),
    (\<\),
    (//<),
    (<\\)

    -- * Laws
    -- $laws
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.Synonym (C, Producer, CoProducer)

-- | A monad transformer over a proxy's downstream end
newtype RespondT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a b' m b =
    RespondT { runRespondT :: p a' a b' b m b' }

instance (Monad m, ListT p) => Functor (RespondT p a' a b' m) where
    fmap f p = RespondT (runRespondT p //> \a -> respond (f a))

instance (Monad m, ListT p) => Applicative (RespondT p a' a b' m) where
    pure a = RespondT (respond a)

    mf <*> mx = RespondT (
        runRespondT mf //> \f ->
        runRespondT mx //> \x ->
        respond (f x) )

instance (Monad m, ListT p) => Monad (RespondT p a' a b' m) where
    return a = RespondT (respond a)

    m >>= f  = RespondT (runRespondT m //> \a -> runRespondT (f a))

instance (ListT p) => MonadTrans (RespondT p a' a b') where
    lift m = RespondT (lift_P m ?>= \a -> respond a)

instance (MonadIO m, ListT p) => MonadIO (RespondT p a' a b' m) where
    liftIO m = lift (liftIO m)

-- | 'ProduceT' is isomorphic to \"ListT done right\"
type ProduceT p = RespondT p C () ()

-- | Convert a 'ProduceT' into a 'Producer' suitable for composition
runProduceT :: ProduceT p m b -> () -> Producer p b m ()
runProduceT p () = runRespondT p

-- | A monad transformer over a proxy's upstream end
newtype RequestT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b' b m a' =
    RequestT { runRequestT :: p a' a b' b m a }

instance (Monad m, ListT p) => Functor (RequestT p a b' b m) where
    fmap f p = RequestT (runRequestT p //< \a -> request (f a))

instance (Monad m, ListT p) => Applicative (RequestT p a b' b m) where
    pure a = RequestT (request a)

    mf <*> mx = RequestT (
        runRequestT mf //< \f ->
        runRequestT mx //< \x ->
        request (f x) )

instance (Monad m, ListT p) => Monad (RequestT p a b' b m) where
    return a = RequestT (request a)

    m >>= f  = RequestT (runRequestT m //< \a -> runRequestT (f a))

instance (ListT p) => MonadTrans (RequestT p a' a b') where
    lift m = RequestT (lift_P m ?>= \a -> request a)

instance (MonadIO m, ListT p) => MonadIO (RequestT p a b' b m) where
    liftIO m = lift (liftIO m)

-- | 'CoProduceT' is isomorphic to \"ListT done right\"
type CoProduceT p = RequestT p () () C

-- | Convert a 'CoProduceT' into a 'CoProducer' suitable for composition
runCoProduceT :: CoProduceT p m a' -> () -> CoProducer p a' m ()
runCoProduceT p () = runRequestT p

{- * I make educated guesses about which associativy is most efficient for each
     operator.
   * Keep composition lower in precedence than function composition, which
     is 9 at the time of of this comment, so that users can write things like:

> lift . k />/ p
>
> hoist f . k />/ p
-}
infixr 8 /</, >\\
infixl 8 \>\, //<
infixl 8 \<\, //>
infixr 8 />/, <\\

-- | The two generalized \"ListT\" categories
class (Proxy p) => ListT p where
    {-| @f >\\\\ p@ replaces all 'request's in @p@ with @f@.

        Point-ful version of ('\>\')
    -}
    (>\\) :: (Monad m)
          => (b' -> p a' a x' x m b)
          ->        p b' b x' x m c
          ->        p a' a x' x m c

    {-| @p \/\/> f@ replaces all 'respond's in @p@ with @f@.

        Point-ful version of ('/>/')
    -}
    (//>) :: (Monad m)
          =>       p x' x b' b m a'
          -> (b -> p x' x c' c m b')
          ->       p x' x c' c m a'

{-| @f \\>\\ g@ replaces all 'request's in 'g' with 'f'.

    Point-free version of ('>\\')
-}
(\>\) :: (Monad m, ListT p)
      => (b' -> p a' a x' x m b)
      -> (c' -> p b' b x' x m c)
      -> (c' -> p a' a x' x m c)
f \>\ g = \c' -> f >\\ g c'
{-# INLINABLE (\>\) #-}

-- | Equivalent to ('\>\') with the arguments flipped
(/</) :: (Monad m, ListT p)
      => (c' -> p b' b x' x m c)
      -> (b' -> p a' a x' x m b)
      -> (c' -> p a' a x' x m c)
p1 /</ p2 = p2 \>\ p1
{-# INLINABLE (/</) #-}

{-| @f \/>\/ g@ replaces all 'respond's in 'f' with 'g'.

    Point-free version of ('//>')
-}
(/>/) :: (Monad m, ListT p)
      => (a -> p x' x b' b m a')
      -> (b -> p x' x c' c m b')
      -> (a -> p x' x c' c m a')
f />/ g = \a -> f a //> g
{-# INLINABLE (/>/) #-}

-- | Equivalent to ('/>/') with the arguments flipped
(\<\) :: (Monad m, ListT p)
      => (b -> p x' x c' c m b')
      -> (a -> p x' x b' b m a')
      -> (a -> p x' x c' c m a')
p1 \<\ p2 = p2 />/ p1
{-# INLINABLE (\<\) #-}

-- | Equivalent to ('>\\') with the arguments flipped
(//<) :: (Monad m, ListT p)
      =>        p b' b x' x m c
      -> (b' -> p a' a x' x m b)
      ->        p a' a x' x m c
p //< f = f >\\ p
{-# INLINABLE (//<) #-}

-- | Equivalent to ('//>') with the arguments flipped
(<\\) :: (Monad m, ListT p)
      => (b -> p x' x c' c m b')
      ->       p x' x b' b m a'
      ->       p x' x c' c m a'
f <\\ p = p //> f
{-# INLINABLE (<\\) #-}

{- $laws
    The 'ListT' class defines the ability to:
    
    * Replace existing 'request' commands using ('\>\')

    * Replace existing 'respond' commands using ('/>/')
    
    Laws:

    * ('/>/') and 'respond' form a ListT Kleisli category:

> return = respond
>
> (>=>) = (//>)

    * ('\>\') and 'request' also form a ListT Kleisli category:

> return = request
>
> (>>=) = (//<)

    Additionally, ('\>\') and ('/>/') both define functors between Proxy Kleisli
    categories:

> a \>\ (b >=> c) = (a \>\ b) >=> (a \>\ c)
>
> a \>\ return = return

> (b >=> c) />/ a = (b />/ a) >=> (c />/ a)
>
> return />/ a = return
-}
