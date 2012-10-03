{-| This module provides an abstract interface to 'Proxy'-like behavior, so that
    multiple proxy implementations can share the same library of utility
    proxies. -}

module Control.Proxy.Class (
    -- * Proxy composition
    Channel(..),
    -- * Proxy request
    Request(..),
    -- * Proxy respond
    Respond(..),
    ) where

{- * I use educated guesses about which associativy is optimal for each operator
   * Keep precedence lower than function composition, which is 9 at the time of
     of this comment -}
infixr 7 <-<
infixl 7 >->
infixr 8 /</
infixl 8 \>\
infixl 8 \<\
infixr 8 />/

{-| The 'Channel' class defines an interface to a bidirectional flow of
    information.

    Laws:

    * ('>->') and 'idT' form a category:

> idT >-> f = f
> f >-> idT = f
> (f >-> g) >-> h = f >-> (g >-> h)

    Minimal complete definition: 'idT' and either ('>->') or ('<-<').
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

{-| The 'Request' class defines the ability to:

    * Request input using the 'request' command

    * Replace existing 'request' commands using ('\>\')
    
    Laws:

    * ('\>\') and 'request' form a category:

> request \>\ f = f
> f \>\ request = f
> (f \>\ g) \>\ h = f \>\ (g \>\ h)

    Minimal complete definition: 'request' and either ('\>\') or ('/</').
-}
class Request p where
    {-| 'request' input from upstream, passing an argument with the request

        @request a'@ passes @a'@ as a parameter to upstream that upstream may
        use to decide what response to return.  'request' binds the upstream's
        response to its own return value. -}
    request :: (Monad m) => a' -> p a' a x' x m a

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

{-| The 'Respond' class defines the ability to:

    * Respond with output using the 'respond' command

    * Replace existing 'respond' commands using ('/>/')
    
    Laws:

    * ('/>/') and 'request' form a category:

> respond />/ f = f
> f />/ respond = f
> (f />/ g) />/ h = f />/ (g />/ h)

    Minimal complete definition: 'respond' and either ('/>/') or ('\<\').
-}
class Respond p where
    {-| 'respond' with an output for downstream and bind downstream's next
        'request'
          
        @respond b@ satisfies a downstream 'request' by supplying the value @b@
        'respond' blocks until downstream 'request's a new value and binds the
        argument from the next 'request' as its return value. -}
    respond :: (Monad m) => a -> p x' x a' a m a'

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
