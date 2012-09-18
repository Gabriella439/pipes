{-| This module provides the 'ProxyC' class which provides the abstract
    interface to types that implement 'Proxy'-like capabilities.

    For an extended tutorial, consult "Control.Proxy.Tutorial".-}

module Control.Proxy.Class (
    -- * Overview
    -- $usage

    -- * The ProxyC class
    -- $proxyc
    ProxyC(..),

    -- * Flipped operators
    (>->),
    (\>\),
    (/>/),
    (>|>),
    ) where

-- All associativies are designed to optimize performance
infixr 8 /</
infixl 8 \>\
infixl 8 \<\
infixr 8 />/
infixr 7 <-<
infixl 7 >->
{- The priorities for <|< and >|> should match <=< and >=> from Control.Monad

   I don't match the associativity for <=<, since:

    * Upstream-associative binds punish the naive free monad instance that I use
    * Do notation is upstream-associative by default
    * It doesn't make sense to associate the operator the same way when it is
      flipped

   I consider the associativity of <=< a minor bug in Control.Monad, which I do
   not intend to reproduce here. -}
infixl 1 <|<
infixr 1 >|>

{- $usage
    The 'ProxyC' class defines an interface to four separate categories that
    encompass what it means to be \"proxy-like\", where each category defines a
    different way to compose values of the following shape:

> -- The fundamental unit of abstraction for proxies
> (Monad m, ProxyC p) => a -> p b c d e m r

    Most users of this library will use the 'respond' and 'request' commands
    to define their own proxies and the ('<-<') composition operator to connect
    predefined proxies.

    Extension writers will also find the ('/</') and ('\<\') operators useful,
    since they let you modify existing proxies that implement the 'ProxyC'
    interface without pattern matching on a specific type.
-}

{- $proxyc
    The 'ProxyC' class uses four categories to structure the exposed API, each
    of which provides a composition operation and its identity.

  * The first category comprises the ('<-<') composition operator and its
    identity, 'idT'.  ('<-<') embodies the most commonly used composition
    operator, which connects 'request' statements in the downstream proxy with
    'respond' statements in the upstream proxy.  'idT' represents the \"empty\"
    proxy that auto-forwards everything passing through it:

> idT <-< f = f
> f <-< idT = f
> (f <-< g) <-< h = f <-< (g <-< h)

  * The next category comprises the ('/</') composition operator and its
    identity, 'request'.  The 'request' command requests parametrized input from
    upstream, which is satisfied by the dual 'respond' command.  (@f \/<\/ g@)
    substitutes all occurences of 'request' in @f@ with @g@, which explains why
    'request' is the identity of ('/</'):

> request /</ f = f  -- Replacing a single 'request' with 'f' yields 'f'
> f /</ request = f  -- Replacing every 'request' with 'request' changes nothing
> (f /</ g) /</ h = f /</ (g /</ h)

  * The dual category comprises the ('\<\') composition operator and its
    identity, 'respond'.  The 'respond' command responds to the previous
    'request' and binds the argument of the next 'request'.  (@f \\<\\ g@)
    substitutes all occurences of 'respond' in @g@ with @f@.  This explains why
    'respond' is the identity of ('\<\'):

> respond \<\ f = f -- Replacing every 'respond' with 'respond' changes nothing
> f \<\ respond = f -- Replacing a single 'respond' with 'f' yields 'f'
> (f \<\ g) \<\ h = f \<\ (g \<\ h)

  * The final category comprises Kleisli composition ('<|<') and its identity,
    'returnP'.  I add these as methods to the type class to work around
    limitations of Haskell's type class system.  You should always instantiate
    these with ('<=<') and 'return'.

> returnP <|< f = f
> f <|< returnP = f
> (f <|< g) <|< h = f <|< (g <|< h)

-}

{-| An interface to interactive types 

    Laws:

    * ('<-<') and 'idT' form a category:

> idT <-< f = f
> f <-< idT = f
> (f <-< g) <-< h = f <-< (g <-< h)

    * ('/</') and 'request' form a category:

> request /</ f = f
> f /</ request = f
> (f /</ g) /</ h = f /</ (g /</ h)

    * ('\<\') and 'respond' form a category:

> respond \<\ f = f
> f \<\ respond = f
> (f \<\ g) \<\ h = f \<\ (g \<\ h)

    * ('<|<') and 'returnP' form a category:

> returnP <|< f = f
> f <|< returnP = f
> (f <|< g) <|< h = f <|< (g <|< h)

-}
class ProxyC p where
    {-| 'idT' acts like a \'T\'ransparent proxy, passing all requests further
        upstream, and passing all responses further downstream. -}
    idT :: (Monad m) => a' -> p a' a a' a m r

    {-| Compose two proxies, satisfying all requests from downstream with
        responses from upstream. -}
    (<-<) :: (Monad m)
          => (c' -> p b' b c' c m r)
          -> (b' -> p a' a b' b m r)
          -> (c' -> p a' a c' c m r)

    {-| 'request' input from upstream, passing an argument with the request

        @request a'@ passes @a'@ as a parameter to upstream that upstream may
        use to decide what response to return.  'request' binds the upstream's
        response to its own return value. -}
    request :: (Monad m) => a' -> p a' a x' x m a

    -- | @f /</ g@ replaces all 'request's in 'f' with 'g'.
    (/</) :: (Monad m)
          => (c' -> p b' b x' x m c)
          -> (b' -> p a' a x' x m b)
          -> (c' -> p a' a x' x m c)

    {-| 'respond' with an output for downstream and bind downstream's next
        'request'
          
        @respond b@ satisfies a downstream 'request' by supplying the value @b@.
        'respond' blocks until downstream 'request's a new value and binds the
        argument from the next 'request' as its return value. -}
    respond :: (Monad m) => a -> p x' x a' a m a'

    -- | @f \<\ g@ replaces all 'respond's in 'g' with 'f'.
    (\<\) :: (Monad m)
          => (b -> p x' x c' c m b')
          -> (a -> p x' x b' b m a')
          -> (a -> p x' x c' c m a')

    -- | Equivalent to 'return', to work around type class limitations
    returnP :: (Monad m) => a -> p x' x y' y m a

    -- | Equivalent to ('<=<'), to work around type class limitations
    (<|<) :: (Monad m)
          => (b -> p x' x y' y m c)
          -> (a -> p x' x y' y m b)
          -> (a -> p x' x y' y m c)

{-| Compose two proxies, satisfying all requests from downstream with responses
    from upstream.

    Corresponds to ('<-<') with the arguments flipped -}
(>->) :: (Monad m, ProxyC p)
      => (b' -> p a' a b' b m r)
      -> (c' -> p b' b c' c m r)
      -> (c' -> p a' a c' c m r)
(>->) = flip (<-<)

{-| @f \>\ g@ replaces all 'request's in 'g' with 'f'.

    Corresponds to ('/</') with the arguments flipped -}
(\>\) :: (Monad m, ProxyC p)
      => (b' -> p a' a x' x m b)
      -> (c' -> p b' b x' x m c)
      -> (c' -> p a' a x' x m c)
(\>\) = flip (/</)

{-| @f />/ g@ replaces all 'respond's in 'f' with 'g'.

    Corresponds to ('\<\') with the arguments flipped -}
(/>/) :: (Monad m, ProxyC p)
      => (a -> p x' x b' b m a')
      -> (b -> p x' x c' c m b')
      -> (a -> p x' x c' c m a')
(/>/) = flip (\<\)

{-| Equivalent to ('>=>'), to work around type class limitations

    Corresponds to ('<|<') with the arguments flipped -}
(>|>) :: (Monad m, ProxyC p)
      => (a -> p x' x y' y m b)
      -> (b -> p x' x y' y m c)
      -> (a -> p x' x y' y m c)
(>|>) = flip (<|<)
