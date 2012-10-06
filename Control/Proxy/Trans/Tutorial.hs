-- | This module provides the tutorial for the "Control.Proxy.Trans" hierarchy

module Control.Proxy.Trans.Tutorial (
    -- * Motivation
    -- $motivation

    -- * Proxy Transformers
    -- $proxytrans

    -- * Compatibility
    -- $compatibility

    -- * Proxy Transformer Stacks
    -- $stacks
    ) where

import Control.Monad.Trans.Class
import Control.Proxy
import Control.Proxy.Trans.Either
import Control.Proxy.Trans.State

{- $motivation
    In a 'Session', all composed proxies share effects within the base monad.
    To see how, consider the following simple 'Session':

> client1 :: () -> Client () () (StateT Int IO) r
> client1 () = forever $ do
>     s <- lift get
>     lift $ lift $ putStrLn $ "Client: " ++ show s
>     lift $ put (s + 1)
>     request ()
>
> server1 :: () -> Server () () (StateT Int IO) r
> server1 () = forever $ do
>     s <- lift get
>     lift $ lift $ putStrLn $ "Server: " ++ show s
>     lift $ put (s + 1)
>     respond ()

>>> execWriterT $ runProxy $ client1 <-< server1
Client: 0
Server: 1
Client: 2
Server: 3
Client: 4
Server: 5
...

    The client and server share the same state, which is sometimes not what we
    want.  We can easily solve this by running each 'Proxy' with its own local
    state by changing the order of the 'Proxy' and 'StateT' monad transformers:

> client2 :: () -> StateT Int (Client () () IO) r
> client2 () = forever $ do
>     s <- get
>     lift $ lift $ putStrLn $ "Client: " ++ show s
>     put (s + 1)
>     lift $ request ()
>
> server2 :: () -> StateT Int (Server () () IO) r
> server2 () = forever $ do
>     s <- get
>     lift $ lift $ putStrLn $ "Server: " ++ show s
>     put (s + 1)
>     lift $ respond ()

    ... but then we can no longer compose them directly.  We have to first
    unwrap each one with 'evalStateT' before composing:

>>> runProxy $ (`evalStateT` 0) . client2 <-< (`evalStateT` 0) . server2
Client: 0
Server: 0
Client: 1
Server: 1
Client: 2
Server: 2
...

    Here's another example: suppose we want to handle errors within proxies.  We
    could try adding 'EitherT' to the base monad like so:

> import Control.Error
>
> client3 :: () -> Client () () (EitherT String IO) ()
> client3 () = forM_ [1..] $ \i -> do
>     lift $ lift $ print i
>     request ()
>
> server3 :: (Monad m) => () -> Server () () (EitherT String m) r
> server3 () = lift $ left "ERROR"

>>> runEithert $ runProxy $ client2 <-< server2
1
Left "ERROR"

    Unfortunately, we can't modify @server2@ to 'catchT' that error because we
    cannot access the inner 'EitherT' monad transformer until we run the
    'Session'.  We'd really prefer to place the 'EitherT' monad transformer
    /outside/ the 'Proxy' monad transformer so that we can catch and handle
    errors locally within a 'Proxy' without disturbing other proxies:

> client4 :: () -> EitherT String (Client () () IO) ()
> client4 () = forM_ [1..] $ \i -> do
>     lift $ lift $ print i
>     lift $ request ()
>
> server4 :: () -> EitherT String (Server () () IO) ()
> server4 () = (forever $ do
>     lift $ respond ()
>     throwT "Error" )
>   `catchT` (\str -> do
>         lift $ lift $ putStrLn $ "Caught: " ++ str
>         server4 () )

    However, this solution similarly requires unwrapping the client and server
    using 'runEitherT' before composing them:

>>> runProxy $ runEitherT . client4 <-< runEitherT . server4
1
Caught: Error
2
Caught: Error
3
Caught: Error
...

-}

{- $proxytrans
    We need some way to layer monad transformers /outside/ the proxy type
    without interfering with 'Proxy' composition.  To do this, we overload
    'Proxy' composition using the 'Channel' type class from
    "Control.Proxy.Class":

> class Channel p where
>     idT :: (Monad) m => a' -> p a' a a' a m r
>     (>->)
>      :: (Monad m)
>      => (b' -> p a' a b' b m r)
>      -> (c' -> p b' b c' c m r)
>      -> c' -> p a' a c' c m r

    Obviously, 'Proxy' implements this class:

> instance Channel Proxy where ...

    ... but we would also like our monad transformers layered outside the
    'Proxy' type to also implement the 'Channel' class so that we could compose
    them directly without unwrapping.  Unfortunately, these monad transformers
    do not fit the signature of the 'Channel' class.

    Fortunately, the "Control.Proxy.Trans" hierarchy provides several common
    monad transformers which have been upgraded to fit the 'Channel' type class.
    I call these \"proxy transformers\".

    For example, "Control.Proxy.Trans.State" provides a proxy transformer
    equivalent to @Control.Monad.Trans.State@.  Similarly,
    "Control.Proxy.Trans.Either" provides a proxy transformer equivalent to
    @Control.Monad.Trans.Either@.

    Let's use a working code example to demonstrate how to use them:

> import Control.Proxy.Trans.State
> 
> client5 :: () -> StateP Int Proxy () () () C IO r
> client5 () = forever $ do
>     s <- get
>     liftP $ lift $ putStrLn $ "Client: " ++ show s
>     put (s + 1)
>     liftP $ request ()
>
> server5 :: () -> StateP Int Proxy C () () () IO r
> server5 () = forever $ do
>     s <- get
>     liftP $ lift $ putStrLn $ "Server: " ++ show s
>     put (s + 1)
>     liftP $ respond ()

    You'll see that our type signatures changed.  Now we use 'StateP' instead of
    'StateT'.  However, 'StateP' does not transform monads, but instead
    transforms proxies.

    To see this, let's first study the kind of 'StateT'.  If we first define:

> kind MonadKind = * -> *

    Then @StateT s@ takes a monad, and returns a new monad:

> StateT s :: MonadKind -> MonadKind

    Now consider the kind of a 'Proxy'-like type constructor suitable for the
    'Channel' type class:

> kind ProxyKind = * -> * -> * -> * -> (* -> *) -> * -> *

    Then @StateP s@  takes a 'Proxy'-like and returns a new 'Proxy'-like type:

> StateP s :: ProxyKind -> ProxyKind

    This is why I call these \"proxy transformers\" and not monad transformers.
    They all take some 'Proxy'-like type that implements 'Channel' and transform
    it into a new 'Proxy'-like type that also implements 'Channel'.  For
    example, 'StateP' implement the following instance:

> instance (Channel p) => Channel (StateP s p) where ...

    All proxy transformers guarantee that if the base proxy implements the
    'Channel' type class, then the transformed proxy also implements the
    'Channel' type class.  This means that you can build a proxy transformer
    stack, just like you might build a monad transformer stack.

    Unfortunately, in order to use proxy transformers, you must expand out the
    'Client' and 'Server' type synonyms, which are not compatible with proxy
    transformers.  Sorry!  This is why there are no 'Server' or 'Client' type
    synonyms in the types of our new client and server and I had to write out
    all the inputs and outputs.

    Notice how the outermost 'lift' statements in our client and server have
    changed to 'liftP'.  'liftP' replaces 'lift' for proxy transformers, and it
    lifts any action in the base proxy to an action in the transformed proxy.
    In the previous example, the base proxy was 'Proxy' and the transformed
    proxy was @StateP s Proxy@, so 'liftP's type got specialized to:

> liftP :: Proxy a' a b' b m r -> StateP s Proxy a' a b' b m r

    The 'ProxyTrans' class defines 'liftP', and all proxy transformers implement
    the 'ProxyTrans' class.  Since proxies are still monads, 'liftP' must
    behave just like 'lift' and obey the monad transformer laws:

> (liftP .) return = return
>
> (liftP .) (f >=> g) = (liftP .) f >=> (liftP .) g

    But, unlike 'lift', 'liftP' obeys one extra set of laws that guarantee it 
    also lifts composition sensibly:

> (liftP .) idT = idT
>
> (liftP .) (f >-> g) = (liftP .) f >-> (liftP .) g

    In fact, this @(liftP .)@ pattern is so ubiquitous, that the 'ProxyTrans'
    class provides the additional 'mapP' method for convenience:

> mapP = (liftP .)

    Proxy transformers automatically derive how to lift composition correctly
    and also guarantee that the derived composition obeys the category laws if
    the base composition obeyed the category laws.  Since 'Proxy' composition
    obeys the category laws, any proxy transformer stack built on top of it
    automatically derives a composition operation that is correct by
    construction.

    Let's prove this by directly composing our 'StateP'-extended proxies without
    unwrapping them:

> :t client5 <-< server5 :: () -> StateP Int Proxy C () () C IO r

    However, we still have to unwrap the final 'StateP' 'Session' before we can
    pass it to 'runProxy'.  We use 'runStateK' for this purpose:

>>> runProxy $ runStateK 0 $ client5 <-< server5
Client: 0
Server: 0
Client: 1
Server: 1
Client: 2
Server: 2
Client: 3
Server: 3
...

    Keep in mind that 'runStateK' takes the initial state as its first argument,
    unlike 'runStateT'.  I break from the @transformers@ convention for
    syntactic convenience.

    We can similarly fix our 'EitherT' example, using 'EitherP' from
    "Control.Proxy.Trans.Either":

> import Control.Proxy.Trans.Either as E
>
> client6 :: () -> EitherP String Proxy () () () C IO ()
> client6 () = forM_ [1..] $ \i -> do
>     liftP $ lift $ print i
>     liftP $ request ()
>
> server6 :: () -> EitherP String Proxy C () () () IO ()
> server6 () = (forever $ do
>     liftP $ respond ()
>     E.throw "Error" )
>   `E.catch` (\str -> do
>         liftP $ lift $ putStrLn $ "Caught: " ++ str
>         server6 () )

>>> runProxy $ runEitherK $ client6 <-< server6
1
Caught: Error
2
Caught: Error
3
Caught: Error
...

-}

{- $compatibility
    Proxy transformers do more than just lift composition.  They automatically
    provide proxies written in the base monad.  For example, what if I wanted to
    use the 'takeB_' proxy from "Control.Proxy.Prelude.Base" to cap the number
    of results?  I can't compose it directly because it uses the 'Proxy' type:

> takeB_ :: (Monad m) => Int -> a' -> Proxy a' a a' a m ()

    ... whereas @client6@ and @server6@ use @EitherP String Proxy@.  However,
    this doesn't matter because we can automatically lift 'takeB_' to be
    compatible with them using 'mapP':

>>> runProxy $ runEitherK $ client6 <-< mapP (takeB_ 2) <-< server6
1
Caught: Error
2
Caught:Error

    'mapP' promotes any proxy written using the base proxy type to automatically
    be compatible with proxies written using the extended proxy type.  This
    means you can safely write utility proxies using the smallest feature set
    they require and promote them as necessary to work with more extended
    feature sets.  This ensures that any proxies you write always remain
    forwards-compatible as people write new extensions.
-}

{- $stacks
    You can stack proxy transformers to combine their effects, such as in the
    following example, which combines everything we've used so far:

> client7 :: () -> EitherP String (StateP Int Proxy) () Int () C IO r
> client7 () = do
>     n <- liftP get
>     liftP $ liftP $ lift $ print n
>     n' <- liftP $ liftP $ request ()
>     liftP $ put n'
>     E.throw "ERROR"

>>> runProxy $ runStateK 0 $ runEitherK $ client7 <-< mapP (mapP (enumFromS 1))
0
(Left "Error", 1)

    But that's still not the full story!  For calls to the base monad (i.e. 'IO'
    in this case), you don't need to precede them with all those 'liftP's.
    Every proxy transformer also correctly derives 'MonadTrans', so you can dig
    straight to the base monad by just calling 'lift' at the outer-most level:

> client7 :: () -> EitherP String (StateP Int Proxy) () Int () C IO r
> client7 () = do
>     n <- liftP get
>     lift $ print n  -- Much better!
>     n' <- liftP $ liftP $ request ()
>     liftP $ put n'
>     E.throw "ERROR"

    Also, you can combine multiple proxy transformers into a single proxy
    transformer, just like you would with monad transformers:

> newtype BothP e s p a' a b' b m r =
>     BothP { unBothP :: EitherP e (StateP s p) a' a b' b m r }
>     deriving (Functor, Applicative, Monad, MonadTrans, Channel)
> 
> instance ProxyTrans (BothP e s) where
>     liftP = BothP . liftP . liftP
> 
> runBoth
>  :: (Monad m)
>  => s
>  -> (b' -> BothP e s p a' a b' b m r)
>  -> (b' -> p a' a b' b m (Either e r, s))
> runBoth s = runStateK s . runEitherK . fmap unBothP
> 
> get' :: (Monad (p a' a b' b m), Channel p)
>      => BothP e s p a' a b' b m s
> get' = BothP $ liftP get
> 
> put' :: (Monad (p a' a b' b m), Channel p)
>      => s -> BothP e s p a' a b' b m ()
> put' x = BothP $ liftP $ put x
> 
> throw' :: (Monad (p a' a b' b m), Channel p)
>        => e -> BothP e s p a' a b' b m r
> throw' e = BothP $ E.throw e

    Then we can write proxies using this new proxy transformer of ours:

> client8 :: () -> BothP String Int Proxy () Int () C IO r
> client8 () = do
>     n <- get'
>     lift $ print n
>     n' <- liftP $ request ()
>     put' n'
>     throw' "ERROR"

>>> runProxy $ runBoth 0 $ client8 <-< mapP (enumFromS 1)
0
(Left "ERROR",1)

    Note that 'request' and 'respond' are not automatically liftable, because of
    technical limitations with Haskell type classes.  When I resolve these
    issues they will also be automatically promoted by proxy transformers.  For
    now, you must lift them manually using 'liftP':

> request = (liftP .) request
> respond = (liftP .) respond

    The left 'request' and 'respond' in the above equations are what the lifted
    definitions would be for each proxy transformer if Haskell's type class
    system didn't get in my way.
-}
