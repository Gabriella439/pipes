module Control.Proxy.Trans.Tutorial (
    -- * Motivation
    -- $motivation

    -- * Proxy Transformers
    -- $proxytrans
    ) where

import Control.Monad.Trans.Class
import Control.Proxy
import Control.Proxy.Trans.Either
import Control.Proxy.Trans.State

{- $motivation
    In a 'Session', all composed proxies share the same base monad, meaning that
    you cannot use the base monad to sand-box effects to individual proxies.

    To see this, consider the following simple 'Session':

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

    The client and server share state within the base monad, which is sometimes
    not what we want.  We can keep track of state locally within a 'Proxy' by
    reversing the order of the monad transformers:

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

    Similarly, we might want to implement error handling within proxies.  We
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

    Unfortunately, we can't modify @server2@ to 'catchT' that error because it
    unrecoverably brings down the entire session.  We'd really prefer that
    'EitherT' were the outermost monad transformer so that we can catch errors
    locally:

> client4 :: () -> EitherT String (Client () () IO) ()
> client4 () = forM_ [1..] $ \i -> do
>     lift $ lift $ print i
>     lift $ request ()
>
> server4 :: () -> EitherT String (Server () () IO) ()
> server4 () = (forever $ do
>     lift $ respond ()
>     left "Error" )
>   `catchT` (\str -> do
>         lift $ lift $ putStrLn $ "Caught: " ++ str
>         server4 () )

    However, this again requires unwrapping them using 'runEitherT' before
    composing them:

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
    We encounter a common pattern: Any monad transformer nested within the base
    monad of a 'Proxy' is shared across the entire 'Session' and doesn't
    interfere with composition.  Any monad transformer nested outside the
    'Proxy' type is local to that specific 'Proxy', but interferes with
    composition.  Wouldn't it be nice if we could get 'Proxy'-local transformers
    that didn't interfere with composition?

    Well, we can!  The "Control.Proxy.Trans" hierarchy provides a series of
    common monad transformers that correctly lift the ability to be composed.

    For example, if we want to add 'Proxy'-local state, we just import
    "Control.Proxy.Trans.State":

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
    'StateT'.  This is because 'StateP' is a proxy transformer and not a monad
    transformer.  If I were to define the following kind synonym:

> kind ProxyKind = * -> * -> * -> (* -> *) -> * -> *

    .. then the kind of 'StateP' would be:

> StateP :: * -> ProxyKind -> ProxyKind

    In other words, it transforms a 'Proxy'-like type constructor into a new
    'Proxy'-like type constructor, thus the name: proxy transformer.

    Unfortunately, in order to use proxy transformers, you must expand out the
    'Client' and 'Server' type synonyms, which are not compatible with proxy
    transformers.  Sorry!

    Notice how the outermost 'lift' statements have changed to 'liftP'.  'liftP'
    belongs to the 'ProxyTrans' class, which all proxy transformers implement.
    It behaves just like 'lift' and obeys the monad transformer laws:

> (liftP .) return = return
>
> (liftP .) (f >=> g) = (liftP .) f >=> (liftP .) g

    But, unlike 'lift', 'liftP' also obeys one extra set of laws that guarantee
    it lifts composition sensibly:

> (liftP .) idT = idT
>
> (liftP .) (f >-> g) = (liftP .) f >-> (liftP .) g

    In fact, this @(liftP .)@ pattern is so ubiquitous, that the 'ProxyTrans'
    class provides the additional 'mapP' method for convenience:

> mapP = (liftP .)

    The 'Channel' type class makes this lifting possible.  This type class
    defines the ('>->') and 'idT' methods that all 'Proxy'-like types must
    implement:

> class Channel p where
>     idT :: (Monad) m => a' -> p a' a a' a m r
>     (>->)
>      :: (Monad m)
>      => (b' -> p a' a b' b m r)
>      -> (c' -> p b' b c' c m r)
>      -> c' -> p a' a c' c m r

    All proxy transformers guarantee that if the base proxy implements the
    'Channel' type class, then the transformed proxy also implements the
    'Channel' type class.  Proxy transformers automatically derive how to lift
    composition correctly.

    This means we can compose our 'StateP'-extended proxies directly without
    unwrapping them:

> :t client5 <-< server5 :: () -> StateP Int Proxy C () () C IO r

    However, we still have to unwrap the resulting 'StateP' proxy transformer
    before we can pass it to 'runProxy'.  We use 'runStateK' for this purpose:

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

    Keep in mind that 'runStateK' takes the state as the first argument, unlike
    'runStateT'.  I break from the @transformers@ convention in this regard, for
    syntactic convenience.
-}
