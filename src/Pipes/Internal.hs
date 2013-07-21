{-| This is an internal module, meaning that it is unsafe to import unless you
    understand the risks.

    This module provides a fast implementation by weakening the monad
    transformer laws.  These laws do not hold if you can pattern match on the
    constructors, as the following counter-example illustrates:

> lift . return = M . return . Pure
>
> return = Pure
>
> lift . return /= return

    You do not need to worry about this if you do not import this module, since
    the other modules in this library do not export the constructors or export
    any functions which can violate the monad transformer laws.
-}

module Pipes.Internal (
    Proxy(..),
    observe
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))

{-| A 'Proxy' is a monad transformer that receives and sends information on both
    an upstream and downstream interface.  The 'Pipes.await' command
    communicates with the upstream interface and the 'Pipes.yield' command
    communicates with the downstream interface.

    You can connect proxies together in five different ways:

    * ('Pipes.>->'): connect pull-based streams

    * ('Pipes.>~>'): connect push-based streams

    * ('Pipes.\>\'): chain folds

    * ('Pipes./>/'): chain unfolds

    * ('Control.Monad.>=>'): sequence proxies

    The type variables signify:

    * @a'@ and @a@ - The upstream interface, where @(a')@s go out and @(a)@s
      come in

    * @b'@ and @b@ - The downstream interface, where @(b)@s go out and @(b')@s
      come in

    * @m @ - The base monad

    * @r @ - The return value

    Diagrammatically:

> Upstream | Downstream
>     +---------+
>     |         |
> a' <==       <== b'
>     |         |
> a  ==>       ==> b
>     |    |    |
>     +----|----+
>          v
>          r

-}
data Proxy a' a b' b m r
    = Await a' (a  -> Proxy a' a b' b m r )
    | Yield b  (b' -> Proxy a' a b' b m r )
    | M        (m    (Proxy a' a b' b m r))
    | Pure  r

instance (Monad m) => Functor (Proxy a' a b' b m) where
    fmap f p0 = go p0 where
        go p = case p of
            Await a' fa  -> Await a' (\a  -> go (fa  a ))
            Yield b  fb' -> Yield b  (\b' -> go (fb' b'))
            M        m   -> M (m >>= \p' -> return (go p'))
            Pure  r      -> Pure (f r)

instance (Monad m) => Applicative (Proxy a' a b' b m) where
    pure      = Pure
    pf <*> px = go pf where
        go p = case p of
            Await a' fa  -> Await a' (\a  -> go (fa  a ))
            Yield b  fb' -> Yield b  (\b' -> go (fb' b'))
            M        m   -> M (m >>= \p' -> return (go p'))
            Pure   f     -> fmap f px

instance (Monad m) => Monad (Proxy a' a b' b m) where
    return = Pure
    (>>=)  = _bind

_bind
    :: (Monad m)
    => Proxy a' a b' b m r
    -> (r -> Proxy a' a b' b m r')
    -> Proxy a' a b' b m r'
p0 `_bind` f = go p0 where
    go p = case p of
        Await a' fa  -> Await a' (\a  -> go (fa  a ))
        Yield b  fb' -> Yield b  (\b' -> go (fb' b'))
        M        m   -> M (m >>= \p' -> return (go p'))
        Pure   r     -> f r

{-# RULES
    "_bind (Await a' k) f" forall a' k f .
        _bind (Await a' k) f = Await a' (\a  -> _bind (k a)  f);
    "_bind (Yield b  k) f" forall b  k f .
        _bind (Yield b  k) f = Yield b  (\b' -> _bind (k b') f);
    "_bind (M        m) f" forall m    f .
        _bind (M        m) f = M (m >>= \p -> return (_bind p f));
    "_bind (Pure  r   ) f" forall r    f .
        _bind (Pure  r   ) f = f r;
  #-}

instance MonadTrans (Proxy a' a b' b) where
    lift m = M (m >>= \r -> return (Pure r))

instance MFunctor (Proxy a' a b' b) where
    hoist nat p0 = go (observe p0) where
        go p = case p of
            Await a' fa  -> Await a' (\a  -> go (fa  a ))
            Yield b  fb' -> Yield b  (\b' -> go (fb' b'))
            M        m   -> M (nat (m >>= \p' -> return (go p')))
            Pure     r   -> Pure r

instance (MonadIO m) => MonadIO (Proxy a' a b' b m) where
    liftIO m = M (liftIO (m >>= \r -> return (Pure r)))

{-| The monad transformer laws are correct when viewed through the 'observe'
    function:

> observe (lift (return r)) = observe (return r)
>
> observe (lift (m >>= f)) = observe (lift m >>= lift . f)

    This correctness comes at a small cost to performance, so use this function
    sparingly.

    This function is a convenience for low-level @pipes@ implementers.  You do
    not need to use 'observe' if you stick to the safe API.
-}
observe :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m r
observe p0 = M (go p0) where
    go p = case p of
        Await a' fa  -> return (Await a' (\a  -> observe (fa  a )))
        Yield b  fb' -> return (Yield b  (\b' -> observe (fb' b')))
        M        m'  -> m' >>= go
        Pure  r      -> return (Pure r)
{-# INLINABLE observe #-}
