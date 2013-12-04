{-| This is an internal module, meaning that it is unsafe to import unless you
    understand the risks.

    This module provides a fast implementation by weakening the monad
    transformer laws.  These laws do not hold if you can pattern match on the
    constructors, as the following counter-example illustrates:

@
'lift' '.' 'return' = 'M' '.' 'return' '.' 'Pure'

'return' = 'Pure'

'lift' '.' 'return' /= 'return'
@

    You do not need to worry about this if you do not import this module, since
    the other modules in this library do not export the constructors or export
    any functions which can violate the monad transformer laws.
-}

{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , UndecidableInstances
  , CPP
  #-}

-- The rewrite RULES require the 'TrustWorthy' annotation
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

module Pipes.Internal (
    -- * Internal
      Proxy(..)
    , unsafeHoist
    , observe,
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
#ifndef haskell98
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Error (MonadError(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter(..))
import Data.Monoid (mempty,mappend)
#endif

{-| A 'Proxy' is a monad transformer that receives and sends information on both
    an upstream and downstream interface.

    The type variables signify:

    * @a'@ and @a@ - The upstream interface, where @(a')@s go out and @(a)@s
      come in

    * @b'@ and @b@ - The downstream interface, where @(b)@s go out and @(b')@s
      come in

    * @m @ - The base monad

    * @r @ - The return value
-}
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r

instance (Monad m) => Functor (Proxy a' a b' b m) where
    fmap f p0 = go p0 where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure    r      -> Pure (f r)

instance (Monad m) => Applicative (Proxy a' a b' b m) where
    pure      = Pure
    pf <*> px = go pf where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure     f     -> fmap f px

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
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure     r     -> f r

{-# RULES
    "_bind (Request a' k) f" forall a' k f .
        _bind (Request a' k) f = Request a' (\a  -> _bind (k a)  f);
    "_bind (Respond b  k) f" forall b  k f .
        _bind (Respond b  k) f = Respond b  (\b' -> _bind (k b') f);
    "_bind (M          m) f" forall m    f .
        _bind (M          m) f = M (m >>= \p -> return (_bind p f));
    "_bind (Pure    r   ) f" forall r    f .
        _bind (Pure    r   ) f = f r;
  #-}

instance MonadTrans (Proxy a' a b' b) where
    lift m = M (m >>= \r -> return (Pure r))

{-| 'unsafeHoist' is like 'hoist', but faster.

    This is labeled as unsafe because you will break the monad transformer laws
    if you do not pass a monad morphism as the first argument.  This function is
    safe if you pass a monad morphism as the first argument.
-}
unsafeHoist
    :: (Monad m)
    => (forall x . m x -> n x) -> Proxy a' a b' b m r -> Proxy a' a b' b n r
unsafeHoist nat = go
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (nat (m >>= \p' -> return (go p')))
        Pure       r   -> Pure r
{-# INLINABLE unsafeHoist #-}

#ifndef haskell98
instance MFunctor (Proxy a' a b' b) where
    hoist nat p0 = go (observe p0) where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (nat (m >>= \p' -> return (go p')))
            Pure       r   -> Pure r
#endif

instance (MonadIO m) => MonadIO (Proxy a' a b' b m) where
    liftIO m = M (liftIO (m >>= \r -> return (Pure r)))

#ifndef haskell98
instance (MonadReader r m) => MonadReader r (Proxy a' a b' b m) where
    ask = lift ask
    local f = go
        where
          go p = case p of
              Request a' fa  -> Request a' (\a  -> go (fa  a ))
              Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
              Pure    r      -> Pure r
              M       m      -> M (local f m >>= \r -> return (go r))
#if MIN_VERSION_mtl(2,1,0)
    reader = lift . reader
#endif

instance (MonadState s m) => MonadState s (Proxy a' a b' b m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2,1,0)
    state = lift . state
#endif

instance (MonadWriter w m) => MonadWriter w (Proxy a' a b' b m) where
#if MIN_VERSION_mtl(2,1,0)
    writer = lift . writer
#endif
    tell = lift . tell
    listen p0 = go p0 mempty
      where
        go p w = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ) w)
            Respond b  fb' -> Respond b  (\b' -> go (fb' b') w)
            M       m      -> M (do
                (p', w') <- listen m
                return (go p' $! mappend w w') )
            Pure    r      -> Pure (r, w)

    pass p0 = go p0 mempty
      where
        go p w = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ) w)
            Respond b  fb' -> Respond b  (\b' -> go (fb' b') w)
            M       m      -> M (do
                (p', w') <- listen m
                return (go p' $! mappend w w') )
            Pure    (r, f) -> M (pass (return (Pure r, \_ -> f w)))

instance (MonadError e m) => MonadError e (Proxy a' a b' b m) where
    throwError = lift . throwError
    catchError p0 f = go p0
      where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            Pure    r      -> Pure r
            M          m   -> M ((do
                p' <- m
                return (go p') ) `catchError` (\e -> return (f e)) )
#endif

instance (MonadPlus m) => Alternative (Proxy a' a b' b m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus m) => MonadPlus (Proxy a' a b' b m) where
    mzero = lift mzero
    mplus p0 p1 = go p0
      where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            Pure    r      -> Pure r
            M          m   -> M ((do
                p' <- m
                return (go p') ) `mplus` return p1 )

{-| The monad transformer laws are correct when viewed through the 'observe'
    function:

@
'observe' ('lift' ('return' r)) = 'observe' ('return' r)

'observe' ('lift' (m '>>=' f)) = 'observe' ('lift' m '>>=' 'lift' '.' f)
@

    This correctness comes at a small cost to performance, so use this function
    sparingly.

    This function is a convenience for low-level @pipes@ implementers.  You do
    not need to use 'observe' if you stick to the safe API.
-}
observe :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m r
observe p0 = M (go p0) where
    go p = case p of
        Request a' fa  -> return (Request a' (\a  -> observe (fa  a )))
        Respond b  fb' -> return (Respond b  (\b' -> observe (fb' b')))
        M          m'  -> m' >>= go
        Pure    r      -> return (Pure r)
{-# INLINABLE observe #-}
