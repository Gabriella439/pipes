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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pipes.Internal (
    Proxy(..),
    observe
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter(..))
import Data.Monoid (mempty)

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

instance MFunctor (Proxy a' a b' b) where
    hoist nat p0 = go (observe p0) where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (nat (m >>= \p' -> return (go p')))
            Pure       r   -> Pure r

instance (MonadIO m) => MonadIO (Proxy a' a b' b m) where
    liftIO m = M (liftIO (m >>= \r -> return (Pure r)))

instance (MonadReader r m) => MonadReader r (Proxy a' a b' b m) where
    ask = lift ask
    local f = go
        where
          go p = case p of
              Request a' fa  -> Request a' (\a  -> go (fa  a ))
              Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
              Pure    r      -> Pure r
              M       m      -> M (go `liftM` local f m)
    reader = lift . reader

instance (MonadState s m) => MonadState s (Proxy a' a b' b m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadWriter w m) => MonadWriter w (Proxy a' a b' b m) where
    writer = lift . writer
    tell = lift . tell
    listen = go mempty
        where
          go w p = case p of
              Request a' fa  -> Request a' (\a  -> go w (fa  a ))
              Respond b  fb' -> Respond b  (\b' -> go w (fb' b'))
              Pure    r      -> Pure (r, w)
              M       m      -> M ((\(p', w') -> go w' p') `liftM` listen m)

    pass = go
        where
          -- Pass needs to happen in the underlying MonadWriter, so we
          -- can only sensibly do something if the 'w -> w' function is
          -- produced inside M.
          --
          -- If we encounter an M, we first extract the 'w -> w' function
          -- using extract, then "pass" that in the underlying monad.
          go p = case p of
              Request a' fa  -> Request a' (\a  -> go (fa  a ))
              Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
              Pure    (r, _) -> Pure r
              M       m      -> M (pass (extract `liftM` m))
          -- Extract can only do something sensibly if there is a Pure
          -- embedded inside the M we're called on. So if we encounter
          -- a Pure, we lift the modification function out for 'go' to
          -- 'pass'. If there is no pure, the only sensibly thing is to
          -- not modify the writer state and recurse.
          extract p = case p of
              Request a' fa  -> (Request a' (\a  -> go (fa  a )), id)
              Respond b  fb' -> (Respond b  (\b' -> go (fb' b')), id)
              Pure    (r, f) -> (Pure r, f)
              M       m      -> (go (M m), id)

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
