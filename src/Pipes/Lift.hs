{-| Many actions in base monad transformers cannot be automatically
    'Control.Monad.Trans.Class.lift'ed.  These functions lift these remaining
    actions so that they work in the 'Proxy' monad transformer.
-}

{-# LANGUAGE CPP #-}

module Pipes.Lift (
    -- * ErrorT
      errorP
#ifndef haskell98
    , runErrorP
    , catchError
#endif
    , liftCatchError

    -- * MaybeT
    , maybeP
#ifndef haskell98
    , runMaybeP
#endif

    -- * ReaderT
    , readerP
#ifndef haskell98
    , runReaderP
#endif

    -- * StateT
    , stateP
#ifndef haskell98
    , runStateP
    , evalStateP
    , execStateP
#endif

    -- * WriterT
    -- $writert
    , writerP
#ifndef haskell98
    , runWriterP
    , execWriterP
#endif

    -- * RWST
    , rwsP
#ifndef haskell98
    , runRWSP
    , evalRWSP
    , execRWSP

    -- * Utilities
    , distribute
#endif

    ) where

import Control.Monad.Trans.Class (lift, MonadTrans(..))
import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Data.Monoid (Monoid)
import Pipes.Internal (Proxy(..), unsafeHoist)
#ifndef haskell98
import Control.Monad.Morph (hoist, MFunctor(..))
import Pipes.Core (runEffect, request, respond, (//>), (>\\))
#endif

-- | Wrap the base monad in 'E.ErrorT'
errorP
    :: (Monad m, E.Error e)
    => Proxy a' a b' b m (Either e r)
    -> Proxy a' a b' b (E.ErrorT e m) r
errorP p = do
    x <- unsafeHoist lift p
    lift $ E.ErrorT (return x)
{-# INLINABLE errorP #-}

#ifndef haskell98
-- | Run 'E.ErrorT' in the base monad
runErrorP
    :: (Monad m, E.Error e)
    => Proxy a' a b' b (E.ErrorT e m) r
    -> Proxy a' a b' b m (Either e r)
runErrorP    = E.runErrorT . distribute 
{-# INLINABLE runErrorP #-}

-- | Catch an error in the base monad
catchError
    :: (Monad m, E.Error e) 
    => Proxy a' a b' b (E.ErrorT e m) r
    -- ^
    -> (e -> Proxy a' a b' b (E.ErrorT e m) r)
    -- ^
    -> Proxy a' a b' b (E.ErrorT e m) r
catchError e h = errorP . E.runErrorT $ 
    E.catchError (distribute e) (distribute . h)
{-# INLINABLE catchError #-}
#endif

-- | Catch an error using a catch function for the base monad
liftCatchError
    :: (Monad m)
    => (   m (Proxy a' a b' b m r)
        -> (e -> m (Proxy a' a b' b m r))
        -> m (Proxy a' a b' b m r) )
    -- ^
    ->    (Proxy a' a b' b m r
        -> (e -> Proxy a' a b' b m r)
        -> Proxy a' a b' b m r)
    -- ^
liftCatchError c p0 f = go p0
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure    r      -> Pure r
        M          m   -> M ((do
            p' <- m
            return (go p') ) `c` (\e -> return (f e)) )
{-# INLINABLE liftCatchError #-}

-- | Wrap the base monad in 'M.MaybeT'
maybeP
    :: (Monad m)
    => Proxy a' a b' b m (Maybe r) -> Proxy a' a b' b (M.MaybeT m) r
maybeP p = do
    x <- unsafeHoist lift p
    lift $ M.MaybeT (return x)
{-# INLINABLE maybeP #-}

#ifndef haskell98
-- | Run 'M.MaybeT' in the base monad
runMaybeP
    :: (Monad m)
    => Proxy a' a b' b (M.MaybeT m) r
    -> Proxy a' a b' b m (Maybe r)
runMaybeP p = M.runMaybeT $ distribute p
{-# INLINABLE runMaybeP #-}
#endif

-- | Wrap the base monad in 'R.ReaderT'
readerP
    :: (Monad m)
    => (i -> Proxy a' a b' b m r) -> Proxy a' a b' b (R.ReaderT i m) r
readerP k = do
    i <- lift R.ask
    unsafeHoist lift (k i)
{-# INLINABLE readerP #-}

#ifndef haskell98
-- | Run 'R.ReaderT' in the base monad
runReaderP
    :: (Monad m)
    => i
    -> Proxy a' a b' b (R.ReaderT i m) r
    -> Proxy a' a b' b m r
runReaderP r p = (`R.runReaderT` r) $ distribute p
{-# INLINABLE runReaderP #-}
#endif

-- | Wrap the base monad in 'S.StateT'
stateP
    :: (Monad m)
    => (s -> Proxy a' a b' b m (r, s)) -> Proxy a' a b' b (S.StateT s m) r
stateP k = do
    s <- lift S.get
    (r, s') <- unsafeHoist lift (k s)
    lift (S.put s')
    return r
{-# INLINABLE stateP #-}

#ifndef haskell98
-- | Run 'S.StateT' in the base monad
runStateP
    :: (Monad m)
    => s
    -> Proxy a' a b' b (S.StateT s m) r
    -> Proxy a' a b' b m (r, s)
runStateP s p = (`S.runStateT` s) $ distribute p
{-# INLINABLE runStateP #-}

-- | Evaluate 'S.StateT' in the base monad
evalStateP
    :: (Monad m)
    => s
    -> Proxy a' a b' b (S.StateT s m) r
    -> Proxy a' a b' b m r
evalStateP s p = fmap fst $ runStateP s p
{-# INLINABLE evalStateP #-}

-- | Execute 'S.StateT' in the base monad
execStateP
    :: (Monad m)
    => s
    -> Proxy a' a b' b (S.StateT s m) r
    -> Proxy a' a b' b m s
execStateP s p = fmap snd $ runStateP s p
{-# INLINABLE execStateP #-}
#endif

{- $writert
    Note that 'runWriterP' and 'execWriterP' will keep the accumulator in
    weak-head-normal form so that folds run in constant space when possible.

    This means that until @transformers@ adds a truly strict 'W.WriterT', you
    should consider unwrapping 'W.WriterT' first using 'runWriterP' or
    'execWriterP' before running your 'Proxy'.  You will get better performance
    this way and eliminate space leaks if your accumulator doesn't have any lazy
    fields.
-}

-- | Wrap the base monad in 'W.WriterT'
writerP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b m (r, w) -> Proxy a' a b' b (W.WriterT w m) r
writerP p = do
    (r, w) <- unsafeHoist lift p
    lift $ W.tell w
    return r
{-# INLINABLE writerP #-}

#ifndef haskell98
-- | Run 'W.WriterT' in the base monad
runWriterP
    :: (Monad m, Data.Monoid.Monoid w)
    => Proxy a' a b' b (W.WriterT w m) r
    -> Proxy a' a b' b m (r, w)
runWriterP p = W.runWriterT $ distribute p
{-# INLINABLE runWriterP #-}

-- | Execute 'W.WriterT' in the base monad
execWriterP
    :: (Monad m, Data.Monoid.Monoid w)
    => Proxy a' a b' b (W.WriterT w m) r
    -> Proxy a' a b' b m w
execWriterP p = fmap snd $ runWriterP p
{-# INLINABLE execWriterP #-}
#endif

-- | Wrap the base monad in 'RWS.RWST'
rwsP
    :: (Monad m, Monoid w)
    => (i -> s -> Proxy a' a b' b m (r, s, w))
    -> Proxy a' a b' b (RWS.RWST i w s m) r
rwsP k = do
    i <- lift RWS.ask
    s <- lift RWS.get
    (r, s', w) <- unsafeHoist lift (k i s)
    lift $ do
        RWS.put s'
        RWS.tell w
    return r
{-# INLINABLE rwsP #-}

#ifndef haskell98
-- | Run 'RWS.RWST' in the base monad
runRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Proxy a' a b' b (RWS.RWST r w s m) d
    -> Proxy a' a b' b m (d, s, w)
runRWSP  i s p = (\b -> RWS.runRWST b i s) $ distribute p
{-# INLINABLE runRWSP #-}

-- | Evaluate 'RWS.RWST' in the base monad
evalRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Proxy a' a b' b (RWS.RWST r w s m) d
    -> Proxy a' a b' b m (d, w)
evalRWSP i s p = fmap f $ runRWSP i s p
  where
    f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSP #-}

-- | Execute 'RWS.RWST' in the base monad
execRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Proxy a' a b' b (RWS.RWST r w s m) d
    -> Proxy a' a b' b m (s, w)
execRWSP i s p = fmap f $ runRWSP i s p
  where
    f x = let (_, s', w) = x in (s', w)
{-# INLINABLE execRWSP #-}

-- | Distribute 'Proxy' over a monad transformer
distribute
    ::  ( Monad m
        , MonadTrans t
        , MFunctor t
        , Monad (t m)
        , Monad (t (Proxy a' a b' b m))
        )
    => Proxy a' a b' b (t m) r
    -- ^ 
    -> t (Proxy a' a b' b m) r
    -- ^ 
distribute p =  runEffect $ request' >\\ unsafeHoist (hoist lift) p //> respond'
  where
    request' = lift . lift . request
    respond' = lift . lift . respond
{-# INLINABLE distribute #-}
#endif
