{-| Many actions in base monad transformers cannot be automatically
    'Control.Monad.Trans.Class.lift'ed.  These functions lift these remaining
    actions so that they work in the 'Proxy' monad transformer.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Pipes.Lift (
    -- * ErrorT
    errorP,
    runErrorP,
    runErrorPB,
    catchError,
    liftCatchError,

    -- * MaybeT
    maybeP,
    runMaybeP,
    runMaybePB,

    -- * ReaderT
    readerP,
    runReaderP,
    runReaderPB,

    -- * StateT
    stateP,
    runStateP,
    runStatePB,
    evalStateP,
    evalStatePB,
    execStateP,
    execStatePB,

    -- * WriterT
    -- $writert
    writerP,
    runWriterP,
    runWriterPB,
    execWriterP,
    execWriterPB,

    -- * RWST
    rwsP,
    runRWSP,
    runRWSPB,
    evalRWSP,
    evalRWSPB,
    execRWSP,
    execRWSPB,

    runSubPipeT,
    runSubPipeTB,
    fromToLifted,
    fromToLiftedB,

    directionalize,
    ) where

-- import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Control.Monad.Trans.RWS.Strict as RWS

import Data.Monoid (Monoid(mempty, mappend))
import Pipes.Internal
import Pipes.Core

import Control.Monad.Morph -- (hoist)
-- import Control.Monad.Trans.Class ()
import qualified Data.Void as V

import Control.Monad (forever)

fromToLifted
  :: (Monad (t (Proxy a' a b' b m)), Monad m,
      Monad (t m), MFunctor t,
      MonadTrans t) =>
     Proxy a' a b' b (t m) r
     -> Proxy x' x y' y (t (Proxy a' a b' b m)) r
fromToLifted    =  directionalize fromToLiftedB

fromToLiftedB
  :: (Monad (t (Proxy a' a b' b m)), Monad m, Monad (t m),
      MonadTrans t, MFunctor t) =>
     (c -> Proxy a' a b' b (t m) r)
     -> c ->  Effect' (t (Proxy a' a b' b m)) r
fromToLiftedB p = (//> (lift . lift . respond)) --  yield two layers lower
                . ((lift . lift .request) >\\)  --  awiat two lyaers lower
                . hoist (hoist lift)            --  Insert new layer for pipe to
                                                -- connect to
                . p                             -- Proxy

runSubPipeT
  :: (Monad (t (Proxy x'1 b1 b' b m)), Monad m,
      Monad (t m), MFunctor t,
      MonadTrans t) =>
     Proxy x'1 b1 b' b (t m) r
     -> t (Proxy x'1 b1 b' b m) r
runSubPipeT    = directionalize runSubPipeTB

runSubPipeTB
  :: (Monad (t (Proxy x'1 b1 b' b m)), Monad m, Monad (t m),
      MonadTrans t, MFunctor t) =>
     (a -> Proxy x'1 b1 b' b (t m) r) -> a -> t (Proxy x'1 b1 b' b m) r
runSubPipeTB =  (runEffect .) . fromToLiftedB

-- | This can be considered the inverse of generalize from the Pipes.Prelude 
-- specialize :: forall x t . (x -> t) -> t
-- specialize :: (x -> Proxy x' a' x a m r) -> Proxy x' a' x a m r

specialize :: (() -> t) -> t
specialize p = p ()

generalize :: a -> b -> a
generalize = const

directionalize :: ((t -> a) -> () -> c) -> a -> c
directionalize p = specialize . p . generalize

-- | Wrap the base monad in 'E.ErrorT'
errorP
    :: (Monad m, E.Error e)
    => Proxy a' a b' b m (Either e r)
    -> Proxy a' a b' b (E.ErrorT e m) r
errorP p = do
    x <- unsafeHoist lift p
    lift $ E.ErrorT (return x)
{-# INLINABLE errorP #-}

-- | Run 'E.ErrorT' in the base monad
runErrorP
  :: (Monad m, E.Error e) =>
     Proxy a' a b' b (E.ErrorT e m) r
     -> Proxy a' a b' b m (Either e r)
runErrorP     = directionalize runErrorPB
{-# INLINABLE runErrorP #-}

runErrorPB
  :: (Monad m, E.Error e) =>
     (c
      -> Proxy a' a b' b (E.ErrorT e m) r)
     -> c -> Proxy a' a b' b m (Either e r)
runErrorPB    = (E.runErrorT .) . runSubPipeTB 
{-# INLINABLE runErrorPB #-}

-- | Catch an error in the base monad
catchError
    :: (Monad m, E.Error e) 
    => Proxy a' a b' b (E.ErrorT e m) r
    -- ^
    -> (e -> Proxy a' a b' b (E.ErrorT e m) r)
    -- ^
    -> Proxy a' a b' b (E.ErrorT e m) r
catchError e h = errorP . E.runErrorT $ 
    E.catchError (runSubPipeT e) (runSubPipeT . h)
{-# INLINABLE catchError #-}


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

-- | Run 'M.MaybeT' in the base monad
runMaybeP
  :: Monad m =>
     Proxy a' a b' b (M.MaybeT m) r
     -> Proxy a' a b' b m (Maybe r)
runMaybeP     = directionalize  runMaybePB
{-# INLINABLE runMaybeP #-}

runMaybePB
  :: Monad m =>
     (c -> Proxy a' a b' b (M.MaybeT m) r)
     -> c -> Proxy a' a b' b m (Maybe r)
runMaybePB    = (M.runMaybeT .)  . runSubPipeTB
{-# INLINABLE runMaybePB #-}

-- | Wrap the base monad in 'R.ReaderT'
readerP
    :: (Monad m)
    => (i -> Proxy a' a b' b m r) -> Proxy a' a b' b (R.ReaderT i m) r
readerP k = do
    i <- lift R.ask
    unsafeHoist lift (k i)
{-# INLINABLE readerP #-}

-- | Run 'R.ReaderT' in the base monad
runReaderP
  :: Monad m =>
     i
     -> Proxy a' a b' b (R.ReaderT i m) r
     -> Proxy a' a b' b m r
runReaderP    = directionalize . runReaderPB
{-# INLINABLE runReaderP #-}

runReaderPB
  :: Monad m =>
     i
     -> (c -> Proxy a' a b' b (R.ReaderT i m) r)
     -> c -> Proxy a' a b' b m r
runReaderPB r = ((`R.runReaderT` r) .) . runSubPipeTB
{-# INLINABLE runReaderPB #-}

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

-- | Run 'S.StateT' in the base monad
runStateP
  :: Monad m =>
     s
     -> Proxy a' a b' b (S.StateT s m) r
     -> Proxy a' a b' b m (r, s)
runStateP     = directionalize . runStatePB
{-# INLINABLE runStateP #-}

runStatePB
  :: Monad m =>
     s
     -> (c -> Proxy a' a b' b (S.StateT s m) r)
     ->  c -> Proxy a' a b' b m (r, s)
runStatePB  s = ((`S.runStateT` s) .) . runSubPipeTB
{-# INLINABLE runStatePB #-}

-- | Evaluate 'S.StateT' in the base monad
evalStateP
  :: Monad m =>
     s
     -> Proxy a' a b' b (S.StateT s m) r
     -> Proxy a' a b' b m r
evalStateP    = directionalize . evalStatePB
{-# INLINABLE evalStateP #-}

evalStatePB
  :: Monad m =>
     s
     -> (c -> Proxy a' a b' b (S.StateT s m) r)
     ->  c -> Proxy a' a b' b m r
evalStatePB s = (fmap fst .) . runStatePB s
{-# INLINABLE evalStatePB #-}

-- | Execute 'S.StateT' in the base monad
execStateP
  :: Monad m =>
     s
     -> Proxy a' a b' b (S.StateT s m) r
     -> Proxy a' a b' b m s
execStateP    = directionalize . execStatePB
{-# INLINABLE execStateP #-}

execStatePB
  :: Monad m =>
     s
     -> (c -> Proxy a' a b' b (S.StateT s m) r)
     ->  c -> Proxy a' a b' b m s
execStatePB s = (fmap snd .) . runStatePB s
{-# INLINABLE execStatePB #-}

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

-- | Run 'W.WriterT' in the base monad
runWriterP
  :: (Monad m, Data.Monoid.Monoid w) =>
     Proxy a' a b' b (W.WriterT w m) r
     -> Proxy a' a b' b m (r, w)
runWriterP    = directionalize runWriterPB
{-# INLINABLE runWriterP #-}

runWriterPB
  :: (Monad m, Data.Monoid.Monoid w) =>
     (c -> Proxy a' a b' b (W.WriterT w m) r)
     -> c -> Proxy a' a b' b m (r, w)
runWriterPB   = (W.runWriterT .) . runSubPipeTB
{-# INLINABLE runWriterPB #-}

-- | Execute 'W.WriterT' in the base monad
execWriterP
  :: (Monad m, Data.Monoid.Monoid w) =>
     Proxy a' a b' b (W.WriterT w m) r
     -> Proxy a' a b' b m w
execWriterP   = directionalize execWriterPB
{-# INLINABLE execWriterP #-}

execWriterPB
  :: (Monad m, Data.Monoid.Monoid w) =>
     (c -> Proxy a' a b' b (W.WriterT w m) r)
     -> c -> Proxy a' a b' b m w
execWriterPB  = (fmap snd .) . runWriterPB
{-# INLINABLE execWriterPB #-}


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

-- | Run 'RWS.RWST' in the base monad
runRWSP
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> Proxy a' a b' b (RWS.RWST r w s m) d
     -> Proxy a' a b' b m (d, s, w)
runRWSP         = (directionalize .) . runRWSPB 
{-# INLINABLE runRWSP #-}

runRWSPB
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> (c -> Proxy a' a b' b (RWS.RWST r w s m) d)
     -> c -> Proxy a' a b' b m (d, s, w)
runRWSPB  i s p = (\b -> RWS.runRWST b i s) . runSubPipeTB p
{-# INLINABLE runRWSPB #-}

-- | Evaluate 'RWS.RWST' in the base monad
evalRWSP
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> Proxy a' a b' b (RWS.RWST r w s m) d
     -> Proxy a' a b' b m (d, w)
evalRWSP      = (directionalize .) . evalRWSPB
{-# INLINABLE evalRWSP #-}

evalRWSPB
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> (c -> Proxy a' a b' b (RWS.RWST r w s m) d)
     -> c -> Proxy a' a b' b m (d, w)
evalRWSPB i s = (fmap f .) . runRWSPB i s
  where f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSPB #-}


-- todo fix type sigs below
-- | Execute 'RWS.RWST' in the base monad
execRWSP
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> Proxy a' a b' b (RWS.RWST r w s m) d
     -> Proxy a' a b' b m (s, w)
execRWSP      = (directionalize .) . execRWSPB
{-# INLINABLE execRWSP #-}

execRWSPB
  :: (Monad m, Monoid w) =>
     r
     -> s
     -> (c -> Proxy a' a b' b (RWS.RWST r w s m) d)
     -> c -> Proxy a' a b' b m (s, w)
execRWSPB i s = (fmap f .) . runRWSPB i s
  where f x = let (_, s, w) = x in (s, w)
{-# INLINABLE execRWSPB #-}
