{-| Many actions in base monad transformers cannot be automatically
    'Control.Monad.Trans.Class.lift'ed.  These functions lift these remaining
    actions so that they work in the 'Proxy' monad transformer.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE ImpredicativeTypes#-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

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

    directionalize,
    specialize, 
    runEffect',
    unitD,
--    unitU
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

(>->)
    :: (Monad m)
    => Proxy a' a () b m r
    -- ^
    -> Proxy () b c' c m r
    -- ^
    -> Proxy a' a c' c m r

p1 >-> p2 = (\() -> p1) +>> p2
{-# INLINABLE (>->) #-}

await :: (Monad m) => Consumer' a m a
await = request ()
{-# INLINABLE await #-}

yield :: (Monad m) => a -> Producer' a m ()
yield = respond
{-# INLINABLE yield #-}

fromToLifted
  :: (Monad (t m), Monad (t (Pipe a b m)), Monad m, 
      MonadTrans t, MFunctor t) =>
     Pipe a b (t m) r -> Effect (t (Pipe a b m)) r
fromToLifted    =  directionalize fromToLiftedB

fromToLiftedB
  :: (Monad (t (Proxy x'1 b1 b' b m)), Monad m, Monad (t m),
      MonadTrans t, MFunctor t) =>
     (a -> Proxy x'1 b1 b' b (t m) a')
     -> a ->  Effect' (t (Proxy x'1 b1 b' b m)) a'
fromToLiftedB p = (//> (lift . lift . respond)) --  yield two layers lower
                . ((lift . lift .request) >\\)  --  awiat two lyaers lower
                . hoist (hoist lift)            --  Insert new layer for pipe to
                                                -- connect to
                . p                             -- Proxy


{-
runSubPipeT
  :: (Monad (t m), Monad (t (Pipe' a b m)), Monad m,
      MonadTrans t, MFunctor t) =>
     Pipe' a b (t m) r -> t (Pipe' a b m) r
runSubPipeT
  :: (Monad (t m), Monad (t (Proxy x b1 x b m)),
      Monad m, Control.Monad.Morph.MFunctor t,
      MonadTrans t) =>
     Pipes.Internal.Proxy b'1 b1 b' b (t m) r
     -> t (Pipes.Internal.Proxy x b1 x b m) r
-}
runSubPipeT    = directionalize runSubPipeTB

runSubPipeTB
  :: (Monad (t (Proxy x'1 b1 b' b m)), Monad m, Monad (t m),
      MonadTrans t, MFunctor t) =>
     (a -> Proxy x'1 b1 b' b (t m) r) -> a -> t (Proxy x'1 b1 b' b m) r
runSubPipeTB =  (runEffect' .) . fromToLiftedB

unitD :: Monad m => Proxy x' x () () m b
unitD = forever $ yield ()

{-
unitU :: Monad m => Proxy () a () () m b
unitU = forever $ await >> yield ()
-}

runEffect'
  :: Monad m =>
     Proxy () () () V.Void m r -> m r
runEffect'   p = runEffect $ unitD  >-> p -- >-> unitU

-- | This can be considered the inverse of generalize from the Pipes.Prelude 
specialize :: forall x t . (x -> t) -> t
-- specialize :: (x -> Proxy x' a' x a m r) -> Proxy x' a' x a m r
specialize p = p undefined

{-
directionalize
  :: Monad m =>
     ((s -> Proxy s b1 s b m r) -> a -> c) -> Proxy () b1 () b m r -> c
-}
{-
directionalize
  :: Monad m => forall x .
     ((s -> Proxy s b1 s b m r) -> x -> c)
     -> Proxy x b1 x b m r -> c
-}
-- directionalize p = specialize . p . generalize
directionalize p = (specialize . p) . (\f -> (\_ -> f))

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
     Pipe a b (E.ErrorT e m) r -> Pipe a b m (Either e r)
runErrorP     = directionalize runErrorPB
{-# INLINABLE runErrorP #-}

runErrorPB
  :: (Monad m, E.Error e) =>
     (a -> Proxy x'1 b1 b' b (E.ErrorT e m) a1)
     -> a -> Proxy x'1 b1 b' b m (Either e a1)
runErrorPB    = (E.runErrorT .) . runSubPipeTB 
{-# INLINABLE runErrorPB #-}

-- | Catch an error in the base monad
catchError
    :: (Monad m) 
    => Proxy a' a b' b (E.ErrorT e m) r
    -- ^
    -> (e -> Proxy a' a b' b (E.ErrorT f m) r)
    -- ^
    -> Proxy a' a b' b (E.ErrorT f m) r
catchError p0 f = go p0
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure    r      -> Pure r
        M          m   -> M (E.ErrorT (do
            x <- E.runErrorT m
            return (Right (case x of
                Left  e  -> f  e
                Right p' -> go p' )) ))
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
     Pipe a b (M.MaybeT m) r -> Pipe a b m (Maybe r)
runMaybeP     = directionalize  runMaybePB
{-# INLINABLE runMaybeP #-}

runMaybePB
  :: Monad m => 
     (a -> Proxy x'1 b1 b' b (M.MaybeT m) a1)
     -> a -> Proxy x'1 b1 b' b m (Maybe a1)
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
     i -> Pipe a b (R.ReaderT i m) r -> Pipe a b m r
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
{-
runStateP
  :: Monad m =>
     s -> Pipe a b (S.StateT s m) r -> Pipe a b m (r, s)
-}
{-
evalStateP
  :: Monad m =>
     a
     -> Proxy
          x1 a' () a1 (S.StateT a m) r
     -> Proxy x' a' x a1 m r
-}
runStateP     = directionalize . runStatePB
{-# INLINABLE runStateP #-}

runStatePB
  :: Monad m => 
     s
     -> (c -> Proxy a' a b' b (S.StateT s m) r)
     -> c
     -> Proxy a' a b' b m (r, s)
runStatePB  s = ((`S.runStateT` s) .) . runSubPipeTB
{-# INLINABLE runStatePB #-}

-- | Evaluate 'S.StateT' in the base monad
{-
evalStateP
  :: Monad m =>
     c -> Pipe a b (S.StateT c m) r -> Pipe a b m r
-}
evalStateP    = directionalize . evalStatePB
{-# INLINABLE evalStateP #-}

evalStatePB
  :: Monad m =>
     s
     -> (c -> Proxy a' a b' b (S.StateT s m) r)
     -> c
     -> Proxy a' a b' b m r
evalStatePB s = (fmap fst .) . runStatePB s
{-# INLINABLE evalStatePB #-}

-- | Execute 'S.StateT' in the base monad
execStateP
  :: Monad m =>
     s -> Pipe a b (S.StateT s m) r -> Pipe a b m s
execStateP    = directionalize . execStatePB
{-# INLINABLE execStateP #-}

execStatePB
  :: Monad m =>
     s
     -> (c -> Proxy a' a b' b (S.StateT s m) r)
     -> c
     -> Proxy a' a b' b m s
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
     Pipe a b (W.WriterT w m) r -> Pipe a b m (r, w) 
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
     Pipe a b (W.WriterT w m) r -> Pipe a b m w
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
  :: (Monad m, Data.Monoid.Monoid w) =>
     a
     -> a1
     -> Pipe a2 b (RWS.RWST a w a1 m) r
     -> Proxy () a2 () b m (r, a1, w)
runRWSP         = (directionalize .) . runRWSPB 
{-# INLINABLE runRWSP #-}

runRWSPB
  :: (Monad m, Data.Monoid.Monoid w) =>
     r
     -> s
     -> (a -> Proxy x'1 b1 b' b (RWS.RWST r w s m) a1)
     -> a
     -> Proxy x'1 b1 b' b m (a1, s, w)
runRWSPB  i s p = (\b -> RWS.runRWST b i s) . runSubPipeTB p
{-# INLINABLE runRWSPB #-}

-- | Evaluate 'RWS.RWST' in the base monad
evalRWSP
  :: (Monad m, Data.Monoid.Monoid t2) =>
     a
     -> a1
     -> Pipe a2 b (RWS.RWST a t2 a1 m) r
     -> Pipe a2 b m (r, t2)
evalRWSP      = (directionalize .) . evalRWSPB
{-# INLINABLE evalRWSP #-}

evalRWSPB
  :: (Monad m, Data.Monoid.Monoid t2) =>
     r
     -> t
     -> (a -> Proxy x'1 b1 b' b (RWS.RWST r t2 t m) t1)
     -> a
     -> Proxy x'1 b1 b' b m (t1, t2)
evalRWSPB i s = (fmap f .) . runRWSPB i s
  where f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSPB #-}


-- todo fix type sigs below
-- | Execute 'RWS.RWST' in the base monad
execRWSP
  :: (Monad m, Data.Monoid.Monoid t) =>
     a
     -> a1
     -> Pipe a2 b (RWS.RWST a t a1 m) r
     -> Pipe a2 b m (a1, t) 
execRWSP      = (directionalize .) . execRWSPB
{-# INLINABLE execRWSP #-}

execRWSPB
  :: (Monad m, Data.Monoid.Monoid t2) =>
     r
     -> t1
     -> (a -> Proxy x'1 b1 b' b (RWS.RWST r t2 t1 m) t)
     -> a
     -> Proxy x'1 b1 b' b m (t1, t2)
execRWSPB i s = (fmap f .) . runRWSPB i s
  where f x = let (_, s, w) = x in (s, w)
{-# INLINABLE execRWSPB #-}

{- todo

evalRWSP      = (directionalize .) . evalRWSPB


evalRWSPB i s = (fmap f .) . runRWSPB i s
  where f x = let (r, _, w) = x in (r, w)
-}

