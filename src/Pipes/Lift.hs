{-| Many actions in base monad transformers cannot be automatically
    'Control.Monad.Trans.Class.lift'ed.  These functions lift these remaining
    actions so that they work in the 'Proxy' monad transformer.
-}

{-# LANGUAGE CPP #-}

module Pipes.Lift (
    -- * ErrorT
    runErrorP,
    catchError,
    liftCatchError,

    -- * MaybeT
    runMaybeP,

    -- * ReaderT
    runReaderP,

    -- * StateT
    runStateP,
    evalStateP,
    execStateP,

    -- * WriterT
    -- $writert
    runWriterP,
    execWriterP,

    -- * RWST
    runRWSP,
    evalRWSP,
    execRWSP
    ) where

import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Data.Monoid (Monoid(mempty, mappend))
import Pipes.Internal

-- | Run 'E.ErrorT' in the base monad
runErrorP
    :: (Monad m)
    => Proxy a' a b' b (E.ErrorT e m) r -> Proxy a' a b' b m (Either e r)
runErrorP = go
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure    r      -> Pure (Right r)
        M          m   -> M (do
            x <- E.runErrorT m
            return (case x of
                Left  e  -> Pure (Left e)
                Right p' -> go p' ) )
{-# INLINABLE runErrorP #-}

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

-- | Run 'M.MaybeT' in the base monad
runMaybeP
    :: (Monad m)
    => Proxy a' a b' b (M.MaybeT m) r -> Proxy a' a b' b m (Maybe r)
runMaybeP = go
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure    r      -> Pure (Just r)
        M          m   -> M (do
            x <- M.runMaybeT m
            return (case x of
                Nothing -> Pure Nothing
                Just p' -> go p' ) )
{-# INLINABLE runMaybeP #-}

-- | Run 'R.ReaderT' in the base monad
runReaderP
    :: (Monad m)
    => i -> Proxy a' a b' b (R.ReaderT i m) r -> Proxy a' a b' b m r
runReaderP i = go
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure    r      -> Pure r
        M          m   -> M (do
            p' <- R.runReaderT m i
            return (go p') )
{-# INLINABLE runReaderP #-}

-- | Run 'S.StateT' in the base monad
runStateP
    :: (Monad m)
    => s -> Proxy a' a b' b (S.StateT s m) r -> Proxy a' a b' b m (r, s)
runStateP = go
  where
    go s p = case p of
        Request a' fa  -> Request a' (\a  -> go s (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go s (fb' b'))
        Pure    r      -> Pure (r, s)
        M          m   -> M (do
            (p', s') <- S.runStateT m s
            return (go s' p') )
{-# INLINABLE runStateP #-}

-- | Evaluate 'S.StateT' in the base monad
evalStateP
    :: (Monad m) => s -> Proxy a' a b' b (S.StateT s m) r -> Proxy a' a b' b m r
evalStateP s = fmap fst . runStateP s
{-# INLINABLE evalStateP #-}

-- | Execute 'S.StateT' in the base monad
execStateP
    :: (Monad m) => s -> Proxy a' a b' b (S.StateT s m) r -> Proxy a' a b' b m s
execStateP s = fmap snd . runStateP s
{-# INLINABLE execStateP #-}

{- $writert
    Note that 'runWriterP' and 'execWriterP' will keep the accumulator in
    weak-head-normal form so that folds run in constant space when possible.

    This means that until @transformers@ adds a truly strict 'W.WriterT', you
    should consider unwrapping 'W.WriterT' first using 'runWriterP' or
    'execWriterP' before 'Pipes.run'ning your 'Proxy'.  You will get better
    performance this way and eliminate space leaks if your accumulator doesn't
    have any lazy fields.
-}

-- | Run 'W.WriterT' in the base monad
runWriterP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b (W.WriterT w m) r -> Proxy a' a b' b m (r, w)
runWriterP = go mempty
  where
    go w p = case p of
        Request a' fa  -> Request a' (\a  -> go w (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go w (fb' b'))
        Pure  r      -> Pure (r, w)
        M        m   -> M (do
            (p', w') <- W.runWriterT m
            let wt = mappend w w'
            wt `seq` return (go wt p') )
{-# INLINABLE runWriterP #-}

-- | Execute 'W.WriterT' in the base monad
execWriterP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b (W.WriterT w m) r -> Proxy a' a b' b m w
execWriterP = fmap snd . runWriterP
{-# INLINABLE execWriterP #-}

-- | Run 'RWST' in the base monad
runRWSP :: (Monad m, Monoid w)
        => i
        -> s
        -> Proxy a' a b' b (RWS.RWST i w s m) r
        -> Proxy a' a b' b m (r, s, w)
runRWSP i = go mempty
  where
    go w s p = case p of
        Request a' fa  -> Request a' (\a  -> go w s (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go w s (fb' b'))
        Pure    r      -> Pure (r, s, w)
        M          m   -> M (do
            (p', s', w') <- RWS.runRWST m i s
            let wt = mappend w w'
            wt `seq` return (go w' s' p') )
{-# INLINABLE runRWSP #-}

-- | Evaluate 'RWST' in the base monad
evalRWSP :: (Monad m, Monoid w)
         => i
         -> s
         -> Proxy a' a b' b (RWS.RWST i w s m) r
         -> Proxy a' a b' b m (r, w)
evalRWSP i s = fmap go . runRWSP i s
    where go (r, _, w) = (r, w)
{-# INLINABLE evalRWSP #-}

-- | Execute 'RWST' in the base monad
execRWSP :: (Monad m, Monoid w)
         => i
         -> s
         -> Proxy a' a b' b (RWS.RWST i w s m) r
         -> Proxy a' a b' b m (s, w)
execRWSP i s = fmap go . runRWSP i s
    where go (_, s', w) = (s', w)
{-# INLINABLE execRWSP #-}
