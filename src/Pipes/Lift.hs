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
    execWriterP
    ) where

import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
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
    -> (e -> Proxy a' a b' b (E.ErrorT f m) r)
    -> Proxy a' a b' b (E.ErrorT f m) r
catchError p0 f = go p0
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure   r       -> Pure r
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
    -- ^ Catch function for the base monad
    ->    (Proxy a' a b' b m r
        -> (e -> Proxy a' a b' b m r)
        -> Proxy a' a b' b m r)
    -- ^ Catch function for 'Proxy'
liftCatchError c p0 f = go p0
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure   r       -> Pure r
        M          m   -> M ((do
            p' <- m
            return (go p') ) `c` (\e -> return (f e)) )
{-# INLINABLE liftCatchError #-}

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
        Pure    r      -> Pure (r, w)
        M          m   -> M (do
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
