module Pipes.Lift (
    -- * StateT
    runStateP,
    evalStateP,
    execStateP,

    -- * WriterT
    runWriterP,
    execWriterP,

    -- * ErrorT
    runErrorP,
    catch,
    liftCatch,

    -- * MaybeT
    runMaybeP
    ) where

import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
import Data.Monoid (Monoid(mempty, mappend))
import Pipes.Core
import Prelude hiding (catch)

-- | Run 'StateT' in the base monad
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
catch
    :: (Monad m) 
    => Proxy a' a b' b (E.ErrorT e m) r
    -> (e -> Proxy a' a b' b (E.ErrorT f m) r)
    -> Proxy a' a b' b (E.ErrorT f m) r
catch p f = go p
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
{-# INLINABLE catch #-}

liftCatch
    :: (Monad m)
    => (   m (Proxy a' a b' b m r)
        -> (e -> m (Proxy a' a b' b m r))
        -> m (Proxy a' a b' b m r) )
    -> Proxy a' a b' b m r
    -> (e -> Proxy a' a b' b m r)
    -> Proxy a' a b' b m r
liftCatch c p f = go p
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure   r       -> Pure r
        M          m   -> M (c (m >>= return . go) (\e -> return (f e)))
{-# INLINABLE liftCatch #-}

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
