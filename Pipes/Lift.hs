module Pipes.Lift (
    runStateP,
    evalStateP,
    execStateP,
    runWriterP,
    execWriterP,
    runErrorP,
    catch,
    runMaybeP
    ) where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Data.Monoid
import Pipes.Core
import Prelude hiding (catch)

runStateP
    :: (Monad m)
    => s -> Proxy a' a b' b (StateT s m) r -> Proxy a' a b' b m (r, s)
runStateP = go
  where
    go s p = case p of
        Request a' fa  -> Request a' (\a  -> go s (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go s (fb' b'))
        Pure    r      -> Pure (r, s)
        M          m   -> M (do
            (p', s') <- runStateT m s
            return (go s' p') )
{-# INLINABLE runStateP #-}

evalStateP
    :: (Monad m) => s -> Proxy a' a b' b (StateT s m) r -> Proxy a' a b' b m r
evalStateP s = fmap fst . runStateP s
{-# INLINABLE evalStateP #-}

execStateP
    :: (Monad m) => s -> Proxy a' a b' b (StateT s m) r -> Proxy a' a b' b m s
execStateP s = fmap snd . runStateP s
{-# INLINABLE execStateP #-}

runWriterP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b (WriterT w m) r -> Proxy a' a b' b m (r, w)
runWriterP = go mempty
  where
    go w p = case p of
        Request a' fa  -> Request a' (\a  -> go w (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go w (fb' b'))
        Pure    r      -> Pure (r, w)
        M          m   -> M (do
            (p', w') <- runWriterT m
            let wt = mappend w w'
            wt `seq` return (go wt p') )
{-# INLINABLE runWriterP #-}

execWriterP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b (WriterT w m) r -> Proxy a' a b' b m w
execWriterP = fmap snd . runWriterP
{-# INLINABLE execWriterP #-}

runErrorP
    :: (Monad m)
    => Proxy a' a b' b (ErrorT e m) r -> Proxy a' a b' b m (Either e r)
runErrorP = go
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure    r      -> Pure (Right r)
        M          m   -> M (do
            x <- runErrorT m
            return (case x of
                Left  e  -> Pure (Left e)
                Right p' -> go p' ) )
{-# INLINABLE runErrorP #-}

catch
    :: (Monad m) 
    => Proxy a' a b' b (ErrorT e m) r
    -> (e -> Proxy a' a b' b (ErrorT f m) r)
    -> Proxy a' a b' b (ErrorT f m) r
catch p f = go p
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure   r       -> Pure r
        M          m   -> M (ErrorT (do
            x <- runErrorT m
            return (Right (case x of
                Left  e  -> f  e
                Right p' -> go p' )) ))
{-# INLINABLE catch #-}

runMaybeP
    :: (Monad m) => Proxy a' a b' b (MaybeT m) r -> Proxy a' a b' b m (Maybe r)
runMaybeP = go
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        Pure    r      -> Pure (Just r)
        M          m   -> M (do
            x <- runMaybeT m
            return (case x of
                Nothing -> Pure Nothing
                Just p' -> go p' ) )
{-# INLINABLE runMaybeP #-}
