{-
    Copyright 2012 Gabriel Gonzalez

    This file is part of the Haskell Pipes Library.

    The is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hPDB is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with hPDB.  If not, see <http://www.gnu.org/licenses/>.
-}
module Data.Coroutine where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

data Coroutine f m r =
    Pure r | M (m (Coroutine f m r)) | F (f (Coroutine f m r))

instance (Functor f, Monad m) => Functor (Coroutine f m) where
    fmap f c = case c of
        Pure r -> Pure $ f r
        M mc -> M $ liftM (fmap f) mc
        F fc -> F $ fmap  (fmap f) fc

instance (Functor f, Monad m) => Applicative (Coroutine f m) where
    pure = Pure
    f <*> x = case f of
        Pure r -> fmap r x
        M mc -> M $ liftM (<*> x) mc
        F fc -> F $ fmap  (<*> x) fc

instance (Functor f, Monad m) => Monad (Coroutine f m) where
    return = pure
    m >>= f = case m of
        Pure r -> f r
        M mc -> M $ liftM (>>= f) mc
        F fc -> F $ fmap  (>>= f) fc

instance MonadTrans (Coroutine f) where lift = M . liftM Pure
