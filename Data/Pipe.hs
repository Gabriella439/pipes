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
module Data.Pipe where

import Control.Category
import Control.Monad
import Control.Monad.Trans
import Data.Coroutine
import Data.Monoid
import Prelude hiding ((.), id)

data PipeF a b x = Await (a -> x) | Yield (b, x)

instance Functor (PipeF a b) where
    fmap f p = case p of
        Await x -> Await $ fmap f x
        Yield x -> Yield $ fmap f x

type Pipe a b m r = Coroutine (PipeF a b) m r
type Producer b m r = Pipe () b m r
type Consumer a m r = Pipe a () m r

await :: Pipe a b m a
await   = F $ Await Pure 

yield :: b -> Pipe a b m ()
yield x = F $ Yield (x, Pure ())

pipe :: (Monad m) => (a -> b) -> Pipe a b m c
pipe f = forever $ await >>= yield . f

newtype Lazy   m r a b = Lazy   { unLazy   :: Pipe a b m r}
newtype Strict m r a b = Strict { unStrict :: Pipe a b m r}

p1 >-< p2 = unStrict (Strict p1 . Strict p2)
p1 >+< p2 = unLazy   (Lazy   p1 . Lazy   p2)
infixr 9 >+<, >-<

instance (Monad m) => Category (Lazy m r) where
    id = Lazy $ pipe id
    Lazy p1' . Lazy p2' = Lazy $ case (p1', p2') of
        (F (Yield (x1, p1)), p2                ) -> yield x1 >> p1 >+< p2
        (M m1              , p2                ) -> lift m1 >>= \p1 -> p1 >+< p2
        (Pure r1           , _                 ) -> Pure r1
        (F (Await f1)      , F (Yield (x2, p2))) -> f1 x2 >+< p2
        (p1                , F (Await f2)      ) -> await >>= \x -> p1 >+< f2 x
        (p1                , M m2              ) -> lift m2 >>= \p2 -> p1 >+< p2
        (_                 , Pure r2           ) -> Pure r2

instance (Monad m) => Category (Strict m r) where
    id = Strict $ pipe id
    Strict p1' . Strict p2' = Strict $ case (p1', p2') of
        (_                 , Pure r2           ) -> Pure r2
        (p1                , M m2              ) -> lift m2 >>= \p2 -> p1 >-< p2
        (p1                , F (Await f2)      ) -> await >>= \x -> p1 >-< f2 x
        (F (Await f1)      , F (Yield (x2, p2))) -> f1 x2 >-< p2
        (Pure r1           , _                 ) -> Pure r1
        (M m1              , p2                ) -> lift m1 >>= \p1 -> p1 >-< p2
        (F (Yield (x1, p1)), p2                ) -> yield x1 >> p1 >-< p2

runPipe :: (Monad m) => Pipe () () m r -> m r
runPipe p' = case p' of
    Pure r            -> return r
    M mp              -> mp >>= runPipe
    F (Await f      ) -> runPipe $ f ()
    F (Yield ((), p)) -> runPipe p
