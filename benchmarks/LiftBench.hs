{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Common (commonMain)
import Control.DeepSeq
import Control.Monad.Identity
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Criterion.Main
import Data.Monoid
import Pipes
import Pipes.Lift

defaultMax :: Int
defaultMax = 1000000

instance NFData a => NFData (Sum a)

main :: IO ()
main = commonMain defaultMax liftBenchmarks

iter :: forall m a . (Monad m , Ord a, Num a) => (a -> m a) -> a -> Effect m a
iter a vmax = loop 0
    where
        loop n
            | n > vmax  = return vmax
            | otherwise = do
                x <- lift $ a n
                loop $! x

s_bench :: Int -> Effect (S.StateT Int Identity) Int
s_bench = iter (\n -> S.get >>= (\a -> S.put $! a + n) >> return (n + 1))

w_bench :: Int -> Effect (W.WriterT (Sum Int) Identity) Int
w_bench = iter (\n -> (W.tell $! Sum n) >> return (n + 1))

r_bench :: Int -> Effect (R.ReaderT Int Identity) Int
r_bench = iter (\n -> R.ask >>= (\a -> return $ n + a))

rwsp_bench :: Int -> Effect (RWS.RWST Int (Sum Int) Int Identity) Int
rwsp_bench = iter act
    where
        act n = do
            x <- RWS.ask
            RWS.tell (Sum n)
            s <- RWS.get
            RWS.put $! (s + n)
            return $ n + x

-- Run before Proxy
runB :: (a -> Effect Identity r) -> a -> r
runB f a = runIdentity $ runEffect $ f a

-- Run after Proxy
runA :: (Monad m) => (m r -> Identity a) -> Effect m r -> a
runA f a = runIdentity $ f (runEffect a)

liftBenchmarks :: Int -> [Benchmark]
liftBenchmarks vmax =
    let applyBench = map ($ vmax)
    in
    [
      bgroup "ReaderT" $
        let defT f = (\d -> f d 1)
        in applyBench
        [
          bench "runReaderP_B" . whnf (runB (runReaderP 1) . r_bench)
        , bench "runReaderP_A" . whnf (runA (defT R.runReaderT) . r_bench)
        ]
    , bgroup "StateT" $
        let defT f = (\s -> f s 0)
        in applyBench
        [
          bench "runStateP_B"  . nf (runB (runStateP 0) . s_bench)
        , bench "runStateP_A"  . nf (runA (defT S.runStateT) . s_bench)
        , bench "evalStateP_B" . whnf (runB (evalStateP 0) . s_bench)
        , bench "evalStateP_A" . whnf (runA (defT S.evalStateT) . s_bench)
        , bench "execStateP_B" . whnf (runB (execStateP 0) . s_bench)
        , bench "execStateP_A" . whnf (runA (defT S.execStateT) . s_bench)
        ]
{-
    , bgroup "WriterT" $ applyBench
        [
        -- Running WriterP after runEffect will space leak.
          bench "runWriterP_B"  . nf (runB runWriterP . w_bench)
        , bench "execWriterP_B" . nf (runB execWriterP . w_bench)
        ]
    , bgroup "RWSP" $
        let defT f = (\d -> f d 1 0)
        in applyBench
        [
          bench "runRWSP_B"  . nf (runB (runRWSP 1 0) . rwsp_bench)
        , bench "runRWSP_A"  . nf (runA (defT RWS.runRWST) . rwsp_bench)
        , bench "evalRWSP_B" . nf (runB (evalRWSP 1 0) . rwsp_bench)
        , bench "evalRWSP_A" . nf (runA (defT RWS.evalRWST) . rwsp_bench)
        , bench "execRWSP_B" . nf (runB (execRWSP 1 0) . rwsp_bench)
        , bench "execRWSP_A" . nf (runA (defT RWS.execRWST) . rwsp_bench)
        ]
-}
    ]
