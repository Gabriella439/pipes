{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Criterion.Main
import Common (commonMain)
import Control.Monad.Identity (Identity, runIdentity)
import Pipes
import qualified Pipes.Prelude as P
import Prelude hiding (enumFromTo)

defaultMax :: Int
defaultMax = 1000000

main :: IO ()
main = commonMain defaultMax preludeBenchmarks

enumFromTo :: (Int -> a) -> Int -> Int -> Producer a Identity ()
enumFromTo f n1 n2 = loop n1
    where
        loop n =
            if n <= n2
            then do
                yield $! f n
                loop $! n + 1
            else return ()
{-# INLINABLE enumFromTo #-}

drain :: Producer b Identity r -> r
drain p = runIdentity $ runEffect $ for p discard

msum :: (Monad m) => Producer Int m () -> m Int
msum = P.foldM (\a b -> return $ a + b) (return 0) return

scanMSum :: (Monad m) => Pipe Int Int m r
scanMSum = P.scanM (\x y -> return (x + y)) (return 0) return

-- Using runIdentity seems to reduce outlier counts.
preludeBenchmarks :: Int -> [Benchmark]
preludeBenchmarks vmax =
    let applyBench b = b benchEnum_p
        benchEnum_p  = enumFromTo id 1 vmax
    in
    [
      bgroup "Folds" $ map applyBench
        [
          bench "all"       . whnf (runIdentity . P.all (<= vmax))
        , bench "any"       . whnf (runIdentity . P.any (> vmax))
        , bench "find"      . whnf (runIdentity . P.find (== vmax))
        , bench "findIndex" . whnf (runIdentity . P.findIndex (== vmax))
        , bench "fold"      . whnf (runIdentity . P.fold (+) 0 id)
        , bench "foldM"     . whnf (runIdentity . msum)
        , bench "head"      . nf (runIdentity . P.head)
        , bench "index"     . nf (runIdentity . P.index (vmax-1))
        , bench "last"      . nf (runIdentity . P.last)
        , bench "length"    . whnf (runIdentity . P.length)
        , bench "null"      . whnf (runIdentity  . P.null)
        , bench "toList"    . nf P.toList
        ]
    , bgroup "Pipes" $ map applyBench
        [
          bench "chain"       . whnf (drain . (>-> P.chain (\_ -> return ())))
        , bench "drop"        . whnf (drain . (>-> P.drop vmax))
        , bench "dropWhile"   . whnf (drain . (>-> P.dropWhile (<= vmax)))
        , bench "filter"      . whnf (drain . (>-> P.filter even))
        , bench "findIndices" . whnf (drain . (>-> P.findIndices (<= vmax)))
        , bench "map"         . whnf (drain . (>-> P.map id))
        , bench "mapM"        . whnf (drain . (>-> P.mapM return))
        , bench "take"        . whnf (drain . (>-> P.take vmax))
        , bench "takeWhile"   . whnf (drain . (>-> P.takeWhile (<= vmax)))
        , bench "scan"        . whnf (drain . (>-> P.scan (+) 0 id))
        , bench "scanM"       . whnf (drain . (>-> scanMSum))
        ] ++ [
          bench "concat" $ whnf (drain . (>-> P.concat)) $ enumFromTo Just 1 vmax
        ]
    , bgroup "Zips" $ map applyBench
        [
          bench "zip"     . whnf (drain . P.zip benchEnum_p)
        , bench "zipWith" . whnf (drain . P.zipWith (+) benchEnum_p)
        ]
    , bgroup "enumFromTo.vs.each"
        [
          bench "enumFromTo" $ whnf (drain . enumFromTo id 1) vmax
        , bench "each"       $ whnf (drain . each) [1..vmax]
        ]
    ]
