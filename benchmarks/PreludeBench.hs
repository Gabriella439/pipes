{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Criterion.Main hiding (run, parseArgs)
import Common (commonMain)
import Control.Foldl (FoldM)
import qualified Control.Foldl as L
import Control.Monad.Identity (Identity, runIdentity)
import Pipes
import qualified Pipes.Prelude as P
import Prelude hiding (enumFromTo)

defaultMax :: Int
defaultMax = 1000000

main :: IO ()
main = commonMain defaultMax preludeBenchmarks

enumFromTo :: Int -> Int -> Producer Int Identity ()
enumFromTo n1 n2 = loop n1
    where
        loop n =
            if n <= n2
            then do
                yield n
                loop $! n + 1
            else return ()
{-# INLINABLE enumFromTo #-}

drain :: Producer b Identity r -> r
drain p = runIdentity $ run $ for p discard

msum :: FoldM Identity Int Int
msum = L.FoldM (\a b -> return $ a + b) (return 0) return

-- Using runIdentity seems to reduce outlier counts.
preludeBenchmarks :: Int -> [Benchmark]
preludeBenchmarks vmax =
    let applyBench b = b benchEnum_p
        benchEnum_p  = enumFromTo 1 vmax
    in
    [ 
      bgroup "Folds" $ map applyBench
        [
          bench "all"       . whnf (runIdentity . P.all (<= vmax))
        , bench "any"       . whnf (runIdentity . P.any (> vmax))
        , bench "find"      . whnf (runIdentity . P.find (== vmax))
        , bench "findIndex" . whnf (runIdentity . P.findIndex (== vmax))
        , bench "fold"      . whnf (runIdentity . P.fold L.sum)
        , bench "foldM"     . whnf (runIdentity . P.foldM msum)
        , bench "head"      . nf (runIdentity . P.head)
        , bench "index"     . nf (runIdentity . P.index (vmax-1))
        , bench "last"      . nf (runIdentity . P.last)
        , bench "length"    . whnf (runIdentity . P.length)
        , bench "null"      . whnf (runIdentity  . P.null)
        , bench "toList"    . nf P.toList
        ]
    , bgroup "Pipes" $ map applyBench
        [
          bench "chain"       . whnf (drain . (>-> P.chain return))
        , bench "drop"        . whnf (drain . (>-> P.drop vmax))
        , bench "dropWhile"   . whnf (drain . (>-> P.dropWhile (<= vmax)))
        , bench "filter"      . whnf (drain . (>-> P.filter even))
        , bench "findIndices" . whnf (drain . (>-> P.findIndices (<= vmax)))
        , bench "map"         . whnf (drain . (>-> P.map ((+) 1)))
        , bench "take"        . whnf (drain . (>-> P.take vmax))
        , bench "takeWhile"   . whnf (drain . (>-> P.takeWhile (<= vmax)))
        , bench "scan"        . whnf (drain . (>-> P.scan L.sum))
        , bench "scanM"       . whnf (drain . (>-> P.scanM msum))
        ]
    , bgroup "Zips" $ map applyBench
        [
          bench "zip"     . whnf (drain . P.zip benchEnum_p)
        , bench "zipWith" . whnf (drain . P.zipWith (+) benchEnum_p)
        ]
    , bgroup "enumFromTo.vs.each"
        [
          bench "enumFromTo" $ whnf (drain . enumFromTo 1) vmax
        , bench "each"       $ whnf (drain . each) [1..vmax]
        ]
    ]
