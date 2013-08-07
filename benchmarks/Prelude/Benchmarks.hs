{-# LANGUAGE RankNTypes #-}
module Benchmarks (
      preludeBenchmarks
    , enumFromTo
    ) where

import Criterion.Main hiding (run)
import Control.Monad.Identity (Identity, runIdentity)
import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P
import Prelude hiding (enumFromTo)

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

forceMaybeId :: Identity (Maybe a) -> ()
forceMaybeId a =
  case runIdentity a of
    Just v  -> v `seq` ()
    Nothing -> ()

preludeBenchmarks :: Int -> Producer Int Identity () -> [Benchmark]
preludeBenchmarks vmax benchEnum_p =
    [ bgroup "Folds" $ map (\b -> b benchEnum_p) [
        bench "all"       . whnf (P.all (< vmax))
      , bench "any"       . whnf (P.any (> vmax))
      , bench "find"      . whnf (P.find (== vmax))
      , bench "findIndex" . whnf (P.findIndex (== vmax))
      , bench "foldl"     . whnf (P.foldl (+) 0)
      , bench "foldM"     . whnf (P.foldM (\x y -> return $! x + y) 0)
      , bench "head"      . whnf P.head
      , bench "index"     . whnf (forceMaybeId . P.index (vmax-1))
      , bench "last"      . whnf P.last
      , bench "null"      . whnf (runIdentity . P.null)
      , bench "toList"    . nf P.toList
      ]
    , bgroup "Unfolds" [
        bench "enum"       $ whnf (run . (discard <\\) . enumFromTo 1) vmax
      , bench "replicate"  $
            let rep = runIdentity . run . (discard <\\) . flip P.replicate (1::Int)
            in whnf rep vmax
      , bench "replicateM" $
            let repM = runIdentity . run . (discard <\\) . flip P.replicateM (return ())
            in whnf repM vmax
      , bench "yieldIf"    $ whnf (run . (P.yieldIf even ~> discard) <\\ ) benchEnum_p
      , bench "yieldAfter" $ whnf (run . (P.yieldAfter return ~> discard) <\\ ) benchEnum_p
      ]
    , bgroup "Zips" $ map (\b -> b benchEnum_p) [
        bench "zip"     . whnf (run . (discard <\\) . P.zip benchEnum_p)
      , bench "zipWith" . whnf (run . (discard <\\) . P.zipWith (+) benchEnum_p)
      ]
    ]
