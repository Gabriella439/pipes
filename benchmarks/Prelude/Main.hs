module Main (main) where

import Benchmarks
import Criterion.Main (defaultMain)
import Prelude hiding (enumFromTo)

defaultMax :: Int
defaultMax = 1000000

main :: IO ()
main = defaultMain $ preludeBenchmarks defaultMax (enumFromTo 1 defaultMax)
