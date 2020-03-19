{-# LANGUAGE RankNTypes #-}

module Math.NumberTheory.RecurrencesBench
  ( benchSuite
  ) where

import Gauge.Main

import Data.Euclidean (GcdDomain)
import Math.NumberTheory.Recurrences

benchTriangle :: String -> (forall a. (GcdDomain a, Integral a) => [[a]]) -> Int -> Benchmark
benchTriangle name triangle n = bgroup name
  [ benchAt (10 * n)  (1 * n)
  , benchAt (10 * n)  (2 * n)
  , benchAt (10 * n)  (5 * n)
  , benchAt (10 * n)  (9 * n)
  ]
  where
    benchAt i j = bench ("!! " ++ show i ++ " !! " ++ show j)
                $ nf (\(x, y) -> triangle !! x !! y :: Integer) (i, j)

benchPartition :: Int -> Benchmark
benchPartition n = bgroup "partition"
  [ benchAt n
  , benchAt (n * 10)
  , benchAt (n * 100)
  ]
  where
    benchAt m = bench ("!!" ++ show m) $  nf (\k -> partition !! k :: Integer) m

benchSuite :: Benchmark
benchSuite = bgroup "Recurrences"
  [ bgroup "Bilinear"
    [ benchTriangle "binomial"  binomial 1000
    , benchTriangle "stirling1" stirling1 100
    , benchTriangle "stirling2" stirling2 100
    , benchTriangle "eulerian1" eulerian1 100
    , benchTriangle "eulerian2" eulerian2 100
    ]
  , benchPartition 1000
  , bgroup "factorialFactors"
    [ bench "10000" $ nf factorialFactors 10000
    , bench "20000" $ nf factorialFactors 20000
    , bench "40000" $ nf factorialFactors 40000
    ]
  ]
