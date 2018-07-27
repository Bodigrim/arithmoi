{-# LANGUAGE RankNTypes #-}

module Math.NumberTheory.RecurrenciesBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.Recurrencies          (partition)
import Math.NumberTheory.Recurrencies.Bilinear

benchTriangle :: String -> (forall a. (Integral a) => [[a]]) -> Int -> Benchmark
benchTriangle name triangle n = bgroup name
  [ benchAt (10 * n)  (1 * n)
  , benchAt (10 * n)  (2 * n)
  , benchAt (10 * n)  (5 * n)
  , benchAt (10 * n)  (9 * n)
  ]
  where
    benchAt i j = bench ("!! " ++ show i ++ " !! " ++ show j)
                $ nf (\(x, y) -> triangle !! x !! y :: Integer) (i, j)

benchSuite :: Benchmark
benchSuite = bgroup "Recurrencies"
  [
    bgroup "Bilinear"
    [ benchTriangle "binomial"  binomial 1000
    , benchTriangle "stirling1" stirling1 100
    , benchTriangle "stirling2" stirling2 100
    , benchTriangle "eulerian1" eulerian1 100
    , benchTriangle "eulerian2" eulerian2 100
    ]
    ,
    bgroup "Partition"
    [ bench "partition" (nf partition' 100)
    ]
  ]

partition' :: Int -> Int
partition' n = partition !! n