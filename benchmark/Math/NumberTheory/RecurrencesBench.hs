{-# LANGUAGE RankNTypes #-}

module Math.NumberTheory.RecurrencesBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.Recurrencies (binomial, eulerian1, eulerian2,
                                       stirling1, stirling2, partition)

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

benchPartition :: Int -> Benchmark
benchPartition n = bgroup "partition"
  [ benchAt n
  , benchAt (n * 10)
  , benchAt (n * 100)
  ]
  where
    benchAt m = bench ("!!" ++ show m) $  nf (\k -> partition !! k :: Integer) m

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
    bgroup "Pentagonal"
    [ bgroup "Partition function"
      [ benchPartition 1000
      ]
    ]
  ]
