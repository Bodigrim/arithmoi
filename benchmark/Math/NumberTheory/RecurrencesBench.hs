{-# LANGUAGE RankNTypes #-}

module Math.NumberTheory.RecurrencesBench
  ( benchSuite
  ) where

import Test.Tasty.Bench

import Data.Euclidean (GcdDomain)
import Data.List.Infinite (Infinite(..))
import qualified Data.List.Infinite as Inf
import Math.NumberTheory.Recurrences

benchTriangle :: String -> (forall a. (GcdDomain a, Integral a) => Infinite [a]) -> Word -> Benchmark
benchTriangle name triangle n = bgroup name
  [ benchAt (10 * n)  (1 * fromIntegral n)
  , benchAt (10 * n)  (2 * fromIntegral n)
  , benchAt (10 * n)  (5 * fromIntegral n)
  , benchAt (10 * n)  (9 * fromIntegral n)
  ]
  where
    benchAt i j = bench ("!! " ++ show i ++ " !! " ++ show j)
                $ nf (\(x, y) -> triangle Inf.!! x !! y :: Integer) (i, j)

benchPartition :: Word -> Benchmark
benchPartition n = bgroup "partition"
  [ benchAt n
  , benchAt (n * 10)
  , benchAt (n * 100)
  ]
  where
    benchAt m = bench ("!!" ++ show m) $  nf (\k -> partition Inf.!! k :: Integer) m

benchSuite :: Benchmark
benchSuite = bgroup "Recurrences"
  [ bgroup "Bilinear"
    [ benchTriangle "binomial"  binomial  80
    , benchTriangle "stirling1" stirling1 75
    , benchTriangle "stirling2" stirling2 75
    , benchTriangle "eulerian1" eulerian1 50
    , benchTriangle "eulerian2" eulerian2 50
    ]
  , benchPartition 1000
  , bgroup "factorialFactors"
    [ bench "10000" $ nf factorialFactors 10000
    , bench "20000" $ nf factorialFactors 20000
    , bench "40000" $ nf factorialFactors 40000
    ]
  ]
