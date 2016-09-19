module Math.NumberTheory.RecurrenciesBench
  ( benchSuite
  ) where

import Criterion.Main
import Numeric.Natural
import System.Random

import Math.NumberTheory.Recurrencies.Bilinear

benchSuite = bgroup "Bilinears"
  [ bench "binomial"  $ nf (\n -> sum (take n $ map sum binomial) :: Integer) 100
  , bench "stirling1" $ nf (\n -> sum (take n $ map sum stirling1) :: Integer) 100
  , bench "stirling2" $ nf (\n -> sum (take n $ map sum stirling2) :: Integer) 100
  , bench "eulerian1" $ nf (\n -> sum (take n $ map sum eulerian1) :: Integer) 100
  , bench "eulerian2" $ nf (\n -> sum (take n $ map sum eulerian2) :: Integer) 100
  ]
