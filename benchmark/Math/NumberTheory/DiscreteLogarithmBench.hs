{-# LANGUAGE DataKinds #-}
module Math.NumberTheory.DiscreteLogarithmBench 
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.Moduli (Mod, discreteLogarithm)

type Modulus = 10000007

discreteLogarithm' :: Mod Modulus -> Mod Modulus -> Maybe Integer
discreteLogarithm' = discreteLogarithm

benchSuite :: Benchmark
benchSuite = bgroup "Discrete logarithm"
  [ bench "(10^8+7, 5, 326448)" 
      $ nf (uncurry discreteLogarithm') (6, 326448)
  ]
