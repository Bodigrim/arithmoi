{-# LANGUAGE DataKinds #-}
module Math.NumberTheory.DiscreteLogarithmBench 
  ( benchSuite
  ) where

import Gauge.Main
import Data.Maybe

-- import Math.NumberTheory.Moduli (Mod, discreteLogarithm, PrimitiveRoot, isPrimitiveRoot, MultMod, isMultElement)
import Math.NumberTheory.Moduli.Class (isMultElement)
import Math.NumberTheory.Moduli.PrimitiveRoot (PrimitiveRoot, isPrimitiveRoot)
import Math.NumberTheory.Moduli.DiscreteLogarithm (discreteLogarithm)

type Modulus = 1000000007

root :: PrimitiveRoot Modulus
root = fromJust $ isPrimitiveRoot 5

benchSuite :: Benchmark
benchSuite = bgroup "Discrete logarithm"
  [ bench "5^x = 8 mod 10^9+7" $ nf (uncurry discreteLogarithm) (root,fromJust $ isMultElement 8)
  ]
