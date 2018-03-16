{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.GCDBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.GCD as A
import Prelude as P

benchSuite :: Benchmark
benchSuite = bgroup "GCD"
  [ subSuite "large coprimes" 1073741823 100003
  , subSuite "powers of 2" (2^12) (2^19)
  , subSuite "power of 23" (23^3) (23^7)
  ]
  where subSuite :: String -> Int -> Int -> Benchmark
        subSuite name m n = bgroup name
          [ bench "Prelude.gcd" $ nf (P.gcd m) n
          , bench "binaryGCD" $ nf (A.binaryGCD m) n
          , bench "Prelude.coprime" $ nf (\t -> 1 == P.gcd m t) n
          , bench "coprime" $ nf (A.coprime m) n
          ]
