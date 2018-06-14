{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-deprecations  #-}

module Math.NumberTheory.GCDBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.GCD as A
import Prelude as P
import Numeric.Natural

averageGCD :: Integral a => (a -> a -> a) -> a -> a
averageGCD gcdF lim = sum [ gcdF x y | x <- [lim .. 2 * lim], y <- [lim .. x] ]

benchSuite :: Benchmark
benchSuite = bgroup "GCD"
  [ subSuite "large coprimes" 1073741823 100003
  , subSuite "powers of 2" (2^12) (2^19)
  , subSuite "power of 23" (23^3) (23^7)
  , bench "average prelude  Int"     $ nf (averageGCD P.gcd)       (2000 :: Int)
  , bench "average arithmoi Int"     $ nf (averageGCD A.binaryGCD) (2000 :: Int)
  , bench "average prelude  Word"    $ nf (averageGCD P.gcd)       (2000 :: Word)
  , bench "average arithmoi Word"    $ nf (averageGCD A.binaryGCD) (2000 :: Word)
  , bench "average prelude  Integer" $ nf (averageGCD P.gcd)       (2000 :: Integer)
  , bench "average arithmoi Integer" $ nf (averageGCD A.binaryGCD) (2000 :: Integer)
  , bench "average prelude  Natural" $ nf (averageGCD P.gcd)       (2000 :: Natural)
  , bench "average arithmoi Natural" $ nf (averageGCD A.binaryGCD) (2000 :: Natural)
  ]
  where subSuite :: String -> Int -> Int -> Benchmark
        subSuite name m n = bgroup name
          [ bench "Prelude.gcd" $ nf (P.gcd m) n
          , bench "binaryGCD" $ nf (A.binaryGCD m) n
          , bench "Prelude.coprime" $ nf (\t -> 1 == P.gcd m t) n
          , bench "coprime" $ nf (A.coprime m) n
          ]
