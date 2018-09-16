{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.GaussianIntegersBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.ArithmeticFunctions (tau)
import Math.NumberTheory.Quadratic.GaussianIntegers

benchFindPrime :: Integer -> Benchmark
benchFindPrime n = bench (show n) $ nf findPrime n

benchTau :: Integer -> Benchmark
benchTau n = bench (show n) $ nf (\m -> sum [tau (x :+ y) | x <- [1..m], y <- [0..m]] :: Word) n

benchSuite :: Benchmark
benchSuite = bgroup "Gaussian"
  [ bgroup "findPrime" $ map benchFindPrime [1000033, 10000121, 100000037, 1000000009, 10000000033, 100000000057, 1000000000061, 10000000000037]
  , bgroup "tau" $ map benchTau [10, 20, 40, 80]
  ]
