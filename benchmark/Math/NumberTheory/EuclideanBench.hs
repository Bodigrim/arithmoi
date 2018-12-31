{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.EuclideanBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.Euclidean

doBench :: Integral a => (a -> a -> (a, a, a)) -> a -> a
doBench func lim = sum [ let (a, b, c) = func x y in a + b + c | y <- [3, 5 .. lim], x <- [0..y] ]

benchSuite :: Benchmark
benchSuite = bgroup "Euclidean"
  [ bench "extendedGCD/Int"      $ nf (doBench extendedGCD :: Int -> Int)         1000
  , bench "extendedGCD/Word"     $ nf (doBench extendedGCD :: Word -> Word)       1000
  , bench "extendedGCD/Integer"  $ nf (doBench extendedGCD :: Integer -> Integer) 1000
  ]
