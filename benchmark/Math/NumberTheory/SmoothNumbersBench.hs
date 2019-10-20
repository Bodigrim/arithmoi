{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SmoothNumbersBench
  ( benchSuite
  ) where

import Gauge.Main

import Math.NumberTheory.Primes
import Math.NumberTheory.SmoothNumbers

doBench :: Int -> Int
doBench lim = sum $ take lim $ smoothOver $ fromList $ map unPrime [nextPrime 2 .. precPrime lim]

benchSuite :: Benchmark
benchSuite = bgroup "SmoothNumbers"
  [ bench "100"      $ nf doBench   100
  , bench "1000"     $ nf doBench  1000
  , bench "10000"    $ nf doBench 10000
  ]
