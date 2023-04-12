{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.JacobiBench
  ( benchSuite
  ) where

import Test.Tasty.Bench
import Numeric.Natural

import Math.NumberTheory.Moduli.Sqrt

doBench :: Integral a => (a -> a -> JacobiSymbol) -> a -> a
doBench func lim = sum [ x + y | y <- [3, 5 .. lim], x <- [0..y], func x y == One ]

benchSuite :: Benchmark
benchSuite = bgroup "Jacobi"
  [ bench "jacobi/Int"      $ nf (doBench jacobi  :: Int -> Int)         2000
  , bench "jacobi/Word"     $ nf (doBench jacobi  :: Word -> Word)       2000
  , bench "jacobi/Integer"  $ nf (doBench jacobi  :: Integer -> Integer) 2000
  , bench "jacobi/Natural"  $ nf (doBench jacobi  :: Natural -> Natural) 2000
  ]
