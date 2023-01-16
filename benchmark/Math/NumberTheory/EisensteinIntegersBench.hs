{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Math.NumberTheory.EisensteinIntegersBench
  ( benchSuite
  ) where

import Data.Maybe
import Test.Tasty.Bench

import Math.NumberTheory.ArithmeticFunctions (tau)
import Math.NumberTheory.Primes (isPrime)
import Math.NumberTheory.Quadratic.EisensteinIntegers

benchFindPrime :: Integer -> Benchmark
benchFindPrime n = bench (show n) $ nf findPrime (fromJust (isPrime n))

benchTau :: Integer -> Benchmark
benchTau n = bench (show n) $ nf (\m -> sum [tau (x :+ y) | x <- [1..m], y <- [0..m]] :: Word) n

benchSuite :: Benchmark
benchSuite = bgroup "Eisenstein"
  [ bgroup "findPrime" $ map benchFindPrime [1000003, 10000141, 100000039, 1000000021, 10000000033, 100000000003, 1000000000039, 10000000000051]
  , bgroup "tau" $ map benchTau [10, 20, 40, 80]
  ]
