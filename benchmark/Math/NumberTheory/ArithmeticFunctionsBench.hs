{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctionsBench
  ( benchSuite
  ) where

import Test.Tasty.Bench
import Data.Set (Set)

import Math.NumberTheory.ArithmeticFunctions as A

compareFunctions :: String -> (Integer -> Integer) -> [Integer] -> Benchmark
compareFunctions name new range = bench name $ nf (map new) range

compareSetFunctions :: String -> (Integer -> Set Integer) -> Benchmark
compareSetFunctions name new = bench name $ nf (map new) [1..100000]

benchSuite :: Benchmark
benchSuite = bgroup "ArithmeticFunctions"
  [ compareSetFunctions "divisors" A.divisors 
  , bench "divisors/int" $ nf (map A.divisorsSmall)              [1 :: Int .. 100000]
  , compareFunctions "totient" A.totient                         [1..100000]
  , compareFunctions "carmichael" A.carmichael                   [1..100000]
  , compareFunctions "moebius" (A.runMoebius . A.moebius)        [1..100000]
  , compareFunctions "tau" A.tau                                 [1..100000]
  , compareFunctions "sigma 1" (A.sigma 1)                       [1..100000]
  , compareFunctions "sigma 2" (A.sigma 2)                       [1..100000]
  , compareFunctions "ramanujan range" ramanujan                 [1..2000]
  , compareFunctions "ramanujan large prime" ramanujan           [100003]
  , compareFunctions "ramanujan prime power" ramanujan           [2^3000]
  ]
