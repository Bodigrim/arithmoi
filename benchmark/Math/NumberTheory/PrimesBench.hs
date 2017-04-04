{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Math.NumberTheory.PrimesBench
  ( benchSuite
  ) where

import Criterion.Main
import System.Random

import Math.NumberTheory.Logarithms (integerLog2)
import Math.NumberTheory.Primes

genInteger :: Int -> Int -> Integer
genInteger salt bits
    = head
    . dropWhile ((< bits) . integerLog2)
    . scanl (\a r -> a * 2^31 + abs r) 1
    . randoms
    . mkStdGen
    $ salt + bits

comparePrimalityTests :: Int -> Benchmark
comparePrimalityTests bits = bgroup ("primality" ++ show bits)
  [ bench "isPrime"         $ nf (map isPrime)           ns
  , bench "millerRabinV 0"  $ nf (map $ millerRabinV  0) ns
  , bench "millerRabinV 10" $ nf (map $ millerRabinV 10) ns
  , bench "millerRabinV 50" $ nf (map $ millerRabinV 50) ns
  ]
  where
    ns = take bits [genInteger 0 bits ..]

benchSuite = bgroup "Primes" $ map comparePrimalityTests [50, 100, 200, 500, 1000, 2000]
