{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.PrimesBench
  ( benchSuite
  ) where

import Gauge.Main
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

-- | bases by Jim Sinclair, https://miller-rabin.appspot.com
fermatBases :: [Integer]
fermatBases = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]

isStrongFermat :: Integer -> Bool
isStrongFermat n = all (isStrongFermatPP n) fermatBases

isFermat :: Integer -> Bool
isFermat n = all (isFermatPP n) fermatBases

comparePrimalityTests :: Int -> Benchmark
comparePrimalityTests bits = bgroup ("primality" ++ show bits)
  [ bench "isPrime"          $ nf (map isPrime)           ns
  , bench "millerRabinV 0"   $ nf (map $ millerRabinV  0) ns
  , bench "millerRabinV 10"  $ nf (map $ millerRabinV 10) ns
  , bench "millerRabinV 50"  $ nf (map $ millerRabinV 50) ns
  , bench "isStrongFermatPP" $ nf (map isStrongFermat)    ns
  , bench "isFermatPP"       $ nf (map isFermat)          ns
  ]
  where
    ns = take bits [genInteger 0 bits ..]

compareFactorisation :: Int -> Benchmark
compareFactorisation bits =
  bench ("factorise" ++ show bits) $ nf (map factorise) ns
  where
    ns = take (bits `div` 10) [genInteger 0 bits ..]

benchSuite :: Benchmark
benchSuite = bgroup "Primes" $
  map comparePrimalityTests [50, 100, 200, 500, 1000, 2000]
  ++
  map compareFactorisation [50, 60, 70, 80, 90, 100]

