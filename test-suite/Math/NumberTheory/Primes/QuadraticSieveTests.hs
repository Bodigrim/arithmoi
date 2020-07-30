{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils ()
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

findFactor :: Large Int -> Large Int -> Bool
findFactor (Large i) (Large j)
  | p == 2 || q == 2 || p == q = True
  | n < 100000                 = True
  | otherwise                  = n `mod` factor == 0
    where
      factor = quadraticSieve n b b
      b = max 20 $ floor l
      l = exp . sqrt $ log (fromInteger n) * log (log (fromInteger n)) :: Double
      n = toInteger p * toInteger q
      p = unPrime . nextPrime $ i `mod` 100000000
      q = unPrime . nextPrime $ j `mod` 100000000

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Successful Factorisations" findFactor
  ]
