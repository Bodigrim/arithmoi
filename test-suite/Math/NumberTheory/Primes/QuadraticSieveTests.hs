{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils ()
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

findFactor :: (Prime (Large Int), Prime (Large Int)) -> Bool
findFactor (unPrime -> Large p, unPrime -> Large q)
  | p == q    = True
  | otherwise = n `mod` factor == 0
    where
      factor = quadraticSieve n b (2*b)
      b = max 1000 $ floor l
      l = exp . sqrt $ log (fromInteger n) * log (log (fromInteger n)) :: Double
      n = toInteger p * toInteger q

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Successful Factorisations" findFactor
  ]
