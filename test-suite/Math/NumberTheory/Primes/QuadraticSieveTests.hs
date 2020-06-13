{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

prop :: (Prime (Large Int), Prime (Large Int)) -> Bool
prop (unPrime -> Large p, unPrime -> Large q) = (p > 10 ^ 5) && (q > 10 ^ 5) && (p /= q)

quadraticRelation :: (Prime (Large Int), Prime (Large Int)) -> Bool
quadraticRelation (unPrime -> Large p, unPrime -> Large q) = foldr checkQuadratic True (testQuadraticSieve (toInteger (p * q)) 1000 2000)
    where
        checkQuadratic (x, y) acc = (x * x - y * y) `mod` toInteger (p * q) == 0 && acc

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [  QC.testProperty "Successful Factorisations" quadraticRelation
  ]
