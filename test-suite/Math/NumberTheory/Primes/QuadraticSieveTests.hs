{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils ()
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

quadraticRelation :: (Prime (Large Int), Prime (Large Int)) -> Bool
quadraticRelation (unPrime -> Large p, unPrime -> Large q)
  | p == q    = True
  | otherwise = all checkQuadratic (findPairs (toInteger p * toInteger q) 1000 2000)
    where
      checkQuadratic (x, y) = (x * x - y * y) `mod` (toInteger p * toInteger q) == 0

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Successful Factorisations" quadraticRelation
  ]
