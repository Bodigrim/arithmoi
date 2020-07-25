{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils ()
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve
import Debug.Trace

quadraticRelation :: (Prime (Large Int), Prime (Large Int)) -> Bool
quadraticRelation (unPrime -> Large p, unPrime -> Large q)
  | p == q    = True
  | otherwise = trace ("Primes: " ++ show (p, q)) $ checkQuadratic (findSquares (toInteger p * toInteger q) b (2*b))
    where
      checkQuadratic (x, y) = (x * x - y * y) `mod` n == 0
      b = max 1000 $ floor l
      l = exp . sqrt $ (log (fromInteger n)) * (log (log (fromInteger n))) :: Double 
      n = toInteger p * toInteger q

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Successful Factorisations" quadraticRelation
  ]
