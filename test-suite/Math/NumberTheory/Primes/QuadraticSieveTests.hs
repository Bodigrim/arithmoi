-- |
-- Module:      Math.NumberTheory.Primes.QuadraticSieveTests
-- Copyright:   (c) 2020 Federico Bongiorno
-- Licence:     MIT
-- Maintainer:  Federico Bongiorno <federicobongiorno97@gmail.com>
--
-- Tests for Math.NumberTheory.Primes.Factorisation.QuadraticSieve
--

module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve
import qualified Debug.Trace

trace :: String -> a -> a
trace = if debug then Debug.Trace.trace else const id

-- This variable can be set to @True@ to print the integers being tested.
debug :: Bool
debug = False

-- This test checks whether the pair of integers @(x, y)@ produced in @findRoots@
-- satisfies @(x ^ 2 - y ^ 2) `mod` n = 0@. It is better to test for speed using
-- this routine.
checkSquares :: Large Int -> Large Int -> Bool
checkSquares (Large i) (Large j)
  | p == 2 || q == 2 || p == q = True
  | otherwise                  = (x * x - y * y) `mod` n == 0
  where
    (x, y) = trace ("Number: " ++ show n) $ head $ findRoots n $ autoConfig n
    n = p * q
    p = toInteger . unPrime . nextPrime $ i
    q = toInteger . unPrime . nextPrime $ j

-- This tests checks if @quadraticSieve@ can factorise small numbers (up to 5 digits).
checkSmallFactor :: Int -> Int -> Bool
checkSmallFactor i j
  | p == 2 || q == 2 || p == q = True
  | otherwise                  = n `mod` factor == 0
  where
    factor = trace ("Number: " ++ show n) $ quadraticSieve n
    n = p * q
    p = toInteger . unPrime . nextPrime $ i
    q = toInteger . unPrime . nextPrime $ j

-- This tests checks if @quadraticSieve@ can factorise larger numbers (up to 38 digits).
checkFactor :: Large Int -> Large Int -> Bool
checkFactor (Large i) (Large j)
  | p == 2 || q == 2 || p == q = True
  | otherwise                  = n `mod` factor == 0
  where
    factor = trace ("Number: " ++ show n) $ quadraticSieve n
    n = p * q
    p = toInteger . unPrime . nextPrime $ i
    q = toInteger . unPrime . nextPrime $ j

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Squares Property" checkSquares
  , testSmallAndQuick "Small Factorisations" checkSmallFactor
  , QC.testProperty "Factorisations" checkFactor
  ]
