module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils ()
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

checkQuadratic :: Large Int -> Large Int -> Bool
checkQuadratic (Large i) (Large j)
  -- Quadratic Sieve does not work in these cases.
  | p == 2 || q == 2 || p == q = True
  -- The value of @b@ is too low for sieving to be successful.
  | n < 100000                 = True
  | otherwise                  = (firstSquare ^ (2 :: Int) - secondSquare ^ (2 :: Int)) `mod` n == 0
    where
      (firstSquare, secondSquare) = head $ findSquares n b b
      b = floor l
      l = exp . sqrt $ log (fromInteger n) * log (log (fromInteger n)) :: Double
      n = toInteger p * toInteger q
      p = unPrime . nextPrime $ i `mod` 100000000
      q = unPrime . nextPrime $ j `mod` 100000000

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Successful Factorisations" checkQuadratic
  ]
