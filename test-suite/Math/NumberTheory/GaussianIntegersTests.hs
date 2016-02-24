module Math.NumberTheory.GaussianIntegersTests
  ( testSuite
  ) where

import Test.Tasty

import Math.NumberTheory.GaussianIntegers
import Math.NumberTheory.TestUtils

-- | Number is equal to the product of its factors.
factoriseProperty :: Integer -> Integer -> Bool
factoriseProperty x y = g == g'
  where
    g = x :+ y
    factors = factorise g
    g' = foldl (.*) (1 :+ 0) $ map (uncurry (.^)) factors

-- | Number is prime iff it is non-zero
--   and has exactly one (non-unit) factor.
isPrimeProperty :: Integer -> Integer -> Bool
isPrimeProperty x y
  =  x == 0 && y == 0
  || isPrime g && n == 1
  || not (isPrime g) && n /= 1
  where
    g = x :+ y
    factors = factorise g
    nonUnitFactors = filter (\(p, _) -> magnitude p /= 1) factors
    -- Count factors taking into account multiplicity
    n = sum $ map snd nonUnitFactors

-- | The list of primes should include only primes.
primesGeneratesPrimesProperty :: Int -> Bool
primesGeneratesPrimesProperty i = i < 0 || isPrime (primes !! i)

testSuite :: TestTree
testSuite = testGroup "GaussianIntegers"
  [ testSmallAndQuick "factorise"   factoriseProperty
  , testSmallAndQuick "isPrime"     isPrimeProperty
  , testSmallAndQuick "primes"      primesGeneratesPrimesProperty
  ]
