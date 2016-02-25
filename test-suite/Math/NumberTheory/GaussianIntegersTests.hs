{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
    g' = product $ map (uncurry (.^)) factors

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
primesGeneratesPrimesProperty :: NonNegative Int -> Bool
primesGeneratesPrimesProperty (NonNegative i) = isPrime (primes !! i)

-- | signum and abs should satisfy: z == signum z * abs z
signumAbsProperty :: Integer -> Integer -> Bool
signumAbsProperty x y = z == signum z * abs z
  where
    z = x :+ y

-- | abs maps a Gaussian integer to its associate in first quadrant.
absProperty :: Integer -> Integer -> Bool
absProperty x y = inFirstQuadrant && isAssociate
  where
    z = x :+ y
    z'@(x' :+ y') = abs z
    inFirstQuadrant = x' >= 0 && y' >= 0
    isAssociate = z' `elem` map (\e -> z * (0 :+ 1) .^ e) [0 .. 3]

testSuite :: TestTree
testSuite = testGroup "GaussianIntegers"
  [ testSmallAndQuick "factorise"         factoriseProperty
  , testSmallAndQuick "isPrime"           isPrimeProperty
  , testSmallAndQuick "primes"            primesGeneratesPrimesProperty
  , testSmallAndQuick "signumAbsProperty" signumAbsProperty
  , testSmallAndQuick "absProperty"       absProperty
  ]
