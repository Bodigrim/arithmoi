{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module:      Math.NumberTheory.GaussianIntegersTests
-- Copyright:   (c) 2016 Chris Fredrickson
-- Licence:     MIT
-- Maintainer:  Chris Fredrickson <chris.p.fredrickson@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.GaussianIntegers
--

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
absProperty x y = isOrigin || (inFirstQuadrant && isAssociate)
  where
    z = x :+ y
    z'@(x' :+ y') = abs z
    isOrigin = z' == 0 && z == 0
    inFirstQuadrant = x' > 0 && y' >= 0     -- first quadrant includes the positive real axis, but not the origin or the positive imaginary axis
    isAssociate = z' `elem` map (\e -> z * (0 :+ 1) .^ e) [0 .. 3]

testSuite :: TestTree
testSuite = testGroup "GaussianIntegers"
  [ testSmallAndQuick "factorise"         factoriseProperty
  , testSmallAndQuick "isPrime"           isPrimeProperty
  , testSmallAndQuick "primes"            primesGeneratesPrimesProperty
  , testSmallAndQuick "signumAbsProperty" signumAbsProperty
  , testSmallAndQuick "absProperty"       absProperty
  ]
