{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module:      Math.NumberTheory.GaussianIntegersTests
-- Copyright:   (c) 2016 Chris Fredrickson, Google Inc.
-- Licence:     MIT
-- Maintainer:  Chris Fredrickson <chris.p.fredrickson@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.GaussianIntegers
--

module Math.NumberTheory.GaussianIntegersTests
  ( testSuite
  ) where

import Control.Monad (zipWithM_)
import Test.Tasty
import Test.Tasty.HUnit

import Math.NumberTheory.GaussianIntegers
import Math.NumberTheory.TestUtils

lazyCases :: [(GaussianInteger, [(GaussianInteger, Int)])]
lazyCases =
  [ ( 14145130733
    * 10000000000000000000000000000000000000121
    * 100000000000000000000000000000000000000000000000447
    , [(21037 :+ 117058, 1), (117058 :+ 21037, 1)]
    )
  ]

-- | Number is zero or is equal to the product of its factors.
factoriseProperty1 :: Integer -> Integer -> Bool
factoriseProperty1 x y
  =  x == 0 && y == 0
  || g == g'
  where
    g = x :+ y
    factors = factorise g
    g' = product $ map (uncurry (.^)) factors

factoriseProperty2 :: (GaussianInteger, [(GaussianInteger, Int)]) -> Assertion
factoriseProperty2 (n, fs) = zipWithM_ (assertEqual (show n)) fs (factorise n)

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
    nonUnitFactors = filter (\(p, _) -> norm p /= 1) factors
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

-- | a special case that tests rounding/truncating in GCD.
gcdGSpecialCase1 :: Assertion
gcdGSpecialCase1 = assertEqual "gcdG" 1 $ gcdG (12 :+ 23) (23 :+ 34)

testSuite :: TestTree
testSuite = testGroup "GaussianIntegers" $
  [ testSmallAndQuick "factorise"         factoriseProperty1
  , testSmallAndQuick "isPrime"           isPrimeProperty
  , testSmallAndQuick "primes"            primesGeneratesPrimesProperty
  , testSmallAndQuick "signumAbsProperty" signumAbsProperty
  , testSmallAndQuick "absProperty"       absProperty
  , testCase          "gcdG (12 :+ 23) (23 :+ 34)" gcdGSpecialCase1
  ]
  ++
  map (\x -> testCase ("laziness " ++ show (fst x)) (factoriseProperty2 x)) lazyCases
