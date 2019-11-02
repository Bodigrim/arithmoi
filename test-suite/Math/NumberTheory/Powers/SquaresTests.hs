-- |
-- Module:      Math.NumberTheory.Powers.SquaresTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Powers.Squares
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.SquaresTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

import Math.NumberTheory.Roots
import Math.NumberTheory.TestUtils

#include "MachDeps.h"

-- | Check that 'integerSquareRoot' returns the largest integer @m@ with @m*m <= n@.
--
-- (m + 1) ^ 2 /= n && m + 1 >= n `div` (m + 1)
-- means
-- (m + 1) ^ 2 > n
-- but without overflow for bounded types
integerSquareRootProperty :: Integral a => NonNegative a -> Bool
integerSquareRootProperty (NonNegative n) = m >=0 && m * m <= n && (m + 1) ^ 2 /= n && m + 1 >= n `div` (m + 1)
  where
    m = integerSquareRoot n

-- | Specialized to trigger 'isqrtInt''.
integerSquareRootProperty_Int :: NonNegative Int -> Bool
integerSquareRootProperty_Int = integerSquareRootProperty

-- | Specialized to trigger 'isqrtWord'.
integerSquareRootProperty_Word :: NonNegative Word -> Bool
integerSquareRootProperty_Word = integerSquareRootProperty

-- | Specialized to trigger 'isqrtInteger'.
integerSquareRootProperty_Integer :: NonNegative Integer -> Bool
integerSquareRootProperty_Integer = integerSquareRootProperty

-- | Check that 'integerSquareRoot' returns the largest integer @m@ with @m*m <= n@, where @n@ has form @k@^2-1.
integerSquareRootProperty2 :: Integral a => Positive a -> Bool
integerSquareRootProperty2 (Positive k) = n < 0
  || m >=0 && m * m <= n && (m + 1) ^ 2 /= n && m + 1 >= n `div` (m + 1)
  where
    n = k ^ 2 - 1
    m = integerSquareRoot n

-- | Specialized to trigger 'isqrtInt''.
integerSquareRootProperty2_Int :: Positive Int -> Bool
integerSquareRootProperty2_Int = integerSquareRootProperty2

-- | Specialized to trigger 'isqrtWord'.
integerSquareRootProperty2_Word :: Positive Word -> Bool
integerSquareRootProperty2_Word = integerSquareRootProperty2

-- | Specialized to trigger 'isqrtInteger'.
integerSquareRootProperty2_Integer :: Positive Integer -> Bool
integerSquareRootProperty2_Integer = integerSquareRootProperty2

#if WORD_SIZE_IN_BITS == 64

-- | Check that 'integerSquareRoot' of 2^62-1 is 2^31-1, not 2^31.
integerSquareRootSpecialCase1_Int :: Assertion
integerSquareRootSpecialCase1_Int =
  assertEqual "integerSquareRoot" (integerSquareRoot (maxBound `div` 2 :: Int)) (2 ^ 31 - 1)

-- | Check that 'integerSquareRoot' of 2^62-1 is 2^31-1, not 2^31.
integerSquareRootSpecialCase1_Word :: Assertion
integerSquareRootSpecialCase1_Word =
  assertEqual "integerSquareRoot" (integerSquareRoot (maxBound `div` 4 :: Word)) (2 ^ 31 - 1)

-- | Check that 'integerSquareRoot' of 2^64-1 is 2^32-1, not 2^32.
integerSquareRootSpecialCase2 :: Assertion
integerSquareRootSpecialCase2 =
  assertEqual "integerSquareRoot" (integerSquareRoot (maxBound :: Word)) (2 ^ 32 - 1)

#endif

-- | Check that the number 'isSquare' iff its 'integerSquareRoot' is exact.
isSquareProperty :: Integral a => AnySign a -> Bool
isSquareProperty (AnySign n) = (n < 0 && not t) || (n /= m * m && not t) || (n == m * m && t)
  where
    t = isSquare n
    m = integerSquareRoot n

-- | Check that 'exactSquareRoot' returns an exact integer square root
-- and is consistent with 'isSquare'.
exactSquareRootProperty :: Integral a => AnySign a -> Bool
exactSquareRootProperty (AnySign n) = case exactSquareRoot n of
  Nothing -> not (isSquare n)
  Just m  -> isSquare n && n == m * m

testSuite :: TestTree
testSuite = testGroup "Squares"
  [ testGroup "integerSquareRoot"
    [ testIntegralProperty "generic"          integerSquareRootProperty
    , testSmallAndQuick    "generic Int"      integerSquareRootProperty_Int
    , testSmallAndQuick    "generic Word"     integerSquareRootProperty_Word
    , testSmallAndQuick    "generic Integer"  integerSquareRootProperty_Integer

    , testIntegralProperty "almost square"         integerSquareRootProperty2
    , testSmallAndQuick    "almost square Int"     integerSquareRootProperty2_Int
    , testSmallAndQuick    "almost square Word"    integerSquareRootProperty2_Word
    , testSmallAndQuick    "almost square Integer" integerSquareRootProperty2_Integer

#if WORD_SIZE_IN_BITS == 64
    , testCase             "maxBound / 2 :: Int"  integerSquareRootSpecialCase1_Int
    , testCase             "maxBound / 4 :: Word" integerSquareRootSpecialCase1_Word
    , testCase             "maxBound :: Word"     integerSquareRootSpecialCase2
#endif
    ]

  , testIntegralProperty "isSquare"           isSquareProperty
  , testIntegralProperty "exactSquareRoot"    exactSquareRootProperty
  ]
