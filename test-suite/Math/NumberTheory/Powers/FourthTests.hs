-- |
-- Module:      Math.NumberTheory.Powers.FourthTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.Fourth
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.FourthTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe
#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Powers.Fourth
import Math.NumberTheory.TestUtils

#include "MachDeps.h"

-- | Check that 'integerFourthRoot' returns the largest integer @m@ with @m^4 <= n@.
--
-- (m + 1) ^ 4 /= n && (m + 1) ^ 3 >= n `div` (m + 1)
-- means
-- (m + 1) ^ 4 > n
-- but without overflow for bounded types
integerFourthRootProperty :: Integral a => NonNegative a -> Bool
integerFourthRootProperty (NonNegative n) = m >= 0 && m ^ 4 <= n && (m + 1) ^ 4 /= n && (m + 1) ^ 3 >= n `div` (m + 1)
  where
    m = integerFourthRoot n

-- | Specialized to trigger 'biSqrtInt'.
integerFourthRootProperty_Int :: NonNegative Int -> Bool
integerFourthRootProperty_Int = integerFourthRootProperty

-- | Specialized to trigger 'biSqrtWord'.
integerFourthRootProperty_Word :: NonNegative Word -> Bool
integerFourthRootProperty_Word = integerFourthRootProperty

-- | Specialized to trigger 'biSqrtIgr'.
integerFourthRootProperty_Integer :: NonNegative Integer -> Bool
integerFourthRootProperty_Integer = integerFourthRootProperty

-- | Check that 'integerFourthRoot' returns the largest integer @m@ with @m^4 <= n@, , where @n@ has form @k@^4-1.
integerFourthRootProperty2 :: Integral a => NonNegative a -> Bool
integerFourthRootProperty2 (NonNegative k) = n < 0 || m >= 0 && m ^ 4 <= n && (m + 1) ^ 4 /= n && (m + 1) ^ 3 >= n `div` (m + 1)
  where
    n = k ^ 4 - 1
    m = integerFourthRoot n

-- | Specialized to trigger 'biSqrtInt.
integerFourthRootProperty2_Int :: NonNegative Int -> Bool
integerFourthRootProperty2_Int = integerFourthRootProperty2

-- | Specialized to trigger 'biSqrtWord'.
integerFourthRootProperty2_Word :: NonNegative Word -> Bool
integerFourthRootProperty2_Word = integerFourthRootProperty2

#if WORD_SIZE_IN_BITS == 64

-- | Check that 'integerFourthRoot' of 2^60-1 is 2^15-1, not 2^15.
integerFourthRootSpecialCase1_Int :: Assertion
integerFourthRootSpecialCase1_Int =
  assertEqual "integerFourthRoot" (integerFourthRoot (maxBound `div` 8 :: Int)) (2 ^ 15 - 1)

-- | Check that 'integerFourthRoot' of 2^60-1 is 2^15-1, not 2^15.
integerFourthRootSpecialCase1_Word :: Assertion
integerFourthRootSpecialCase1_Word =
  assertEqual "integerFourthRoot" (integerFourthRoot (maxBound `div` 16 :: Word)) (2 ^ 15 - 1)

-- | Check that 'integerFourthRoot' of 2^64-1 is 2^16-1, not 2^16.
integerFourthRootSpecialCase2 :: Assertion
integerFourthRootSpecialCase2 =
  assertEqual "integerFourthRoot" (integerFourthRoot (maxBound :: Word)) (2 ^ 16 - 1)

#endif

-- | Check that 'integerFourthRoot'' returns the largest integer @m@ with @m^4 <= n@.
integerFourthRoot'Property :: Integral a => NonNegative a -> Bool
integerFourthRoot'Property (NonNegative n) = m >= 0 && m ^ 4 <= n && (m + 1) ^ 4 /= n && (m + 1) ^ 3 >= n `div` (m + 1)
  where
    m = integerFourthRoot' n

-- | Check that the number 'isFourthPower' iff its 'integerFourthRoot' is exact.
isFourthPowerProperty :: Integral a => AnySign a -> Bool
isFourthPowerProperty (AnySign n) = (n < 0 && not t) || (n /= m ^ 4 && not t) || (n == m ^ 4 && t)
  where
    t = isFourthPower n
    m = integerFourthRoot n

-- | Check that the number 'isFourthPower'' iff its 'integerFourthRoot'' is exact.
isFourthPower'Property :: Integral a => NonNegative a -> Bool
isFourthPower'Property (NonNegative n) = (n /= m ^ 4 && not t) || (n == m ^ 4 && t)
  where
    t = isFourthPower' n
    m = integerFourthRoot' n

-- | Check that 'exactFourthRoot' returns an exact integer root of fourth power
-- and is consistent with 'isFourthPower'.
exactFourthRootProperty :: Integral a => AnySign a -> Bool
exactFourthRootProperty (AnySign n) = case exactFourthRoot n of
  Nothing -> not (isFourthPower n)
  Just m  -> isFourthPower n && n == m ^ 4

-- | Check that 'isPossibleFourthPower' is consistent with 'exactFourthRoot'.
isPossibleFourthPowerProperty :: Integral a => NonNegative a -> Bool
isPossibleFourthPowerProperty (NonNegative n) = t || not t && isNothing m
  where
    t = isPossibleFourthPower n
    m = exactFourthRoot n

testSuite :: TestTree
testSuite = testGroup "Fourth"
  [ testGroup "integerFourthRoot"
    [ testIntegralProperty "generic"         integerFourthRootProperty
    , testSmallAndQuick    "generic Int"     integerFourthRootProperty_Int
    , testSmallAndQuick    "generic Word"    integerFourthRootProperty_Word
    , testSmallAndQuick    "generic Integer" integerFourthRootProperty_Integer

    , testIntegralProperty "almost Fourth"      integerFourthRootProperty2
    , testSmallAndQuick    "almost Fourth Int"  integerFourthRootProperty2_Int
    , testSmallAndQuick    "almost Fourth Word" integerFourthRootProperty2_Word

    , testCase             "maxBound / 8 :: Int"   integerFourthRootSpecialCase1_Int
    , testCase             "maxBound / 16 :: Word" integerFourthRootSpecialCase1_Word
    , testCase             "maxBound :: Word"      integerFourthRootSpecialCase2
    ]
  , testIntegralProperty "integerFourthRoot'"    integerFourthRoot'Property
  , testIntegralProperty "isFourthPower"         isFourthPowerProperty
  , testIntegralProperty "isFourthPower'"        isFourthPower'Property
  , testIntegralProperty "exactFourthRoot"       exactFourthRootProperty
  , testIntegralProperty "isPossibleFourthPower" isPossibleFourthPowerProperty
  ]
