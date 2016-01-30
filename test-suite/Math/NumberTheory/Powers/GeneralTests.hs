-- |
-- Module:      Math.NumberTheory.Powers.GeneralTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.General
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.GeneralTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit
#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Powers.General
import Math.NumberTheory.TestUtils

-- | Check that 'integerRoot' @pow@ returns the largest integer @m@ with @m^pow <= n@.
integerRootProperty :: (Integral a, Integral b) => AnySign a -> Power b -> Bool
integerRootProperty (AnySign n) (Power pow) = (even pow && n < 0)
  || (toInteger root ^ pow <= toInteger n && toInteger n < toInteger (root + 1) ^ pow)
    where
      root = integerRoot pow n

-- | Check that the number 'isKthPower' iff its 'integerRoot' is exact.
isKthPowerProperty :: (Integral a, Integral b) => AnySign a -> Power b -> Bool
isKthPowerProperty (AnySign n) (Power pow) = (even pow && n < 0 && not t) || (n /= root ^ pow && not t) || (n == root ^ pow && t)
  where
    t = isKthPower pow n
    root = integerRoot pow n

-- | Check that 'exactRoot' returns an exact integer root
-- and is consistent with 'isKthPower'.
exactRootProperty :: (Integral a, Integral b) => AnySign a -> Power b -> Bool
exactRootProperty (AnySign n) (Power pow) = case exactRoot pow n of
  Nothing   -> not (isKthPower pow n)
  Just root -> isKthPower pow n && n == root ^ pow

-- | Check that 'isPerfectPower' is consistent with 'highestPower'.
isPerfectPowerProperty :: Integral a => AnySign a -> Bool
isPerfectPowerProperty (AnySign n) = (k > 1 && t) || (k == 1 && not t)
  where
    t = isPerfectPower n
    (_, k) = highestPower n

-- | Check that the first component of 'highestPower' is square-free.
highestPowerProperty :: Integral a => AnySign a -> Bool
highestPowerProperty (AnySign n) = (n `elem` [-1, 0, 1] && k == 3) || (b ^ k == n && b' == b && k' == 1)
  where
    (b, k) = highestPower n
    (b', k') = highestPower b

-- | Check that 'largePFPower' is consistent with documentation.
largePFPowerProperty :: Positive Integer -> Integer -> Bool
largePFPowerProperty (Positive bd) n = bd == 1 || b == 0 || d' /= 0 || n <= b * d * d || any (\p -> gcd n p > 1) [2..bd] || b ^ k == n
  where
    (b, k) = largePFPower bd n
    (d, d') = bd `quotRem` b

highestPowerSpecialCases :: [Assertion]
highestPowerSpecialCases =
  -- Freezes before d44a13b.
  [ a ( 1013582159576576
      , 1013582159576576
      , 1)
  -- Freezes before d44a13b.
  , a ( 1013582159576576 ^ 7
      , 1013582159576576
      , 7)

  , a ( -2 ^ 63 :: Int
      , -2 :: Int
      , 63)

  , a ( (2 ^ 63 - 1) ^ 21
      , 2 ^ 63 - 1
      , 21)

  , a ( 576116746989720969230211509779286598589421531472851155101032940901763389787901933902294777750323196846498573545522289802689311975294763847414975335235584
      , 576116746989720969230211509779286598589421531472851155101032940901763389787901933902294777750323196846498573545522289802689311975294763847414975335235584
      , 1)

  , a ( -340282366920938463500268095579187314689
      , -340282366920938463500268095579187314689
      , 1)

  , a ( 268398749 :: Int
      , 268398749 :: Int
      , 1)

  , a ( 118372752099 :: Int
      , 118372752099 :: Int
      , 1)

  , a ( 1409777209 :: Int
      , 37547 :: Int
      , 2)

  , a ( -6277101735386680764856636523970481806547819498980467802113
      , -18446744073709551617
      , 3)

  , a ( -18446744073709551619 ^ 5
      , -18446744073709551619
      , 5)
  ]
  where
    a (n, b, k) = assertEqual "highestPower" (b, k) (highestPower n)

testSuite :: TestTree
testSuite = testGroup "General"
  [ testIntegral2Property "integerRoot"    integerRootProperty
  , testIntegral2Property "isKthPower"     isKthPowerProperty
  , testIntegral2Property "exactRoot"      exactRootProperty
  , testIntegralProperty  "isPerfectPower" isPerfectPowerProperty
  , testGroup "highestPower"
    ( testIntegralProperty  "highestPower"   highestPowerProperty
    : zipWith (\i a -> testCase ("special case " ++ show i) a) [1..] highestPowerSpecialCases
    )
  , testSmallAndQuick     "largePFPower"   largePFPowerProperty
  ]
