-- |
-- Module:      Math.NumberTheory.Powers.FourthTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.Fourth
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.FourthTests
  ( testSuite
  ) where

import Test.Tasty
import Test.SmallCheck.Series

import Data.Maybe

import Math.NumberTheory.Powers.Fourth
import Math.NumberTheory.Powers.Utils

-- (m + 1) ^ 4 /= n && (m + 1) ^ 3 >= n `div` (m + 1)
-- means
-- (m + 1) ^ 4 > n
-- but without overflow for bounded types
integerFourthRootProperty :: Integral a => NonNegative a -> Bool
integerFourthRootProperty (NonNegative n) = m >= 0 && m ^ 4 <= n && (m + 1) ^ 4 /= n && (m + 1) ^ 3 >= n `div` (m + 1)
  where
    m = integerFourthRoot n

integerFourthRoot'Property :: Integral a => NonNegative a -> Bool
integerFourthRoot'Property (NonNegative n) = m >= 0 && m ^ 4 <= n && (m + 1) ^ 4 /= n && (m + 1) ^ 3 >= n `div` (m + 1)
  where
    m = integerFourthRoot' n

isFourthPowerProperty :: Integral a => AnySign a -> Bool
isFourthPowerProperty (AnySign n) = (n < 0 && not t) || (n /= m ^ 4 && not t) || (n == m ^ 4 && t)
  where
    t = isFourthPower n
    m = integerFourthRoot n

isFourthPower'Property :: Integral a => NonNegative a -> Bool
isFourthPower'Property (NonNegative n) = (n /= m ^ 4 && not t) || (n == m ^ 4 && t)
  where
    t = isFourthPower' n
    m = integerFourthRoot' n

exactFourthRootProperty :: Integral a => AnySign a -> Bool
exactFourthRootProperty (AnySign n) = case exactFourthRoot n of
  Nothing -> not (isFourthPower n)
  Just m  -> isFourthPower n && n == m ^ 4

isPossibleFourthPowerProperty :: Integral a => NonNegative a -> Bool
isPossibleFourthPowerProperty (NonNegative n) = t || not t && isNothing m
  where
    t = isPossibleFourthPower n
    m = exactFourthRoot n

testSuite :: TestTree
testSuite = testGroup "Fourth"
  [ testIntegralProperty "integerFourthRoot"     integerFourthRootProperty
  , testIntegralProperty "integerFourthRoot'"    integerFourthRoot'Property
  , testIntegralProperty "isFourthPower"         isFourthPowerProperty
  , testIntegralProperty "isFourthPower'"        isFourthPower'Property
  , testIntegralProperty "exactFourthRoot"       exactFourthRootProperty
  , testIntegralProperty "isPossibleFourthPower" isPossibleFourthPowerProperty
  ]
