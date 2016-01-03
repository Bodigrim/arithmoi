-- |
-- Module:      Math.NumberTheory.Powers.GeneralTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.General
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.GeneralTests
  ( testSuite
  ) where

import Test.Tasty
import Test.SmallCheck.Series
import Test.Tasty.HUnit

import Data.Functor.Identity

import Math.NumberTheory.Powers.General
import Math.NumberTheory.Powers.Utils

integerRootProperty :: (Integral a, Integral b) => Identity a -> Power b -> Bool
integerRootProperty (Identity n) (Power pow) = (even pow && n < 0)
  || (toInteger root ^ pow <= toInteger n && toInteger n < toInteger (root + 1) ^ pow)
    where
      root = integerRoot pow n

isKthPowerProperty :: (Integral a, Integral b) => Identity a -> Power b -> Bool
isKthPowerProperty (Identity n) (Power pow) = (even pow && n < 0 && not t) || (n /= root ^ pow && not t) || (n == root ^ pow && t)
  where
    t = isKthPower pow n
    root = integerRoot pow n

exactRootProperty :: (Integral a, Integral b) => Identity a -> Power b -> Bool
exactRootProperty (Identity n) (Power pow) = case exactRoot pow n of
  Nothing   -> not (isKthPower pow n)
  Just root -> isKthPower pow n && n == root ^ pow

isPerfectPowerProperty :: Integral a => Identity a -> Bool
isPerfectPowerProperty (Identity n) = (k > 1 && t) || (k == 1 && not t)
  where
    t = isPerfectPower n
    (_, k) = highestPower n

highestPowerProperty :: Integral a => Identity a -> Bool
highestPowerProperty (Identity n) = (n `elem` [-1, 0, 1] && k == 3) || (b ^ k == n && b' == b && k' == 1)
  where
    (b, k) = highestPower n
    (b', k') = highestPower b

largePFPowerProperty :: Positive Integer -> Integer -> Bool
largePFPowerProperty (Positive bd) n = bd == 1 || b == 0 || d' /= 0 || n <= b * d * d || any (\p -> gcd n p > 1) [2..bd] || b ^ k == n
  where
    (b, k) = largePFPower bd n
    (d, d') = bd `quotRem` b

highestPowerSpecialCase1 :: Assertion
highestPowerSpecialCase1 =
  assertEqual "highestPower" (highestPower 1013582159576576) (1013582159576576, 1 :: Int)

highestPowerSpecialCase2 :: Assertion
highestPowerSpecialCase2 =
  assertEqual "highestPower" (highestPower $ 1013582159576576^7) (1013582159576576, 7 :: Int)

testSuite :: TestTree
testSuite = testGroup "General"
  [ testCase              "highestPower special case 1" highestPowerSpecialCase1
  , testCase              "highestPower special case 2" highestPowerSpecialCase2

  , testIntegral2Property "integerRoot"    integerRootProperty
  , testIntegral2Property "isKthPower"     isKthPowerProperty
  , testIntegral2Property "exactRoot"      exactRootProperty
  , testIntegralProperty  "isPerfectPower" isPerfectPowerProperty
  , testIntegralProperty  "highestPower"   highestPowerProperty
  , testSmallAndQuick     "largePFPower"   largePFPowerProperty
  ]
