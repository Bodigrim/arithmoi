-- |
-- Module:      Math.NumberTheory.Moduli.ChineseTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Moduli.Chinese
--

module Math.NumberTheory.Moduli.ChineseTests
  ( testSuite
  ) where

import Test.Tasty

import Math.NumberTheory.Moduli (chinese)
import Math.NumberTheory.TestUtils

chineseProperty :: Integer -> Positive Integer -> Integer -> Positive Integer -> Bool
chineseProperty n1 (Positive m1) n2 (Positive m2) = not compatible ||
  case chinese (n1, m1) (n2, m2) of
    Nothing -> not compatible
    Just (n, m) -> compatible && (n - n1) `rem` m1 == 0 && (n - n2) `rem` m2 == 0 && m == lcm m1 m2
  where
    g = gcd m1 m2
    compatible = (n1 - n2) `rem` g == 0

testSuite :: TestTree
testSuite = testSmallAndQuick "chinese" chineseProperty
