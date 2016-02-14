-- |
-- Module:      Math.NumberTheory.Powers.IntegerTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.Integer
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.IntegerTests
  ( testSuite
  ) where

import Test.Tasty

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Powers.Integer
import Math.NumberTheory.TestUtils

-- | Check that 'integerPower' == '^'.
integerPowerProperty :: Integer -> Power Int -> Bool
integerPowerProperty a (Power b) = integerPower a b == a ^ b

-- | Check that 'integerWordPower' == '^'.
integerWordPowerProperty :: Integer -> Power Word -> Bool
integerWordPowerProperty a (Power b) = integerWordPower a b == a ^ b

testSuite :: TestTree
testSuite = testGroup "Integer"
  [ testSmallAndQuick "integerPower"     integerPowerProperty
  , testSmallAndQuick "integerWordPower" integerWordPowerProperty
  ]
