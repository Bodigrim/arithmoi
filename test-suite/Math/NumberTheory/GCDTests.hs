-- |
-- Module:      Math.NumberTheory.GCDTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.GCD
--

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.GCDTests
  ( testSuite
  ) where

import Test.Tasty

import Data.Bits

import Math.NumberTheory.GCD
import Math.NumberTheory.TestUtils

-- | Check that 'binaryGCD' matches 'gcd'.
binaryGCDProperty :: (Integral a, Bits a) => AnySign a -> AnySign a -> Bool
binaryGCDProperty (AnySign a) (AnySign b) = binaryGCD a b == gcd a b

-- | Check that 'extendedGCD' is consistent with documentation.
extendedGCDProperty :: forall a. Integral a => AnySign a -> AnySign a -> Bool
extendedGCDProperty (AnySign a) (AnySign b) =
  u * a + v * b == d
  && d == gcd a b
  -- (-1) >= 0 is true for unsigned types
  && (abs u < abs b || abs b <= 1 || (-1 :: a) >= 0)
  && (abs v < abs a || abs a <= 1 || (-1 :: a) >= 0)
  where
    (d, u, v) = extendedGCD a b

-- | Check that numbers are coprime iff their gcd equals to 1.
coprimeProperty :: (Integral a, Bits a) => AnySign a -> AnySign a -> Bool
coprimeProperty (AnySign a) (AnySign b) = coprime a b == (gcd a b == 1)

testSuite :: TestTree
testSuite = testGroup "GCD"
  [ testSameIntegralProperty "binaryGCD"   binaryGCDProperty
  , testSameIntegralProperty "extendedGCD" extendedGCDProperty
  , testSameIntegralProperty "coprime"     coprimeProperty
  ]
