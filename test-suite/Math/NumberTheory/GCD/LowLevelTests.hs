-- |
-- Module:      Math.NumberTheory.GCD.LowLevelTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.GCD.LowLevel
--

{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.GCD.LowLevelTests
  ( testSuite
  ) where

import Test.Tasty

import GHC.Exts

import Math.NumberTheory.GCD.LowLevel
import Math.NumberTheory.TestUtils

-- | Check that 'gcdInt' matches 'gcd'.
gcdIntProperty :: Int -> Int -> Bool
gcdIntProperty a b = gcdInt a b == gcd a b

-- | Check that 'gcdWord' matches 'gcd'.
gcdWordProperty :: Word -> Word -> Bool
gcdWordProperty a b = gcdWord a b == gcd a b

-- | Check that 'gcdInt#' matches 'gcd'.
gcdIntProperty# :: Int -> Int -> Bool
gcdIntProperty# a@(I# a') b@(I# b') = I# (gcdInt# a' b') == gcd a b

-- | Check that 'gcdWord#' matches 'gcd'.
gcdWordProperty# :: Word -> Word -> Bool
gcdWordProperty# a@(W# a') b@(W# b') = W# (gcdWord# a' b') == gcd a b

-- | Check that numbers are coprime iff their gcd equals to 1.
coprimeIntProperty :: Int -> Int -> Bool
coprimeIntProperty a b = coprimeInt a b == (gcd a b == 1)

-- | Check that numbers are coprime iff their gcd equals to 1.
coprimeWordProperty :: Word -> Word -> Bool
coprimeWordProperty a b = coprimeWord a b == (gcd a b == 1)

-- | Check that numbers are coprime iff their gcd equals to 1.
coprimeIntProperty# :: Int -> Int -> Bool
coprimeIntProperty# a@(I# a') b@(I# b') = coprimeInt# a' b' == (gcd a b == 1)

-- | Check that numbers are coprime iff their gcd equals to 1.
coprimeWordProperty# :: Word -> Word -> Bool
coprimeWordProperty# a@(W# a') b@(W# b') = coprimeWord# a' b' == (gcd a b == 1)

testSuite :: TestTree
testSuite = testGroup "LowLevel"
  [ testSmallAndQuick "gcdInt"       gcdIntProperty
  , testSmallAndQuick "gcdWord"      gcdWordProperty
  , testSmallAndQuick "gcdInt#"      gcdIntProperty#
  , testSmallAndQuick "gcdWord#"     gcdWordProperty#
  , testSmallAndQuick "coprimeInt"   coprimeIntProperty
  , testSmallAndQuick "coprimeWord"  coprimeWordProperty
  , testSmallAndQuick "coprimeInt#"  coprimeIntProperty#
  , testSmallAndQuick "coprimeWord#" coprimeWordProperty#
  ]
