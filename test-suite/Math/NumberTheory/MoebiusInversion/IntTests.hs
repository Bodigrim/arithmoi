-- |
-- Module:      Math.NumberTheory.MoebiusInversion.IntTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.MoebiusInversion.Int
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.MoebiusInversion.IntTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding (Positive)

import Math.NumberTheory.MoebiusInversion.Int
import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.TestUtils

totientSumProperty :: Positive Int -> Bool
totientSumProperty (Positive n) = toInteger (totientSum n) == sum (map totient [1 .. toInteger n])

totientSumSpecialCase1 :: Assertion
totientSumSpecialCase1 = assertEqual "totientSum" 4496 (totientSum 121)

totientSumSpecialCase2 :: Assertion
totientSumSpecialCase2 = assertEqual "totientSum" 0 (totientSum (-9001))

totientSumZero :: Assertion
totientSumZero = assertEqual "totientSum" 0 (totientSum 0)

generalInversionProperty :: (Int -> Int) -> Positive Int -> Bool
generalInversionProperty g (Positive n)
  =  g n == sum [f (n `quot` k) | k <- [1 .. n]]
  && f n == sum [fromInteger (moebius (toInteger k)) * g (n `quot` k) | k <- [1 .. n]]
  where
    f = generalInversion g

testSuite :: TestTree
testSuite = testGroup "Int"
  [ testGroup "totientSum"
    [ testSmallAndQuick "matches definitions" totientSumProperty
    , testCase          "special case 1"      totientSumSpecialCase1
    , testCase          "special case 2"      totientSumSpecialCase2
    , testCase          "zero"                totientSumZero
    ]
  , QC.testProperty "generalInversion" generalInversionProperty
  ]
