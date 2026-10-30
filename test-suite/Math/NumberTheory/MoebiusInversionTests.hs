-- |
-- Module:      Math.NumberTheory.MoebiusInversionTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.MoebiusInversion
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.MoebiusInversionTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding (Positive)

import Data.Proxy
import Data.Vector.Unboxed (Vector)

import Math.NumberTheory.MoebiusInversion
import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.TestUtils

proxy :: Proxy Vector
proxy = Proxy

totientSumProperty :: AnySign Word -> Bool
totientSumProperty (AnySign n) = (totientSum proxy n :: Word) == sum (map totient [1..n])

totientSumSpecialCase1 :: Assertion
totientSumSpecialCase1 = assertEqual "totientSum" 4496 (totientSum proxy 121 :: Word)

totientSumZero :: Assertion
totientSumZero = assertEqual "totientSum" 0 (totientSum proxy 0 :: Word)

generalInversionProperty :: (Word -> Word) -> Positive Word -> Bool
generalInversionProperty g (Positive n)
  =  g n == sum [f (n `quot` k) | k <- [1 .. n]]
  && f n == sum [runMoebius (moebius k) * g (n `quot` k) | k <- [1 .. n]]
  where
    f = generalInversion proxy g

testSuite :: TestTree
testSuite = testGroup "MoebiusInversion"
  [ testGroup "totientSum"
    [ testSmallAndQuick "matches definitions" totientSumProperty
    , testCase          "special case 1"      totientSumSpecialCase1
    , testCase          "zero"                totientSumZero
    ]
  , QC.testProperty "generalInversion" generalInversionProperty
  ]
