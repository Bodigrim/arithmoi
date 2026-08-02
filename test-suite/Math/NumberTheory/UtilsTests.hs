module Math.NumberTheory.UtilsTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit (testCase, assertBool)

import Data.Euclidean (WrappedIntegral(..))
import Data.Functor.Identity (Identity(..))

import Math.NumberTheory.TestUtils

import Math.NumberTheory.Utils (stripHighestPower, stripHighestPowerOf2)

stripHighestPowerProperty :: Integral a => NonZero a -> Identity a -> Bool
stripHighestPowerProperty (NonZero d) (Identity n) = n == d ^ e * rest && (abs rest <= abs n || n == abs n)
  where
    (e, WrapIntegral rest) = stripHighestPower (WrapIntegral d) (WrapIntegral n)

stripHighestPowerOf2Property :: Integral a => Identity a -> Bool
stripHighestPowerOf2Property (Identity n) = n == 2 ^ e * rest && (abs rest <= abs n || n == abs n)
  where
    (e, rest) = stripHighestPowerOf2 n

testSuite :: TestTree
testSuite = testGroup "Utils"
  [ testSameIntegralProperty "stripHighestPower" stripHighestPowerProperty
  , testIntegralProperty "stripHighestPowerOf2" stripHighestPowerOf2Property
  , testSmallAndQuick "stripHighestPower (minBound :: Int)" $
      \d -> stripHighestPowerProperty d (Identity (minBound :: Int))
  , testCase "stripHighestPowerOf2 (minBound :: Int)" $
      assertBool "should be true" (stripHighestPowerOf2Property (Identity (minBound :: Int)))
  ]
