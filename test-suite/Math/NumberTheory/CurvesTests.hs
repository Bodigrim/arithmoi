-- |
-- Module:      Math.NumberTheory.CurvesTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Curves
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.CurvesTests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (Positive, NonNegative, generate, getNonNegative)

import GHC.TypeNats (KnownNat)

import Math.NumberTheory.Curves.Montgomery
import Math.NumberTheory.TestUtils

(==>?) :: Maybe a -> (a -> Property) -> Property
x ==>? f = case x of
  Nothing -> discard
  Just y  -> f y

isValid :: KnownNat n => Point a24 n -> Property
isValid p
  =    counterexample "x is not reduced by modulo"    (x >= 0 && x < n)
  .&&. counterexample "z is not reduced by modulo"    (z >= 0 && z < n)
  where
    n = pointN p
    x = pointX p
    z = pointZ p

isValid' :: KnownNat n => Point a24 n -> Bool
isValid' p
  =  (x >= 0 && x < n)
  && (z >= 0 && z < n)
  where
    n = pointN p
    x = pointX p
    z = pointZ p

newPointRangeProperty :: Shrink2 (Positive Integer) -> Shrink2 (Positive Integer) -> Property
newPointRangeProperty (Shrink2 (Positive s)) (Shrink2 (Positive n)) = newPoint s n ==>? \case
  SomePoint p -> isValid p

multiplyRangeProperty :: Shrink2 (Positive Integer) -> Shrink2 (Positive Integer) -> Shrink2 Word -> Property
multiplyRangeProperty (Shrink2 (Positive s)) (Shrink2 (Positive n)) (Shrink2 k) = newPoint s n ==>? \case
  SomePoint p -> isValid' p ==> isValid (multiply k p)

doubleRangeProperty :: Shrink2 (Positive Integer) -> Shrink2 (Positive Integer) -> Shrink2 Word -> Property
doubleRangeProperty (Shrink2 (Positive s)) (Shrink2 (Positive n)) (Shrink2 k) = newPoint s n ==>? \case
  SomePoint p -> isValid' p ==> isValid' kp ==> isValid (double kp)
    where
      kp = multiply k p

addRangeProperty :: Shrink2 (Positive Integer) -> Shrink2 (Positive Integer) -> Shrink2 Word -> Shrink2 Word -> Property
addRangeProperty (Shrink2 (Positive s)) (Shrink2 (Positive n)) (Shrink2 k) (Shrink2 l) = newPoint s n ==>? \case
  SomePoint p -> isValid' p ==> isValid' kp ==> isValid' lp ==> isValid' klp ==> isValid (add kp lp klp)
    where
      kp  = multiply  k      p
      lp  = multiply      l  p
      klp = multiply (k + l) p

doubleAndMultiplyProperty :: Shrink2 (Positive Integer) -> Shrink2 (Positive Integer) -> Shrink2 Word -> Property
doubleAndMultiplyProperty (Shrink2 (Positive s)) (Shrink2 (Positive n)) (Shrink2 k) = newPoint s n ==>? \case
  SomePoint p
    -> k < maxBound `div` 2 ==> double (multiply k p) === multiply (2 * k) p

addAndMultiplyProperty :: Shrink2 (Positive Integer) -> Shrink2 (Positive Integer) -> Shrink2 Word -> Shrink2 Word -> Property
addAndMultiplyProperty (Shrink2 (Positive s)) (Shrink2 (Positive n)) (Shrink2 k) (Shrink2 l) = newPoint s n ==>? \case
  SomePoint p
    -> k < maxBound `div` 3 && l < maxBound `div` 3 && pointX kp /= 0 && gcd n (pointZ kp) == 1 && gcd n (pointZ lp) == 1 && gcd n (pointZ klp) == 1
    ==> add kp lp klp === k2lp
    where
      kp   = multiply k           p
      lp   = multiply l           p
      klp  = multiply (k + l)     p
      k2lp = multiply (k + 2 * l) p

testSuite :: TestTree
testSuite = localOption (QuickCheckMaxRatio 100) $
  localOption (QuickCheckTests 1000) $ testGroup "Montgomery"
  [ QC.testProperty "range of newPoint"        newPointRangeProperty
  , QC.testProperty "range of double"          doubleRangeProperty
  , QC.testProperty "range of add"             addRangeProperty
  , QC.testProperty "range of multiply"        multiplyRangeProperty
  , QC.testProperty "double matches multiply"  doubleAndMultiplyProperty
  , QC.testProperty "add matches multiply"     addAndMultiplyProperty
  ]
