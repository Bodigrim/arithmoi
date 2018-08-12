{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module:      Math.NumberTheory.EisensteinIntegersTests
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.EisensteinIntegers
--

module Math.NumberTheory.EisensteinIntegersTests
  ( testSuite
  ) where

import qualified Math.NumberTheory.EisensteinIntegers as E

import Test.Tasty                                     (TestTree, testGroup)

import Math.NumberTheory.TestUtils                    (testSmallAndQuick)

-- | Check that @signum@ and @abs@ satisfy @z == signum z * abs z@, where @z@ is
-- an @EisensteinInteger@.
signumAbsProperty :: E.EisensteinInteger -> Bool
signumAbsProperty z = z == signum z * abs z

-- | Check that @abs@ maps an @EisensteinInteger@ to its associate in first
-- sextant.
absProperty :: E.EisensteinInteger -> Bool
absProperty z = isOrigin || (inFirstSextant && isAssociate)
  where
    z'@(x' E.:+ y') = abs z
    isOrigin = z' == 0 && z == 0
    -- The First sextant includes the positive real axis, but not the origin
    -- or the line defined by the linear equation @y = (sqrt 3) * x@ in the
    -- Cartesian plane.
    inFirstSextant = x' > y' && y' >= 0
    isAssociate = z' `elem` map (\e -> z * (1 E.:+ 1) ^ e) [0 .. 5]

-- | Verify that `divModE` produces the right quotient and remainder.
divModProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
divModProperty1 x y = (y == 0) || (x `E.divE` y) * y + (x `E.modE` y) == x

-- | Verify that `divModE` produces a remainder smaller than the divisor with
-- regards to the Euclidean domain's function.
modProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
modProperty1 x y = (y == 0) || (E.norm $ x `E.modE` y) < (E.norm y)

testSuite :: TestTree
testSuite = testGroup "EisensteinIntegers" $
  [ testSmallAndQuick "forall z . z == signum z * abs z" signumAbsProperty
  , testSmallAndQuick "abs z always returns an @EisensteinInteger@ in the\
                      \ first sextant of the complex plane" absProperty
  , testSmallAndQuick "divModE works properly" divModProperty1
  , testSmallAndQuick "The remainder's norm is smaller than the divisor's"
                      modProperty1
  ]
