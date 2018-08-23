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
import Math.NumberTheory.Primes                       (primes)
import Test.Tasty                                     (TestTree, testGroup)
import Test.Tasty.HUnit                               (Assertion, assertEqual,
                                                      testCase)

import Math.NumberTheory.TestUtils                    (Positive (..),
                                                       testSmallAndQuick)

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

-- | Verify that @divE@ and @modE@ are what `divModE` produces.
divModProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
divModProperty1 x y = y == 0 || (q == q' && r == r')
  where
    (q, r) = E.divModE x y
    q'     = E.divE x y
    r'     = E.modE x y

-- | Verify that @divModE` produces the right quotient and remainder.
divModProperty2 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
divModProperty2 x y = (y == 0) || (x `E.divE` y) * y + (x `E.modE` y) == x

-- | Verify that @divModE@ produces a remainder smaller than the divisor with
-- regards to the Euclidean domain's function.
modProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
modProperty1 x y = (y == 0) || (E.norm $ x `E.modE` y) < (E.norm y)

-- | Verify that @quotE@ and @reE@ are what `quotRemE` produces.
quotRemProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
quotRemProperty1 x y = (y == 0) || q == q' && r == r'
  where
    (q, r) = E.quotRemE x y
    q'     = E.quotE x y
    r'     = E.remE x y

-- | Verify that @quotRemE@ produces the right quotient and remainder.
quotRemProperty2 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
quotRemProperty2 x y = (y == 0) || (x `E.quotE` y) * y + (x `E.remE` y) == x

-- | Verify that @gcd z2 z2@ always divides @z1@ and @z2@.
gcdEProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
gcdEProperty1 z1 z2
  = z1 == 0 && z2 == 0
  || z1 `E.remE` z == 0 && z2 `E.remE` z == 0 && z == abs z
  where
    z = E.gcdE z1 z2

-- | Verify that a common divisor of @z1, z2@ is a always divisor of @gcdE z1 z2@.
gcdEProperty2 :: E.EisensteinInteger -> E.EisensteinInteger -> E.EisensteinInteger -> Bool
gcdEProperty2 z z1 z2
  = z == 0
  || (E.gcdE z1' z2') `E.remE` z == 0
  where
    z1' = z * z1
    z2' = z * z2

-- | A special case that tests rounding/truncating in GCD.
gcdESpecialCase1 :: Assertion
gcdESpecialCase1 = assertEqual "gcdE" 1 $ E.gcdE (12 E.:+ 23) (23 E.:+ 34)

findPrimesProperty1 :: Positive Int -> Bool
findPrimesProperty1 (Positive index) =
    let -- Only retain primes that are of the form @6k + 1@, for some nonzero natural @k@.
        prop prime = prime `mod` 6 == 1
        p = (!! index) $ filter prop $ drop 3 primes
    in E.isPrime $ E.findPrime p

testSuite :: TestTree
testSuite = testGroup "EisensteinIntegers" $
  [ testSmallAndQuick "forall z . z == signum z * abs z" signumAbsProperty
  , testSmallAndQuick "abs z always returns an @EisensteinInteger@ in the\
                      \ first sextant of the complex plane" absProperty
  , testGroup "Division"
    [ testSmallAndQuick "divE and modE work properly" divModProperty1
    , testSmallAndQuick "divModE works properly" divModProperty2
    , testSmallAndQuick "The remainder's norm is smaller than the divisor's"
                        modProperty1

    , testSmallAndQuick "quotE and remE work properly" quotRemProperty1
    , testSmallAndQuick "quotRemE works properly" quotRemProperty2
    ]

  , testGroup "g.c.d."
    [ testSmallAndQuick "The g.c.d. of two Eisenstein integers divides them"
                        gcdEProperty1
    , testSmallAndQuick "A common divisor of two Eisenstein integers always\
                        \ divides the g.c.d. of those two integers"
                        gcdEProperty2
    , testCase          "g.c.d. (12 :+ 23) (23 :+ 34)" gcdESpecialCase1
    ]
  
  , testGroup "Primality"
    [ testSmallAndQuick "Eisenstein primes found by the norm search used in\
                        \ findPrime are really prime"
                        findPrimesProperty1

    ]
  ]
