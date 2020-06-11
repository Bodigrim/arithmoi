-- |
-- Module:      Math.NumberTheory.EisensteinIntegersTests
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.
--
-- Tests for Math.NumberTheory.EisensteinIntegers
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.EisensteinIntegersTests
  ( testSuite
  ) where

import Prelude hiding (gcd, rem, quot, quotRem)
import Data.Euclidean
import Data.Maybe (fromJust, isJust)
import Data.Proxy
import Test.Tasty.QuickCheck as QC hiding (Positive, getPositive, NonNegative, generate, getNonNegative)
import Test.QuickCheck.Classes
import Test.Tasty                                     (TestTree, testGroup)
import Test.Tasty.HUnit                               (Assertion, assertEqual,
                                                      testCase)

import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E
import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

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

-- | Verify that @rem@ produces a remainder smaller than the divisor with
-- regards to the Euclidean domain's function.
remProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
remProperty1 x y = (y == 0) || E.norm (x `rem` y) < E.norm y

-- | Verify that @quot@ and @rem@ are what `quotRem` produces.
quotRemProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
quotRemProperty1 x y = (y == 0) || q == q' && r == r'
  where
    (q, r) = quotRem x y
    q'     = quot x y
    r'     = rem x y

-- | Verify that @quotRemE@ produces the right quotient and remainder.
quotRemProperty2 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
quotRemProperty2 x y = (y == 0) || (x `quot` y) * y + (x `rem` y) == x

-- | Verify that @gcd z1 z2@ always divides @z1@ and @z2@.
gcdEProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
gcdEProperty1 z1 z2
  = z1 == 0 && z2 == 0
  || z1 `rem` z == 0 && z2 `rem` z == 0
  where
    z = gcd z1 z2

-- | Verify that a common divisor of @z1, z2@ is a always divisor of @gcd z1 z2@.
gcdEProperty2 :: E.EisensteinInteger -> E.EisensteinInteger -> E.EisensteinInteger -> Bool
gcdEProperty2 z z1 z2
  = z == 0
  || gcd z1' z2' `rem` z == 0
  where
    z1' = z * z1
    z2' = z * z2

-- | A special case that tests rounding/truncating in GCD.
gcdESpecialCase1 :: Assertion
gcdESpecialCase1 = assertEqual "gcd" (1 E.:+ 1) $ gcd (12 E.:+ 23) (23 E.:+ 34)

findPrimesProperty1 :: Positive Int -> Bool
findPrimesProperty1 (Positive index) =
    let -- Only retain primes that are of the form @6k + 1@, for some nonzero natural @k@.
        prop prime = unPrime prime `mod` 6 == 1
        p = (!! index) $ filter prop $ drop 3 primes
    in isJust (isPrime (unPrime (E.findPrime p) :: E.EisensteinInteger))

-- | Checks that the @norm@ of the Euclidean domain of Eisenstein integers
-- is multiplicative i.e.
-- @forall e1 e2 in Z[ω] . norm(e1 * e2) == norm(e1) * norm(e2)@.
euclideanDomainProperty1 :: E.EisensteinInteger -> E.EisensteinInteger -> Bool
euclideanDomainProperty1 e1 e2 = E.norm (e1 * e2) == E.norm e1 * E.norm e2

-- | Checks that the numbers produced by @primes@ are actually Eisenstein
-- primes.
primesProperty1 :: Positive Int -> Bool
primesProperty1 (Positive index) = all (isJust . isPrime . (unPrime :: Prime E.EisensteinInteger -> E.EisensteinInteger)) $ take index E.primes

-- | Checks that the infinite list of Eisenstein primes @primes@ is ordered
-- by the numbers' norm.
primesProperty2 :: Positive Int -> Bool
primesProperty2 (Positive index) =
    let isOrdered :: [Prime E.EisensteinInteger] -> Bool
        isOrdered xs = all (\(x, y) -> E.norm (unPrime x) <= E.norm (unPrime y)) . zip xs $ tail xs
    in isOrdered $ take index E.primes

-- | Checks that the numbers produced by @primes@ are all in the first
-- sextant.
primesProperty3 :: Positive Int -> Bool
primesProperty3 (Positive index) =
    all (\e -> abs (unPrime e) == (unPrime e :: E.EisensteinInteger)) $ take index E.primes

-- | An Eisenstein integer is either zero or associated (i.e. equal up to
-- multiplication by a unit) to the product of its factors raised to their
-- respective exponents.
factoriseProperty1 :: E.EisensteinInteger -> Bool
factoriseProperty1 g = g == 0 || abs g == abs g'
  where
    factors = factorise g
    g' = product $ map (\(p, k) -> unPrime p ^ k) factors

-- | Check that there are no factors with exponent @0@ in the factorisation.
factoriseProperty2 :: E.EisensteinInteger -> Bool
factoriseProperty2 z = z == 0 || all ((> 0) . snd) (factorise z)

-- | Check that no factor produced by @factorise@ is a unit.
factoriseProperty3 :: E.EisensteinInteger -> Bool
factoriseProperty3 z = z == 0 || all ((> 1) . E.norm . unPrime . fst) (factorise z)

factoriseSpecialCase1 :: Assertion
factoriseSpecialCase1 = assertEqual "should be equal"
  [ (fromJust $ isPrime $ 2 E.:+ 1, 3)
  , (fromJust $ isPrime $ 3 E.:+ 1, 1)
  ]
  (factorise (15 E.:+ 12))

testSuite :: TestTree
testSuite = testGroup "EisensteinIntegers"
  [ testSmallAndQuick "forall z . z == signum z * abs z" signumAbsProperty
  , testSmallAndQuick "abs z rotates to the first sextant" absProperty
  , testGroup "Division"
    [ testSmallAndQuick "The remainder's norm is smaller than the divisor's"
                        remProperty1

    , testSmallAndQuick "quotE and remE work properly" quotRemProperty1
    , testSmallAndQuick "quotRemE works properly" quotRemProperty2
    ]

  , testGroup "g.c.d."
    [ testSmallAndQuick "The g.c.d. of two Eisenstein integers divides them"
                        gcdEProperty1
    -- smallcheck takes too long
    , QC.testProperty "Common divisor divides gcd"
                        gcdEProperty2
    , testCase          "g.c.d. (12 :+ 23) (23 :+ 34)" gcdESpecialCase1
    ]
  , testSmallAndQuick "The Eisenstein norm function is multiplicative"
                    euclideanDomainProperty1
  , testGroup "Primality"
    [ testSmallAndQuick "findPrime returns prime"
                        findPrimesProperty1
    , testSmallAndQuick "primes are actually prime"
                        primesProperty1
    , testSmallAndQuick "primes is ordered"
                        primesProperty2
    , testSmallAndQuick "primes are in the first sextant"
                        primesProperty3
    ]

    , testGroup "Factorisation"
      [ testSmallAndQuick "factorise produces correct results"
                          factoriseProperty1
      , testSmallAndQuick "factorise produces no factors with exponent 0"
                          factoriseProperty2
      , testSmallAndQuick "factorise produces no unit factors"
                          factoriseProperty3
      , testCase          "factorise 15:+12" factoriseSpecialCase1
      ]
  , lawsToTest $ gcdDomainLaws (Proxy :: Proxy E.EisensteinInteger)
  , lawsToTest $ euclideanLaws (Proxy :: Proxy E.EisensteinInteger)
  ]
