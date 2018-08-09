-- |
-- Module:      Math.NumberTheory.Zeta.DirichletTests
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Zeta.Dirichlet
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Zeta.DirichletTests
  ( testSuite
  ) where

import Data.ExactPi                (approximateValue)
import Data.Ratio                  ((%), denominator)

import Test.Tasty
import Test.Tasty.HUnit            (Assertion, assertEqual, testCase)

import Math.NumberTheory.Zeta      (betas, betasOdd, euler, euler',
                                    eulerPolyAt1)
import Math.NumberTheory.TestUtils

epsilon :: Double
epsilon = 1e-14

betas' :: [Double]
betas' = betas epsilon

betasOddSpecialCase1 :: Assertion
betasOddSpecialCase1
  = assertEqualUpToEps "beta(1) = pi/4" epsilon
    (approximateValue $ betasOdd !! 0)
    (pi / 4)

betasOddSpecialCase2 :: Assertion
betasOddSpecialCase2
  = assertEqualUpToEps "beta(3) = pi^3/32" epsilon
    (approximateValue $ betasOdd !! 1)
    (pi^3 / 32)

betasOddSpecialCase3 :: Assertion
betasOddSpecialCase3
  = assertEqualUpToEps "beta(5) = 5*pi^5/1536" epsilon
    (approximateValue $ betasOdd !! 2)
    ((5 * pi^5) / 1536)

betasOddProperty1 :: Positive Int -> Bool
betasOddProperty1 (Positive m)
  =  betaM < 1
  || betaM < betaM1
  where
    betaM  = approximateValue (betasOdd !! m)
    betaM1 = approximateValue (betasOdd !! (m + 1))

betasOddProperty2 :: NonNegative Int -> Bool
betasOddProperty2 (NonNegative m)
  = abs (betaM - betaM') < epsilon
  where
    betaM  = approximateValue (betasOdd !! m)
    betaM' = betas' !! ((2 * m) + 1)

betasSpecialCase1 :: Assertion
betasSpecialCase1
  = assertEqual "beta(0) = 1/2"
    (betas' !! 0)
    (1 / 2)

betasSpecialCase2 :: Assertion
betasSpecialCase2
  = assertEqualUpToEps "beta(2) = 0.9159655" epsilon
    (betas' !! 2)
    (0.9159655941772190150546035149323841107)

betasSpecialCase3 :: Assertion
betasSpecialCase3
  = assertEqualUpToEps "beta(4) = 0.9889445" epsilon
    (betas' !! 4)
    (0.9889445517411053361084226332283778213)

betasProperty1 :: Positive Int -> Bool
betasProperty1 (Positive m)
  =  betaM <= betaM1
  && betaM1 <= 1
  where
    betaM  = betas' !! m
    betaM1 = betas' !! (m + 1)

betasProperty2 :: NonNegative Int -> NonNegative Int -> Bool
betasProperty2 (NonNegative e1) (NonNegative e2)
  = maximum (take 10 $ drop 2 $ zipWith ((abs .) . (-)) (betas eps1) (betas eps2)) <= eps1 + eps2
  where
    eps1, eps2 :: Double
    eps1 = (1.0 / 2) ^ e1
    eps2 = (1.0 / 2) ^ e2

-- | For every odd positive integer @n@, @E_n@ is @0@.
eulerProperty1 :: Positive Int -> Bool
eulerProperty1 (Positive n) = euler' !! (2 * n - 1) == 0

-- | @forall a . Integral a => euler :: [Ratio a]@ always computes @Ratio@s
-- with denominator @1@.
eulerProperty2 :: NonNegative Int -> Bool
eulerProperty2 (NonNegative n) = denominator (euler !! n) == 1

-- | Every positive even index produces a negative result.
eulerProperty3 :: NonNegative Int -> Bool
eulerProperty3 (NonNegative n) = euler !! (2 + 4 * n) < 0

-- | The Euler number sequence is https://oeis.org/A122045
eulerSpecialCase1 :: Assertion
eulerSpecialCase1 = assertEqual "euler"
    (take 20 euler')
    [1, 0, -1, 0, 5, 0, -61, 0, 1385, 0, -50521, 0, 2702765, 0, -199360981, 0, 19391512145, 0, -2404879675441, 0]

-- | For any even positive integer @n@, @E_n(1)@ is @0@.
eulerPAt1Property1 :: Positive Int -> Bool
eulerPAt1Property1 (Positive n) = (eulerPolyAt1 !! (2 * n)) == 0

-- | The numerators in this sequence are from https://oeis.org/A198631 while the
-- denominators are from https://oeis.org/A006519.
eulerPAt1SpecialCase1 :: Assertion
eulerPAt1SpecialCase1 = assertEqual "eulerPolyAt1"
    (take 20 eulerPolyAt1)
    (zipWith (%) [1, 1, 0, -1, 0, 1, 0, -17, 0, 31, 0, -691, 0, 5461, 0, -929569, 0, 3202291, 0, -221930581]
                 [1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1, 16, 1, 2, 1, 4])

testSuite :: TestTree
testSuite = testGroup "Beta"
  [ testGroup "betasOdd"
    [ testCase "beta(1)"                                        betasOddSpecialCase1
    , testCase "beta(3)"                                        betasOddSpecialCase2
    , testCase "beta(5)"                                        betasOddSpecialCase3
    , testSmallAndQuick "beta(2n-1) < beta(2n+1)"               betasOddProperty1
    , testSmallAndQuick "betasOdd matches betas"                betasOddProperty2
    ]
  , testGroup "betas"
    [ testCase "beta(0)"                                        betasSpecialCase1
    , testCase "beta(2)"                                        betasSpecialCase2
    , testCase "beta(4)"                                        betasSpecialCase3
    , testSmallAndQuick "beta(n) < beta(n+1)"                   betasProperty1
    , testSmallAndQuick "precision"                             betasProperty2
    ]

  , testGroup "Euler numbers"
    [ testCase "First 20 elements of E_n are correct"           eulerSpecialCase1
    , testSmallAndQuick "E_n with n odd is 0"                   eulerProperty1
    , testSmallAndQuick "E_n is always an entire integer"       eulerProperty2
    , testSmallAndQuick "E_n for n in [2,6,8,12..] is negative" eulerProperty3
    ]

  , testGroup "Euler Polynomial of order N evaluated at 1"
    [ testCase "First 20 elements of E_n(1) are correct"        eulerPAt1SpecialCase1
    , testSmallAndQuick "E_n(1) with n in [2,4,6..] is 0"       eulerPAt1Property1
    ]
  ]
