-- |
-- Module:      Math.NumberTheory.Zeta.DirichletTests
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Tests for Math.NumberTheory.Zeta.Dirichlet
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Zeta.DirichletTests
  ( testSuite
  ) where

import Data.ExactPi                (approximateValue)

import Test.Tasty
import Test.Tasty.HUnit            (Assertion, assertEqual, testCase)

import Math.NumberTheory.Zeta      (betas, betasOdd)
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
  ]
