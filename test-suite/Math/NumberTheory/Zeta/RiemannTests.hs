-- |
-- Module:      Math.NumberTheory.Zeta.RiemannTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Zeta.Riemann
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Zeta.RiemannTests
  ( testSuite
  ) where

import Data.ExactPi                (approximateValue)

import Test.Tasty
import Test.Tasty.HUnit            (Assertion, assertEqual, testCase)

import Math.NumberTheory.Zeta
import Math.NumberTheory.TestUtils

epsilon :: Double
epsilon = 1e-40

zetasEvenSpecialCase1 :: Assertion
zetasEvenSpecialCase1
  = assertEqual "zeta(0) = -1/2"
    (approximateValue $ zetasEven !! 0)
    (-1 / 2)

zetasEvenSpecialCase2 :: Assertion
zetasEvenSpecialCase2
  = assertEqualUpToEps "zeta(2) = pi^2/6" epsilon
    (approximateValue $ zetasEven !! 1)
    (pi * pi / 6)

zetasEvenSpecialCase3 :: Assertion
zetasEvenSpecialCase3
  = assertEqualUpToEps "zeta(4) = pi^4/90" epsilon
    (approximateValue $ zetasEven !! 2)
    (pi ^ 4 / 90)

zetasEvenProperty1 :: Positive Int -> Bool
zetasEvenProperty1 (Positive m)
  =  zetaM < 1
  || zetaM > zetaM1
  where
    zetaM  = approximateValue (zetasEven !! m)
    zetaM1 = approximateValue (zetasEven !! (m + 1))

zetasEvenProperty2 :: Positive Int -> Bool
zetasEvenProperty2 (Positive m)
  = abs (zetaM - zetaM') < epsilon
  where
    zetaM  = approximateValue (zetasEven !! m)
    zetaM' = zetas' !! (2 * m)

zetas' :: [Double]
zetas' = zetas epsilon

zetasSpecialCase1 :: Assertion
zetasSpecialCase1
  = assertEqual "zeta(1) = Infinity"
    (zetas' !! 1)
    (1 / 0)

zetasSpecialCase2 :: Assertion
zetasSpecialCase2
  = assertEqualUpToEps "zeta(3) = 1.2020569" epsilon
    (zetas' !! 3)
    1.2020569031595942853997381615114499908

zetasSpecialCase3 :: Assertion
zetasSpecialCase3
  = assertEqualUpToEps "zeta(5) = 1.0369277" epsilon
    (zetas' !! 5)
    1.0369277551433699263313654864570341681

zetasProperty1 :: Positive Int -> Bool
zetasProperty1 (Positive m)
  =  zetaM >= zetaM1
  && zetaM1 >= 1
  where
    zetaM  = zetas' !! m
    zetaM1 = zetas' !! (m + 1)

-- | Let z1 be an approximation of z with precision eps1,
-- and z2 be an approximation of the same value with precision eps2.
-- Then (independently of the true value of z)
-- abs (z1 - z2) < eps1 + eps2.
zetasProperty2 :: NonNegative Int -> NonNegative Int -> Bool
zetasProperty2 (NonNegative e1) (NonNegative e2)
  = maximum (take 25 $ drop 2 $ zipWith ((abs .) . (-)) (zetas eps1) (zetas eps2)) < eps1 + eps2
  where
    eps1, eps2 :: Double
    eps1 = 1.0 / 2 ^ e1
    eps2 = 1.0 / 2 ^ e2

testSuite :: TestTree
testSuite = testGroup "Zeta"
  [ testGroup "zetasEven"
    [ testCase "zeta(0)"                          zetasEvenSpecialCase1
    , testCase "zeta(2)"                          zetasEvenSpecialCase2
    , testCase "zeta(4)"                          zetasEvenSpecialCase3
    , testSmallAndQuick "zeta(2n) > zeta(2n+2)"   zetasEvenProperty1
    , testSmallAndQuick "zetasEven matches zetas" zetasEvenProperty2
    ]
  , testGroup "zetas"
    [ testCase "zeta(1)"                          zetasSpecialCase1
    , testCase "zeta(3)"                          zetasSpecialCase2
    , testCase "zeta(5)"                          zetasSpecialCase3
    , testSmallAndQuick "zeta(n) > zeta(n+1)"     zetasProperty1
    , testSmallAndQuick "precision"               zetasProperty2
    ]
  ]
