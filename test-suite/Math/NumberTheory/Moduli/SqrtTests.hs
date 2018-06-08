-- |
-- Module:      Math.NumberTheory.Moduli.SqrtTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Moduli.Sqrt
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.SqrtTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (nub)
import Data.Maybe (fromJust)

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.UniqueFactorisation (unPrime, isPrime)
import Math.NumberTheory.TestUtils

unwrapP :: PrimeWrapper Integer -> Integer
unwrapP (PrimeWrapper p) = unPrime p

unwrapPP :: (PrimeWrapper Integer, Power Int) -> (Integer, Int)
unwrapPP (p, Power e) = (unwrapP p, e)

-- FIXME: update names. Maybe not - they describe specific behavior inside interfaces.

-- | Check that 'sqrtMod' is defined iff a quadratic residue exists.
--   Also check that the result is a solution of input modular equation.
sqrtModPProperty :: AnySign Integer -> PrimeWrapper Integer -> Bool
sqrtModPProperty (AnySign n) (unwrapP -> p) = case sqrtModMaybe n p of
  Nothing -> jacobi n p == MinusOne
  Just rt -> (p == 2 || jacobi n p /= MinusOne) && rt ^ 2 `mod` p == n `mod` p

sqrtModPListProperty :: AnySign Integer -> PrimeWrapper Integer -> Bool
sqrtModPListProperty (AnySign n) (unwrapP -> p) = all (\rt -> rt ^ 2 `mod` p == n `mod` p) (sqrtModList n p)

sqrtModP'Property :: Positive Integer -> PrimeWrapper Integer -> Bool
sqrtModP'Property (Positive n) (unwrapP -> p) = (p /= 2 && jacobi n p /= One) || rt ^ 2 `mod` p == n `mod` p
  where
    rt = sqrtModExact n p

tonelliShanksProperty1 :: Positive Integer -> PrimeWrapper Integer -> Bool
tonelliShanksProperty1 (Positive n) (unwrapP -> p) = p `mod` 4 /= 1 || jacobi n p /= One || rt ^ 2 `mod` p == n `mod` p
  where
    rt = sqrtModExact n p

tonelliShanksProperty2 :: PrimeWrapper Integer -> Bool
tonelliShanksProperty2 (unwrapP -> p) = p `mod` 4 /= 1 || rt ^ 2 `mod` p == n `mod` p
  where
    n  = head $ filter (\s -> jacobi s p == One) [2..p-1]
    rt = sqrtModExact n p

tonelliShanksSpecialCases :: Assertion
tonelliShanksSpecialCases =
  assertEqual "OEIS A002224" [6, 32, 219, 439, 1526, 2987, 22193, 11740, 13854, 91168, 326277, 232059, 3230839, 4379725, 11754394, 32020334, 151024619, 345641931, 373671108, 1857111865, 8110112775, 4184367042] rts
  where
    ps = [17, 73, 241, 1009, 2689, 8089, 33049, 53881, 87481, 483289, 515761, 1083289, 3818929, 9257329, 22000801, 48473881, 175244281, 427733329, 898716289, 8114538721, 9176747449, 23616331489]
    rts = map (\p -> sqrtModExact 2 p) ps

sqrtModPPProperty :: AnySign Integer -> (PrimeWrapper Integer, Power Int) -> Bool
sqrtModPPProperty (AnySign n) (unwrapP -> p, Power e) = gcd n p > 1 || case sqrtModMaybe n (p ^ e) of
  Nothing -> True
  Just rt -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)

sqrtModPPBase2Property :: AnySign Integer -> Power Int -> Bool
sqrtModPPBase2Property n e = sqrtModPPProperty n (PrimeWrapper $ fromJust $ isPrime (2 :: Integer), e)

sqrtModPPSpecialCase1 :: Assertion
sqrtModPPSpecialCase1 =
  assertEqual "sqrtModPP 16 2 2 = 4" (Just 0) (sqrtModMaybe 16 (2 ^ 2))

sqrtModPPSpecialCase2 :: Assertion
sqrtModPPSpecialCase2 =
  assertEqual "sqrtModPP 16 3 2 = 4" (Just 4) (sqrtModMaybe 16 (3 ^ 2))

sqrtModPPListProperty :: AnySign Integer -> (PrimeWrapper Integer, Power Int) -> Bool
sqrtModPPListProperty (AnySign n) (unwrapP -> p, Power e) = gcd n p > 1
  || all (\rt -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) (sqrtModList n (p ^ e))

sqrtModFProperty :: AnySign Integer -> [(PrimeWrapper Integer, Power Int)] -> Bool
sqrtModFProperty (AnySign n) (map unwrapPP -> pes) = case sqrtModF n pes of
  Nothing -> True
  Just rt -> all (\(p, e) -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) pes

sqrtModFListProperty :: AnySign Integer -> [(PrimeWrapper Integer, Power Int)] -> Bool
sqrtModFListProperty (AnySign n) (map unwrapPP -> pes)
  = nub ps /= ps || all
    (\rt -> all (\(p, e) -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) pes)
    (sqrtModFList n pes)
  where
    ps = map fst pes

sqrtModFListSpecialCase :: Assertion
sqrtModFListSpecialCase =
  assertEqual "sqrtModPPList 0 [(2,1), (3,1), (5,1)]" [0] (sqrtModFList 0 [(2,1), (3,1), (5,1)])

testSuite :: TestTree
testSuite = testGroup "Sqrt"
  [ testSmallAndQuick "sqrtModP"         sqrtModPProperty
  , testSmallAndQuick "sqrtModPList"     sqrtModPListProperty
  , testSmallAndQuick "sqrtModP'"        sqrtModP'Property
  , testGroup "tonelliShanks"
    [ testSmallAndQuick "generic"          tonelliShanksProperty1
    , testSmallAndQuick "smallest residue" tonelliShanksProperty2
    , testCase          "OEIS A002224"     tonelliShanksSpecialCases
    ]
  , testGroup "sqrtModPP"
    [ testSmallAndQuick "generic"        sqrtModPPProperty
    , testSmallAndQuick "_  2 _"         sqrtModPPBase2Property
    , testCase          "16 2 2"         sqrtModPPSpecialCase1
    , testCase          "16 3 2"         sqrtModPPSpecialCase2
    ]
  , testSmallAndQuick "sqrtModPPList"    sqrtModPPListProperty
  , testSmallAndQuick "sqrtModF"         sqrtModFProperty
  , testSmallAndQuick "sqrtModFList"     sqrtModFListProperty
  , testCase          "sqrtModFList 0 [(2,1), (3,1), (5,1)]" sqrtModFListSpecialCase
  ]
