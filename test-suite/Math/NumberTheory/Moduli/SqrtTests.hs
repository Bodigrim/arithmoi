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

import Data.List (nub)

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

unwrapPP :: (Prime, Power Int) -> (Integer, Int)
unwrapPP (Prime p, Power e) = (p, e)

-- | Check that 'sqrtMod' is defined iff a quadratic residue exists.
--   Also check that the result is a solution of input modular equation.
sqrtModPProperty :: AnySign Integer -> Prime -> Bool
sqrtModPProperty (AnySign n) (Prime p) = case sqrtModP n p of
  Nothing -> jacobi n p == MinusOne
  Just rt -> (p == 2 || jacobi n p /= MinusOne) && rt ^ 2 `mod` p == n `mod` p

sqrtModPListProperty :: AnySign Integer -> Prime -> Bool
sqrtModPListProperty (AnySign n) (Prime p) = all (\rt -> rt ^ 2 `mod` p == n `mod` p) (sqrtModPList n p)

sqrtModP'Property :: Positive Integer -> Prime -> Bool
sqrtModP'Property (Positive n) (Prime p) = (p /= 2 && jacobi n p /= One) || rt ^ 2 `mod` p == n `mod` p
  where
    rt = sqrtModP' n p

tonelliShanksProperty :: Positive Integer -> Prime -> Bool
tonelliShanksProperty (Positive n) (Prime p) = p `mod` 4 /= 1 || jacobi n p /= One || rt ^ 2 `mod` p == n `mod` p
  where
    rt = tonelliShanks n p

sqrtModPPProperty :: AnySign Integer -> (Prime, Power Int) -> Bool
sqrtModPPProperty (AnySign n) (Prime p, Power e) = gcd n p > 1 || case sqrtModPP n (p, e) of
  Nothing -> True
  Just rt -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)

sqrtModPPListProperty :: AnySign Integer -> (Prime, Power Int) -> Bool
sqrtModPPListProperty (AnySign n) (Prime p, Power e) = gcd n p > 1
  || all (\rt -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) (sqrtModPPList n (p, e))

sqrtModFProperty :: AnySign Integer -> [(Prime, Power Int)] -> Bool
sqrtModFProperty (AnySign n) (map unwrapPP -> pes) = case sqrtModF n pes of
  Nothing -> True
  Just rt -> all (\(p, e) -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) pes

sqrtModFListProperty :: AnySign Integer -> [(Prime, Power Int)] -> Bool
sqrtModFListProperty (AnySign n) (map unwrapPP -> pes)
  = nub ps /= ps || all
    (\rt -> all (\(p, e) -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) pes)
    (sqrtModFList n pes)
  where
    ps = map fst pes

testSuite :: TestTree
testSuite = testGroup "Sqrt"
  [ testSmallAndQuick "sqrtModP"      sqrtModPProperty
  , testSmallAndQuick "sqrtModPList"  sqrtModPListProperty
  , testSmallAndQuick "sqrtModP'"     sqrtModP'Property
  , testSmallAndQuick "tonelliShanks" tonelliShanksProperty
  , testSmallAndQuick "sqrtModPP"     sqrtModPPProperty
  , testSmallAndQuick "sqrtModPPList" sqrtModPPListProperty
  , testSmallAndQuick "sqrtModF"      sqrtModFProperty
  , testSmallAndQuick "sqrtModFList"  sqrtModFListProperty
  ]
