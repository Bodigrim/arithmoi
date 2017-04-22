-- |
-- Module:      Math.NumberTheory.Moduli.ChineseTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Moduli.Chinese
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.ChineseTests
  ( testSuite
  ) where

import Test.Tasty

import Control.Arrow
import Data.List (tails)

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

-- | Check that 'chineseRemainder' is defined iff modulos are coprime.
--   Also check that the result is a solution of input modular equations.
chineseRemainderProperty :: [(Integer, Positive Integer)] -> Bool
chineseRemainderProperty rms' = case chineseRemainder rms of
  Nothing -> not areCoprime
  Just n  -> areCoprime && map (n `mod`) ms == zipWith mod rs ms
  where
    rms = map (second getPositive) rms'
    (rs, ms) = unzip rms
    areCoprime = all (== 1) [ gcd m1 m2 | (m1 : m2s) <- tails ms, m2 <- m2s ]

-- | Check that 'chineseRemainder' matches 'chineseRemainder2'.
chineseRemainder2Property :: Integer -> Positive Integer -> Integer -> Positive Integer -> Bool
chineseRemainder2Property r1 (Positive m1) r2 (Positive m2) = gcd m1 m2 /= 1
  || Just (chineseRemainder2 (r1, m1) (r2, m2)) == chineseRemainder [(r1, m1), (r2, m2)]

testSuite :: TestTree
testSuite = testGroup "Chinese"
  [ testSmallAndQuick "chineseRemainder"  chineseRemainderProperty
  , testSmallAndQuick "chineseRemainder2" chineseRemainder2Property
  ]
