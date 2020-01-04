-- |
-- Module:      Math.NumberTheory.Moduli.ChineseTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
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

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

chineseCoprimeProperty :: Integer -> Positive Integer -> Integer -> Positive Integer -> Bool
chineseCoprimeProperty n1 (Positive m1) n2 (Positive m2) = case gcd m1 m2 of
  1 -> case chineseCoprime (n1, m1) (n2, m2) of
    Nothing -> False
    Just n  -> (n - n1) `rem` m1 == 0 && (n - n2) `rem` m2 == 0
  _ -> case chineseCoprime (n1, m1) (n2, m2) of
    Nothing -> True
    Just{}  -> False

chineseProperty :: Integer -> Positive Integer -> Integer -> Positive Integer -> Bool
chineseProperty n1 (Positive m1) n2 (Positive m2) = if compatible
  then case chinese (n1, m1) (n2, m2) of
    Nothing -> False
    Just n  -> (n - n1) `rem` m1 == 0 && (n - n2) `rem` m2 == 0
  else case chineseCoprime (n1, m1) (n2, m2) of
    Nothing -> True
    Just{}  -> False
  where
    g = gcd m1 m2
    compatible = (n1 - n2) `rem` g == 0


testSuite :: TestTree
testSuite = testGroup "Chinese"
  [ testSmallAndQuick "chineseCoprime"    chineseCoprimeProperty
  , testSmallAndQuick "chinese"           chineseProperty
  ]
