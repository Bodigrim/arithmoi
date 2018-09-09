-- |
-- Module:      Math.NumberTheory.UniqueFactorisationTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.ArithmeticFunctions
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.UniqueFactorisationTests
  ( testSuite
  ) where

import Test.Tasty

import Math.NumberTheory.Quadratic.EisensteinIntegers
import Math.NumberTheory.Quadratic.GaussianIntegers
import Math.NumberTheory.UniqueFactorisation
import Math.NumberTheory.TestUtils

import Numeric.Natural

testRules :: forall a. (UniqueFactorisation a, Num a, Eq a) => a -> Bool
testRules n
  = n == 0
  || all (\(p, _) -> unP p == abs (unP p)) fs
  && abs n == abs (product (map (\(p, k) -> unP p ^ k) fs))
  where
    fs = factorise n

    unP :: Prime a -> a
    unP = unPrime

testSuite :: TestTree
testSuite = testGroup "UniqueFactorisation"
  [ testSmallAndQuick "Int"     (testRules :: Int     -> Bool)
  , testSmallAndQuick "Word"    (testRules :: Word    -> Bool)
  , testSmallAndQuick "Integer" (testRules :: Integer -> Bool)
  , testSmallAndQuick "Natural" (testRules :: Natural -> Bool)

  , testSmallAndQuick "GaussianInteger"   (testRules :: GaussianInteger   -> Bool)
  , testSmallAndQuick "EisensteinInteger" (testRules :: EisensteinInteger -> Bool)
  ]
