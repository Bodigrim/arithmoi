-- |
-- Module:      Math.NumberTheory.Moduli.SingletonTests
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Moduli.Singleton
--

{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.SingletonTests
  ( testSuite
  ) where

import Test.Tasty

import qualified Data.Map as M

import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

someSFactorsProperty1
  :: (Ord a, Num a)
  => [(Prime a, Word)]
  -> Bool
someSFactorsProperty1 xs = case someSFactors xs of
  Some sm -> unSFactors sm == M.assocs (M.fromListWith (+) xs)

cyclicGroupFromModuloProperty1
  :: (Integral a, UniqueFactorisation a)
  => Positive a
  -> Bool
cyclicGroupFromModuloProperty1 (Positive m) = mcg1 == mcg2
  where
    mcg1 = cyclicGroupFromModulo m
    mcg2 = cyclicGroupFromFactors (factorise m)

testSuite :: TestTree
testSuite = testGroup "Singleton"
  [ testSmallAndQuick "unSFactors . someSFactors = id" (someSFactorsProperty1 @Integer)
  , testIntegralPropertyNoLarge "cyclicGroupFromModulo = cyclicGroupFromFactors . factorise" cyclicGroupFromModuloProperty1
  ]
