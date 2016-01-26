-- |
-- Module:      Math.NumberTheory.Powers.CubesTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.Cubes
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.CubesTests
  ( testSuite
  ) where

import Test.Tasty
import Test.SmallCheck.Series

import Data.Maybe

import Math.NumberTheory.Powers.Cubes
import Math.NumberTheory.Powers.Utils

-- | Check that 'integerCubeRoot' returns the largest integer @m@ with @m^3 <= n@.
--
-- (m + 1) ^ 3 /= n && cond
-- means
-- (m + 1) ^ 3 > n
-- but without overflow for bounded types
integerCubeRootProperty :: Integral a => AnySign a -> Bool
integerCubeRootProperty (AnySign n) = m ^ 3 <= n && (m + 1) ^ 3 /= n && cond
  where
    m = integerCubeRoot n
    cond
      | m == -1   = n == -1
      | m < 0     = (m + 1) ^ 2 <= n `div` (m + 1)
      | otherwise = (m + 1) ^ 2 >= n `div` (m + 1)

-- | Check that 'integerCubeRoot'' returns the largest integer @m@ with @m^3 <= n@.
integerCubeRoot'Property :: Integral a => NonNegative a -> Bool
integerCubeRoot'Property (NonNegative n) = m ^ 3 <= n && (m + 1) ^ 3 /= n && (m + 1) ^ 2 >= n `div` (m + 1)
  where
    m = integerCubeRoot' n

-- | Check that the number 'isCube' iff its 'integerCubeRoot' is exact.
isCubeProperty :: Integral a => AnySign a -> Bool
isCubeProperty (AnySign n) = (n /= m ^ 3 && not t) || (n == m ^ 3 && t)
  where
    t = isCube n
    m = integerCubeRoot n

-- | Check that the number 'isCube'' iff its 'integerCubeRoot'' is exact.
isCube'Property :: Integral a => NonNegative a -> Bool
isCube'Property (NonNegative n) = (n /= m ^ 3 && not t) || (n == m ^ 3 && t)
  where
    t = isCube' n
    m = integerCubeRoot' n

-- | Check that 'exactCubeRoot' returns an exact integer cubic root
-- and is consistent with 'isCube'.
exactCubeRootProperty :: Integral a => AnySign a -> Bool
exactCubeRootProperty (AnySign n) = case exactCubeRoot n of
  Nothing -> not (isCube n)
  Just m  -> isCube n && n == m ^ 3

-- | Check that 'isPossibleCube' is consistent with 'exactCubeRoot'.
isPossibleCubeProperty :: Integral a => NonNegative a -> Bool
isPossibleCubeProperty (NonNegative n) = t || not t && isNothing m
  where
    t = isPossibleCube n
    m = exactCubeRoot n

testSuite :: TestTree
testSuite = testGroup "Cubes"
  [ testIntegralProperty "integerCubeRoot"  integerCubeRootProperty
  , testIntegralProperty "integerCubeRoot'" integerCubeRoot'Property
  , testIntegralProperty "isCube"           isCubeProperty
  , testIntegralProperty "isCube'"          isCube'Property
  , testIntegralProperty "exactCubeRoot"    exactCubeRootProperty
  , testIntegralProperty "isPossibleCube"   isPossibleCubeProperty
  ]
