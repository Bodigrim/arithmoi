-- |
-- Module:      Math.NumberTheory.Powers.SquaresTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.Squares
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.SquaresTests
  ( testSuite
  ) where

import Test.Tasty
import Test.SmallCheck.Series

import Data.Functor.Identity
import Data.Maybe

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Powers.Utils

-- (m + 1) ^ 2 /= n && m + 1 >= n `div` (m + 1)
-- means
-- (m + 1) ^ 2 > n
-- but without overflow for bounded types
integerSquareRootProperty :: Integral a => NonNegative a -> Bool
integerSquareRootProperty (NonNegative n) = m >=0 && m * m <= n && (m + 1) ^ 2 /= n && m + 1 >= n `div` (m + 1)
  where
    m = integerSquareRoot n

integerSquareRoot'Property :: Integral a => NonNegative a -> Bool
integerSquareRoot'Property (NonNegative n) = m >=0 && m * m <= n && (m + 1) ^ 2 /= n && m + 1 >= n `div` (m + 1)
  where
    m = integerSquareRoot' n

isSquareProperty :: Integral a => Identity a -> Bool
isSquareProperty (Identity n) = (n < 0 && not t) || (n /= m * m && not t) || (n == m * m && t)
  where
    t = isSquare n
    m = integerSquareRoot n

isSquare'Property :: Integral a => NonNegative a -> Bool
isSquare'Property (NonNegative n) = (n /= m * m && not t) || (n == m * m && t)
  where
    t = isSquare' n
    m = integerSquareRoot' n

exactSquareRootProperty :: Integral a => Identity a -> Bool
exactSquareRootProperty (Identity n) = case exactSquareRoot n of
  Nothing -> not (isSquare n)
  Just m  -> isSquare n && n == m * m

isPossibleSquareProperty :: Integral a => NonNegative a -> Bool
isPossibleSquareProperty (NonNegative n) = t || not t && not t2 && isNothing m
  where
    t = isPossibleSquare n
    t2 = isPossibleSquare2 n
    m = exactSquareRoot n

isPossibleSquare2Property :: Integral a => NonNegative a -> Bool
isPossibleSquare2Property (NonNegative n) = t || not t && isNothing m
  where
    t = isPossibleSquare2 n
    m = exactSquareRoot n


testSuite :: TestTree
testSuite = testGroup "Squares"
  [ testIntegralProperty "integerSquareRoot"  integerSquareRootProperty
  , testIntegralProperty "integerSquareRoot'" integerSquareRoot'Property
  , testIntegralProperty "isSquare"           isSquareProperty
  , testIntegralProperty "isSquare'"          isSquare'Property
  , testIntegralProperty "exactSquareRoot"    exactSquareRootProperty
  , testIntegralProperty "isPossibleSquare"   isPossibleSquareProperty
  , testIntegralProperty "isPossibleSquare2"  isPossibleSquare2Property
  ]
