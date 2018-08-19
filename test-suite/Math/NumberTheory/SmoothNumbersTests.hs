-- |
-- Module:      Math.NumberTheory.SmoothNumbersTests
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.SmoothNumbersTests
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SmoothNumbersTests
  ( testSuite
  ) where

import Test.Tasty

import Data.List (genericDrop)
import qualified Data.Set as S
import Numeric.Natural

import Math.NumberTheory.SmoothNumbers
import Math.NumberTheory.TestUtils

fromSetListProperty :: Integral a => [a] -> Bool
fromSetListProperty xs = fromSet (S.fromList xs) == fromList xs

fromSmoothUpperBoundProperty :: Integral a => Positive a -> Bool
fromSmoothUpperBoundProperty (Positive n') = case fromSmoothUpperBound n of
    Nothing -> n < 2
    Just sb -> head (genericDrop (n - 1) (smoothOver sb)) == n
  where
    n = n' `mod` 5000

smoothOverInRangeProperty :: Integral a => SmoothBasis a -> Positive a -> Positive a -> Bool
smoothOverInRangeProperty s (Positive lo') (Positive diff')
  = xs == ys
  where
    lo   = lo'   `mod` 2^18
    diff = diff' `mod` 2^18
    hi   = lo + diff
    xs   = smoothOverInRange   s lo hi
    ys   = smoothOverInRangeBF s lo hi

testSuite :: TestTree
testSuite = testGroup "SmoothNumbers"
  [ testGroup "fromSet == fromList"
    [ testSmallAndQuick "Int"     (fromSetListProperty :: [Int] -> Bool)
    , testSmallAndQuick "Word"    (fromSetListProperty :: [Word] -> Bool)
    , testSmallAndQuick "Integer" (fromSetListProperty :: [Integer] -> Bool)
    , testSmallAndQuick "Natural" (fromSetListProperty :: [Natural] -> Bool)
    ]
  , testIntegralProperty "fromSmoothUpperBound" fromSmoothUpperBoundProperty
  , testGroup "smoothOverInRange == smoothOverInRangeBF"
    [ testSmallAndQuick "Int"
      (smoothOverInRangeProperty :: SmoothBasis Int -> Positive Int -> Positive Int -> Bool)
    , testSmallAndQuick "Word"
      (smoothOverInRangeProperty :: SmoothBasis Word -> Positive Word -> Positive Word -> Bool)
    , testSmallAndQuick "Integer"
      (smoothOverInRangeProperty :: SmoothBasis Integer -> Positive Integer -> Positive Integer -> Bool)
    , testSmallAndQuick "Natural"
      (smoothOverInRangeProperty :: SmoothBasis Natural -> Positive Natural -> Positive Natural -> Bool)
    ]
  ]
