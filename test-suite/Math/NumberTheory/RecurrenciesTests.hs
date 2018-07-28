{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.RecurrenciesTests
  ( testSuite
  ) where

import Data.List                      ((!!))

import Math.NumberTheory.Recurrencies (partition, pentagonalSigns, pents)
import Math.NumberTheory.TestUtils

import Test.Tasty
import Test.Tasty.HUnit

-- | Helper to avoid writing @partition !!@ too many times.
partition' :: Integral a => Int -> a
partition' = (partition !!)

-- | Check that the @k@-th generalized pentagonal number is
-- @div (3 * k² - k) 2@, where @k ∈ {0, 1, −1, 2, −2, 3, −3, 4, ...}@.
-- Notice that @-1@ is the @2 * abs (-1) == 2@-nd index in the zero-based list,
-- while @2@ is the @2 * 2 - 1 == 3@-rd, and so on.
pentagonalNumbersProperty1 :: AnySign Int -> Bool
pentagonalNumbersProperty1 (AnySign n)
    | n == 0    = pents !! 0                 == 0
    | n > 0     = pents !! (2 * n - 1) == pent n
    | otherwise = pents !! (2 * abs n)       == pent n
  where
    pent m = div (3 * (m * m) - m) 2

-- | Check that @partition !! 0@ is 1.
partitionSpecialCase0 :: Assertion
partitionSpecialCase0 = assertEqual "partition" (partition' 0) 1

-- Check that @partition !! 1@ is 1.
partitionSpecialCase1 :: Assertion
partitionSpecialCase1 = assertEqual "partition" (partition' 1) 1

-- | Check that @p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(11) + ...@,
-- where @p(x) = 0@ for any negative integer.
partitionProperty1 :: Positive Int -> Bool
partitionProperty1 (Positive n) =
    partition' n == (sum .
                     pentagonalSigns .
                     map (\m -> partition' (n - m)) .
                     takeWhile (\m -> n - m >= 0) .
                     tail $ pents)

testSuite :: TestTree
testSuite = testGroup "Recurrencies"
  [ testGroup "partition"
    [ testSmallAndQuick "matches definition"  partitionProperty1
    , testCase          "partition !! 0"      partitionSpecialCase0
    , testCase          "partition !! 1"      partitionSpecialCase1
    ]
  , testGroup "Generalized pentagonal numbers"
    [ testSmallAndQuick "matches definition" pentagonalNumbersProperty1
    ]
  ]