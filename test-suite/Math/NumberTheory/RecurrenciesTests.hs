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

partition' :: Integral a => Int -> a
partition' = (partition !!)

partitionProperty1 :: Positive Int -> Bool
partitionProperty1 (Positive n) =
    partition' n == (sum .
                     pentagonalSigns True .
                     map (\m -> partition' (n - m)) .
                     takeWhile (\m -> n - m >= 0) .
                     tail $ pents)

partitionSpecialCase0 :: Assertion
partitionSpecialCase0 = assertEqual "partition" (partition' 0) 1

partitionSpecialCase1 :: Assertion
partitionSpecialCase1 = assertEqual "partition" (partition' 1) 1

testSuite :: TestTree
testSuite = testGroup "Recurrencies"
  [ testGroup "partition"
    [ testSmallAndQuick "matches definition"  partitionProperty1
    , testCase          "partition !! 0"      partitionSpecialCase0
    , testCase          "partition !! 1"      partitionSpecialCase1
    ]
  ]