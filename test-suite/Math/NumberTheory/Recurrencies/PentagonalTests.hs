{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Recurrencies.PentagonalTests
  ( testSuite
  ) where

import Data.Proxy                     (Proxy (..))
import GHC.Natural                    (Natural)
import GHC.TypeNats.Compat            (SomeNat (..), someNatVal)

import Math.NumberTheory.Moduli       (Mod, getVal)
import Math.NumberTheory.Recurrencies (partition, pentagonalSigns, pents)
import Math.NumberTheory.TestUtils

import Test.Tasty
import Test.Tasty.HUnit

-- | Helper to avoid writing @partition !!@ too many times.
partition' :: (Enum a, Num a, Ord a) => Int -> a
partition' = (partition !!)

-- | Check that the @k@-th generalized pentagonal number is
-- @div (3 * k² - k) 2@, where @k ∈ {0, 1, −1, 2, −2, 3, −3, 4, ...}@.
-- Notice that @-1@ is the @2 * abs (-1) == 2@-nd index in the zero-based list,
-- while @2@ is the @2 * 2 - 1 == 3@-rd, and so on.
pentagonalNumbersProperty1 :: AnySign Int -> Bool
pentagonalNumbersProperty1 (AnySign n)
    | n == 0    = pents !! 0           == 0
    | n > 0     = pents !! (2 * n - 1) == pent n
    | otherwise = pents !! (2 * abs n) == pent n
  where
    pent m = div (3 * (m * m) - m) 2

-- | Check that @partition !! 0@ is 1.
partitionSpecialCase0 :: Assertion
partitionSpecialCase0 = assertEqual "partition" (partition' 0) 1

-- Check that @partition !! 1@ is 1.
partitionSpecialCase1 :: Assertion
partitionSpecialCase1 = assertEqual "partition" (partition' 1) 1

-- | Check that @p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-11) + ...@,
-- where @p(x) = 0@ for any negative integer.
partitionProperty1 :: Positive Int -> Bool
partitionProperty1 (Positive n) =
    partition' n == (sum .
                     pentagonalSigns .
                     map (\m -> partition' (n - m)) .
                     takeWhile (\m -> n - m >= 0) .
                     tail $ pents)

-- | Check that
-- @partition :: [Math.NumberTheory.Moduli.Mod n] == map (`mod` n) partition@.
partitionProperty2 :: NonNegative Integer -> Positive Natural -> Bool
partitionProperty2 (NonNegative m)
                   n@(someNatVal . getPositive -> (SomeNat (Proxy :: Proxy n))) =
    (take m' . map getVal $ (partition :: [Mod n])) ==
    map helper (take m' partition :: [Integer])
  where
    m' = fromIntegral m
    n' = fromIntegral n
    helper x = x `mod` n'

testSuite :: TestTree
testSuite = testGroup "Pentagonal"
  [ testGroup "partition"
    [ testSmallAndQuick "matches definition"  partitionProperty1
    , testSmallAndQuick "mapping residue modulus 'n' is the same as giving\
                        \'partition' type '[Mod n]'" partitionProperty2
    , testCase          "partition !! 0"      partitionSpecialCase0
    , testCase          "partition !! 1"      partitionSpecialCase1
    ]
  , testGroup "Generalized pentagonal numbers"
    [ testSmallAndQuick "matches definition" pentagonalNumbersProperty1
    ]
  ]