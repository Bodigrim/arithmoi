-- |
-- Module:      Math.NumberTheory.Recurrences.PentagonalTests
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Tests for Math.NumberTheory.Recurrences.Pentagonal
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Recurrences.PentagonalTests
  ( testSuite
  ) where

import Data.Proxy                    (Proxy (..))
import GHC.Natural                   (Natural)
import GHC.TypeNats                  (SomeNat (..), someNatVal)

import Math.NumberTheory.Moduli      (Mod, getVal)
import Math.NumberTheory.Recurrences (partition)
import Math.NumberTheory.TestUtils

import Test.Tasty
import Test.Tasty.HUnit

-- | Helper to avoid writing @partition !!@ too many times.
partition' :: Num a => Int -> a
partition' = (partition !!)

-- | Check that the @k@-th generalized pentagonal number is
-- @div (3 * k² - k) 2@, where @k ∈ {0, 1, -1, 2, -2, 3, -3, 4, ...}@.
-- Notice that @-1@ is the @2 * abs (-1) == 2@-nd index in the zero-based list,
-- while @2@ is the @2 * 2 - 1 == 3@-rd, and so on.
pentagonalNumbersProperty1 :: AnySign Int -> Bool
pentagonalNumbersProperty1 (AnySign n)
    | n == 0    = pents !! 0           == 0
    | n > 0     = pents !! (2 * n - 1) == pent n
    | otherwise = pents !! (2 * abs n) == pent n
  where
    pent m = div (3 * (m * m) - m) 2

-- | Check that the first 20 elements of @partition@ are correct per
-- https://oeis.org/A000041.
partitionSpecialCase20 :: Assertion
partitionSpecialCase20 = assertEqual "partition"
    (take 20 partition)
    [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490]

-- | Copied from @Math.NumberTheory.Recurrences.Pentagonal@ to test the
-- reference implementation of @partition@.
pentagonalSigns :: Num a => [a] -> [a]
pentagonalSigns = zipWith (*) (cycle [1, 1, -1, -1])

-- | Copied from @Math.NumberTheory.Recurrences.Pentagonal@ to test the
-- reference implementation of @partition@.
pents :: (Enum a, Num a) => [a]
pents = interleave (scanl (\acc n -> acc + 3 * n - 1) 0 [1..])
                   (scanl (\acc n -> acc + 3 * n - 2) 1 [2..])
  where
    interleave :: [a] -> [a] -> [a]
    interleave (n : ns) (m : ms) = n : m : interleave ns ms
    interleave _ _ = []

-- | Check that @p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-11) + ...@,
-- where @p(x) = 0@ for any negative integer and @p(0) = 1@.
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
    , testSmallAndQuick "mod n" partitionProperty2
    , testCase          "first 20 elements of partition are correct"
                        partitionSpecialCase20
    ]
  , testGroup "Generalized pentagonal numbers"
    [ testSmallAndQuick "matches definition" pentagonalNumbersProperty1
    ]
  ]
