-- |
-- Module:      Math.NumberTheory.Primes.HeapTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Primes.Heap
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.HeapTests
  ( testSuite
  ) where

import Prelude hiding (words)

import Test.Tasty
import Test.Tasty.HUnit

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Primes.Heap
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.TestUtils

-- | Check that 'primes' over different integral types matches with 'isPrime'.
primesProperty1 :: Assertion
primesProperty1 = do
  assertEqual "ints  == integers" (trim ints)  (trim integers)
  assertEqual "words == integers" (trim words) (trim integers)
  assertEqual "naive == integers" (trim naive) (trim integers)
  where
    trim :: Integral a => [a] -> [Integer]
    trim = map toInteger . take 100000

    ints     = primes :: [Int]
    words    = primes :: [Word]
    integers = primes :: [Integer]
    naive    = filter isPrime [1..] :: [Integer]

-- | Check that 'sieveFrom' over different integral types matches with 'isPrime'.
sieveFromProperty1 :: NonNegative Integer -> Bool
sieveFromProperty1 (NonNegative lowBound)
  =  trim ints  == trim integers
  && trim words == trim integers
  && trim naive == trim integers
  where
    trim :: Integral a => [a] -> [Integer]
    trim = map toInteger . take 1000

    ints     = sieveFrom (fromInteger lowBound) :: [Int]
    words    = sieveFrom (fromInteger lowBound) :: [Word]
    integers = sieveFrom lowBound               :: [Integer]
    naive    = filter isPrime [lowBound..]      :: [Integer]

testSuite :: TestTree
testSuite = testGroup "Heap"
  [ testCase          "primes"    primesProperty1
  , testSmallAndQuick "sieveFrom" sieveFromProperty1
  ]
