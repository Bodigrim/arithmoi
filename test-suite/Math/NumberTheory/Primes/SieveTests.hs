-- |
-- Module:      Math.NumberTheory.Primes.SieveTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Primes.Sieve
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-deprecations  #-}

module Math.NumberTheory.Primes.SieveTests
  ( testSuite
  ) where

import Prelude hiding (words)

import Test.Tasty
import Test.Tasty.HUnit

import Math.NumberTheory.Primes.Sieve
import qualified Math.NumberTheory.Primes.Heap as H
import Math.NumberTheory.TestUtils

-- | Check that both 'primes' produce the same.
primesProperty1 :: Assertion
primesProperty1 = do
  assertEqual "Sieve == Heap" (trim primes) (trim H.primes)
  where
    trim = take 100000

-- | Check that both 'sieveFrom' produce the same.
sieveFromProperty1 :: AnySign Integer -> Bool
sieveFromProperty1 (AnySign lowBound)
  = trim (sieveFrom lowBound) == trim (H.sieveFrom lowBound)
  where
    trim = take 1000

-- | Check that 'primeList' from 'primeSieve' matches truncated 'primes'.
primeSieveProperty1 :: AnySign Integer -> Bool
primeSieveProperty1 (AnySign highBound)
  = primeList (primeSieve highBound) == takeWhile (<= (highBound `max` 7)) primes

-- | Check that 'primeList' from 'psieveList' matches 'primes'.
psieveListProperty1 :: Assertion
psieveListProperty1 = do
  assertEqual "primes == primeList . psieveList" (trim primes) (trim $ concatMap primeList psieveList)
  where
    trim = take 100000

-- | Check that 'primeList' from 'psieveFrom' matches 'sieveFrom'.
psieveFromProperty1 :: AnySign Integer -> Bool
psieveFromProperty1 (AnySign lowBound)
  = trim (sieveFrom lowBound) == trim (filter (>= lowBound) (concatMap primeList $ psieveFrom lowBound))
  where
    trim = take 1000


testSuite :: TestTree
testSuite = testGroup "Sieve"
  [ testCase          "primes"     primesProperty1
  , testSmallAndQuick "sieveFrom"  sieveFromProperty1
  , testSmallAndQuick "primeSieve" primeSieveProperty1
  , testCase          "psieveList" psieveListProperty1
  , testSmallAndQuick "psieveFrom" psieveFromProperty1
  ]
