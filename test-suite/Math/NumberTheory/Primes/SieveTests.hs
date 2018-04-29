-- |
-- Module:      Math.NumberTheory.Primes.SieveTests
-- Copyright:   (c) 2016-2018 Andrew Lelechenko
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
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.TestUtils

lim1 :: Num a => a
lim1 = 1000000

lim2 :: Num a => a
lim2 = 100000

lim3 :: Num a => a
lim3 = 1000

-- | Check that 'primes' matches 'isPrime'.
primesProperty1 :: Assertion
primesProperty1 = assertEqual "primes matches isPrime"
  (takeWhile (<= lim1) primes)
  (filter (isPrime . toInteger) [1..lim1])

-- | Check that 'primeList' from 'primeSieve' matches truncated 'primes'.
primeSieveProperty1 :: AnySign Integer -> Bool
primeSieveProperty1 (AnySign highBound')
  =  primeList (primeSieve highBound)
  == takeWhile (<= (highBound `max` 7)) primes
  where
    highBound = highBound' `rem` lim1

-- | Check that 'primeList' from 'psieveList' matches 'primes'.
psieveListProperty1 :: Assertion
psieveListProperty1 = assertEqual "primes == primeList . psieveList"
  (take lim2 primes)
  (take lim2 $ concatMap primeList psieveList)

-- | Check that 'sieveFrom' matches 'primeList' of 'psieveFrom'.
sieveFromProperty1 :: AnySign Integer -> Bool
sieveFromProperty1 (AnySign lowBound')
  =  take lim3 (sieveFrom lowBound)
  == take lim3 (filter (>= lowBound) (concatMap primeList $ psieveFrom lowBound))
  where
    lowBound = lowBound' `rem` lim1

-- | Check that 'sieveFrom' matches 'isPrime' near 0.
sieveFromProperty2 :: AnySign Integer -> Bool
sieveFromProperty2 (AnySign lowBound')
  =  take lim3 (sieveFrom lowBound)
  == take lim3 (filter (isPrime . toInteger) [lowBound `max` 0 ..])
  where
    lowBound = lowBound' `rem` lim1

testSuite :: TestTree
testSuite = testGroup "Sieve"
  [ testCase "primes" primesProperty1
  , testSmallAndQuick "primeSieve" primeSieveProperty1
  , testCase "psieveList" psieveListProperty1
  , testGroup "sieveFrom"
    [ testSmallAndQuick "psieveFrom"     sieveFromProperty1
    , testSmallAndQuick "isPrime near 0" sieveFromProperty2
    ]
  ]
