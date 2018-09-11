-- |
-- Module:      Math.NumberTheory.Primes.SieveTests
-- Copyright:   (c) 2016-2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Primes.Sieve
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-deprecations  #-}

module Math.NumberTheory.Primes.SieveTests
  ( testSuite
  ) where

import Prelude hiding (words)

import Test.Tasty
import Test.Tasty.HUnit

import Data.Int
import Data.Proxy (Proxy(..))
import Data.Word
import Numeric.Natural (Natural)

import Math.NumberTheory.Primes (Prime, unPrime)
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
primesProperty1 :: forall a. (Integral a, Show a) => Proxy a -> Assertion
primesProperty1 _ = assertEqual "primes matches isPrime"
  (takeWhile (<= lim1) (map unPrime primes) :: [a])
  (filter (isPrime . toInteger) [1..lim1])

primesProperty2 :: forall a. (Integral a, Bounded a, Show a) => Proxy a -> Assertion
primesProperty2 _ = assertEqual "primes matches isPrime"
  (map unPrime primes :: [a])
  (filter (isPrime . toInteger) [1..maxBound])

-- | Check that 'primeList' from 'primeSieve' matches truncated 'primes'.
primeSieveProperty1 :: AnySign Integer -> Bool
primeSieveProperty1 (AnySign highBound')
  =  primeList (primeSieve highBound)
  == takeWhile ((<= (highBound `max` 7)) . unPrime) primes
  where
    highBound = highBound' `rem` lim1

-- | Check that 'primeList' from 'psieveList' matches 'primes'.
psieveListProperty1 :: forall a. (Integral a, Show a) => Proxy a -> Assertion
psieveListProperty1 _ = assertEqual "primes == primeList . psieveList"
  (take lim2 primes :: [Prime a])
  (take lim2 $ concatMap primeList psieveList)

psieveListProperty2 :: forall a. (Integral a, Show a) => Proxy a -> Assertion
psieveListProperty2 _ = assertEqual "primes == primeList . psieveList"
  (primes :: [Prime a])
  (concat $ takeWhile (not . null) $ map primeList psieveList)

-- | Check that 'sieveFrom' matches 'primeList' of 'psieveFrom'.
sieveFromProperty1 :: AnySign Integer -> Bool
sieveFromProperty1 (AnySign lowBound')
  =  take lim3 (sieveFrom lowBound)
  == take lim3 (filter ((>= lowBound) . unPrime) (concatMap primeList $ psieveFrom lowBound))
  where
    lowBound = lowBound' `rem` lim1

-- | Check that 'sieveFrom' matches 'isPrime' near 0.
sieveFromProperty2 :: AnySign Integer -> Bool
sieveFromProperty2 (AnySign lowBound')
  =  take lim3 (map unPrime (sieveFrom lowBound))
  == take lim3 (filter (isPrime . toInteger) [lowBound `max` 0 ..])
  where
    lowBound = lowBound' `rem` lim1

testSuite :: TestTree
testSuite = testGroup "Sieve"
  [ testGroup "primes"
    [ testCase "Int"     (primesProperty1 (Proxy :: Proxy Int))
    , testCase "Word"    (primesProperty1 (Proxy :: Proxy Word))
    , testCase "Integer" (primesProperty1 (Proxy :: Proxy Integer))
    , testCase "Natural" (primesProperty1 (Proxy :: Proxy Natural))

    , testCase "Int8"    (primesProperty2 (Proxy :: Proxy Int8))
    , testCase "Int16"   (primesProperty2 (Proxy :: Proxy Int16))
    , testCase "Word8"   (primesProperty2 (Proxy :: Proxy Word8))
    , testCase "Word16"  (primesProperty2 (Proxy :: Proxy Word16))
    ]
  , testSmallAndQuick "primeSieve" primeSieveProperty1
  , testGroup "psieveList"
    [ testCase "Int"     (psieveListProperty1 (Proxy :: Proxy Int))
    , testCase "Word"    (psieveListProperty1 (Proxy :: Proxy Word))
    , testCase "Integer" (psieveListProperty1 (Proxy :: Proxy Integer))
    , testCase "Natural" (psieveListProperty1 (Proxy :: Proxy Natural))

    , testCase "Int8"    (psieveListProperty2 (Proxy :: Proxy Int8))
    , testCase "Int16"   (psieveListProperty2 (Proxy :: Proxy Int16))
    , testCase "Word8"   (psieveListProperty2 (Proxy :: Proxy Word8))
    , testCase "Word16"  (psieveListProperty2 (Proxy :: Proxy Word16))
    ]
  , testGroup "sieveFrom"
    [ testSmallAndQuick "psieveFrom"     sieveFromProperty1
    , testSmallAndQuick "isPrime near 0" sieveFromProperty2
    ]
  ]
