-- |
-- Module:      Math.NumberTheory.Primes.SieveTests
-- Copyright:   (c) 2016-2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Primes.Sieve
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.SieveTests
  ( testSuite
  ) where

import Prelude hiding (words)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Bits
import Data.Int
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Word
import Numeric.Natural (Natural)

import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

lim1 :: Num a => a
lim1 = 1000000

lim2 :: Num a => a
lim2 = 100000

-- | Check that 'primes' matches 'isPrime'.
primesProperty1 :: forall a. (Integral a, Show a) => Proxy a -> Assertion
primesProperty1 _ = assertEqual "primes matches isPrime"
  (takeWhile (<= lim1) (map unPrime primes) :: [a])
  (filter (isJust . isPrime . toInteger) [1..lim1])

primesProperty2 :: forall a. (Integral a, Bounded a, Show a) => Proxy a -> Assertion
primesProperty2 _ = assertEqual "primes matches isPrime"
  (map unPrime primes :: [a])
  (filter (isJust . isPrime . toInteger) [1..maxBound])

atkinPrimesProperty1 :: Large Word -> Large Word -> Property
atkinPrimesProperty1 (Large x) (Large y) = actual === expected
  where
    lim = 1000000
    from = fromIntegral $ x `mod` lim
    to   = fromIntegral $ x `mod` lim + y `mod` lim
    expected = mapMaybe isPrime [from..to]
    actual   = atkinFromTo from to

-- | Check that 'primeList' from 'primeSieve' matches truncated 'primes'.
primeSieveProperty1 :: AnySign Integer -> Bool
primeSieveProperty1 (AnySign highBound')
  =  [nextPrime 2 .. precPrime highBound]
  == takeWhile (\p -> unPrime p <= highBound) primes
  where
    highBound = max 2 (highBound' `rem` lim1)

-- | Check that 'primeList' from 'psieveList' matches 'primes'.
psieveListProperty1 :: forall a. (Integral a, Show a, Enum (Prime a), Bits a, UniqueFactorisation a) => Proxy a -> Assertion
psieveListProperty1 _ = assertEqual "primes == primeList . psieveList"
  (take lim2 primes :: [Prime a])
  (take lim2 [nextPrime 1..])

psieveListProperty2 :: forall a. (Integral a, Bounded a, Show a) => Proxy a -> Assertion
psieveListProperty2 _ = assertEqual "primes == primeList . psieveList"
  (map unPrime primes :: [a])
  (filter (isJust . isPrime . toInteger) [0..maxBound])

testSuite :: TestTree
testSuite = testGroup "Sieve"
  [ testProperty "atkinPrimes" atkinPrimesProperty1
  , testGroup "primes"
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
  ]
