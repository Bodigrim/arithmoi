-- |
-- Module:      Math.NumberTheory.Primes.TestingTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Primes.Testing
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.TestingTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Integer.GMP.Internals (nextPrimeInteger)

import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.TestUtils

isPrimeProperty1 :: Assertion
isPrimeProperty1 = assertEqual "[0..100]" expected actual
  where
    expected = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
    actual   = filter isPrime [0..100]

isPrimeProperty2 :: Integer -> Bool
isPrimeProperty2 n = isPrime n == isPrime (negate n)

isPrimeProperty3 :: Assertion
isPrimeProperty3 = assertBool "Carmichael pseudoprimes" $ all (not . isPrime) pseudoprimes
  where
    -- OEIS A002997
    pseudoprimes = [561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745, 63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821, 334153, 340561, 399001, 410041, 449065, 488881, 512461]

isPrimeProperty4 :: Assertion
isPrimeProperty4 = assertBool "strong pseudoprimes to base 2" $ all (not . isPrime) pseudoprimes
  where
    -- OEIS A001262
    pseudoprimes = [2047, 3277, 4033, 4681, 8321, 15841, 29341, 42799, 49141, 52633, 65281, 74665, 80581, 85489, 88357, 90751, 104653, 130561, 196093, 220729, 233017, 252601, 253241, 256999, 271951, 280601, 314821, 357761, 390937, 458989, 476971, 486737]

isPrimeProperty5 :: Assertion
isPrimeProperty5 = assertBool "strong Lucas pseudoprimes" $ all (not . isPrime) pseudoprimes
  where
    -- OEIS A217255
    pseudoprimes = [5459, 5777, 10877, 16109, 18971, 22499, 24569, 25199, 40309, 58519, 75077, 97439, 100127, 113573, 115639, 130139, 155819, 158399, 161027, 162133, 176399, 176471, 189419, 192509, 197801, 224369, 230691, 231703, 243629, 253259, 268349, 288919, 313499, 324899]

isPrimeProperty6 :: NonNegative Integer -> Bool
isPrimeProperty6 (NonNegative n) = if isPrime n
  then nextPrimeInteger (n - 1) == n
  else isPrime (nextPrimeInteger n)

isStrongFermatPPProperty :: NonNegative Integer -> Integer -> Bool
isStrongFermatPPProperty (NonNegative n) b = not (isPrime n) || isStrongFermatPP n b

testSuite :: TestTree
testSuite = testGroup "Testing"
  [ testGroup "isPrime"
    [ testCase          "[0..100]"                   isPrimeProperty1
    , testSmallAndQuick "negate"                     isPrimeProperty2
    , testCase          "Carmichael pseudoprimes"    isPrimeProperty3
    , testCase          "strong pseudoprimes base 2" isPrimeProperty4
    , testCase          "strong Lucas pseudoprimes"  isPrimeProperty5
    , testSmallAndQuick "matches GMP"                isPrimeProperty6
    ]
  , testGroup "isStrongFermatPP"
    [ testSmallAndQuick "matches isPrime" isStrongFermatPPProperty
    ]
  ]
