-- |
-- Module:      Math.NumberTheory.Primes.CountingTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Primes.Counting
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.CountingTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Math.NumberTheory.Primes (unPrime)
import Math.NumberTheory.Primes.Counting
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.TestUtils

-- | https://en.wikipedia.org/wiki/Prime-counting_function#Table_of_.CF.80.28x.29.2C_x_.2F_ln_x.2C_and_li.28x.29
table :: [(Integer, Integer)]
table =
  [ (10^1,   4)
  , (10^2,   25)
  , (10^3,   168)
  , (10^4,   1229)
  , (10^5,   9592)
  , (10^6,   78498)
  , (10^7,   664579)
  , (10^8,   5761455)
  , (10^9,   50847534)
  , (10^10,  455052511)
  , (10^11,  4118054813)
  , (10^12,  37607912018)
  -- , (10^13,  346065536839)
  -- , (10^14,  3204941750802)
  -- , (10^15,  29844570422669)
  -- , (10^16,  279238341033925)
  -- , (10^17,  2623557157654233)
  -- , (10^18,  24739954287740860)
  -- , (10^19,  234057667276344607)
  -- , (10^20,  2220819602560918840)
  ]

-- | Check that values of 'primeCount' are non-negative.
primeCountProperty1 :: Integer -> Bool
primeCountProperty1 n = n > primeCountMaxArg
  || n >  0 && primeCount n >= 0
  || n <= 0 && primeCount n == 0

-- | Check that 'primeCount' is monotonically increasing function.
primeCountProperty2 :: Positive Integer -> Positive Integer -> Bool
primeCountProperty2 (Positive n1) (Positive n2)
  =  n1 > primeCountMaxArg
  || n2 > primeCountMaxArg
  || n1 <= n2 && p1 <= p2
  || n1 >  n2 && p1 >= p2
  where
    p1 = primeCount n1
    p2 = primeCount n2

-- | Check that 'primeCount' is strictly increasing iff an argument is prime.
primeCountProperty3 :: Positive Integer -> Bool
primeCountProperty3 (Positive n)
  =  isPrime n && primeCount (n - 1) + 1 == primeCount n
  || not (isPrime n) && primeCount (n - 1) == primeCount n

-- | Check tabulated values.
primeCountSpecialCases :: [Assertion]
primeCountSpecialCases = map a table
  where
    a (n, m) = assertEqual "primeCount" m (primeCount n)


-- | Check that values of 'nthPrime' are positive.
nthPrimeProperty1 :: Positive Integer -> Bool
nthPrimeProperty1 (Positive n) = n > nthPrimeMaxArg
  || unPrime (nthPrime n) > 0

-- | Check that 'nthPrime' is monotonically increasing function.
nthPrimeProperty2 :: Positive Integer -> Positive Integer -> Bool
nthPrimeProperty2 (Positive n1) (Positive n2)
  =  n1 > nthPrimeMaxArg
  || n2 > nthPrimeMaxArg
  || n1 <= n2 && p1 <= p2
  || n1 >  n2 && p1 >= p2
  where
    p1 = nthPrime n1
    p2 = nthPrime n2

-- | Check that values of 'nthPrime' are prime.
nthPrimeProperty3 :: Positive Integer -> Bool
nthPrimeProperty3 (Positive n) = isPrime $ unPrime $ nthPrime n

-- | Check tabulated values.
nthPrimeSpecialCases :: [Assertion]
nthPrimeSpecialCases = map a table
  where
  a (n, m) = assertBool "nthPrime" $ n > unPrime (nthPrime m)


-- | Check that values of 'approxPrimeCount' are non-negative.
approxPrimeCountProperty1 :: Integral a => AnySign a -> Bool
approxPrimeCountProperty1 (AnySign a) = approxPrimeCount a >= 0

-- | Check that 'approxPrimeCount' is consistent with 'approxPrimeCountOverestimateLimit'.
approxPrimeCountProperty2 :: Integral a => Positive a -> Bool
approxPrimeCountProperty2 (Positive a) = a >= approxPrimeCountOverestimateLimit
  || toInteger (approxPrimeCount a) >= primeCount (toInteger a)


-- | Check that values of 'nthPrimeApprox' are positive.
nthPrimeApproxProperty1 :: AnySign Integer -> Bool
nthPrimeApproxProperty1 (AnySign a) = nthPrimeApprox a > 0

-- | Check that 'nthPrimeApprox' is consistent with 'nthPrimeApproxUnderestimateLimit'.
nthPrimeApproxProperty2 :: Positive Integer -> Bool
nthPrimeApproxProperty2 (Positive a) = a >= nthPrimeApproxUnderestimateLimit
  || toInteger (nthPrimeApprox a) <= unPrime (nthPrime (toInteger a))


testSuite :: TestTree
testSuite = testGroup "Counting"
  [ testGroup "primeCount"
    ( testSmallAndQuick "non-negative"        primeCountProperty1
    : testSmallAndQuick "monotonic"           primeCountProperty2
    : testSmallAndQuick "increases on primes" primeCountProperty3
    : zipWith (\i a -> testCase ("special case " ++ show i) a) [1..] primeCountSpecialCases
    )
  , testGroup "nthPrime"
    ( testSmallAndQuick "positive"  nthPrimeProperty1
    : testSmallAndQuick "monotonic" nthPrimeProperty2
    : testSmallAndQuick "is prime"  nthPrimeProperty3
    : zipWith (\i a -> testCase ("special case " ++ show i) a) [1..] nthPrimeSpecialCases
    )
  , testGroup "approxPrimeCount"
    [ testIntegralProperty "non-negative"             approxPrimeCountProperty1
    , testIntegralProperty "overestimates primeCount" approxPrimeCountProperty2
    ]
  , testGroup "nthPrimeApprox"
    [ testSmallAndQuick "positive"                nthPrimeApproxProperty1
    , testSmallAndQuick "underestimates nthPrime" nthPrimeApproxProperty2
    ]
  ]
