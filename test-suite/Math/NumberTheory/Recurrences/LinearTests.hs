-- |
-- Module:      Math.NumberTheory.Recurrences.LinearTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Recurrences.Linear
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Recurrences.LinearTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Arrow
import Data.List (sort)
import qualified Data.List.Infinite as Inf

import Math.NumberTheory.Primes
import Math.NumberTheory.Recurrences.Linear
import Math.NumberTheory.TestUtils

-- | Check that 'fibonacci' matches the definition of Fibonacci sequence.
fibonacciProperty1 :: AnySign Int -> Bool
fibonacciProperty1 (AnySign n) = fibonacci n + fibonacci (n + 1) == fibonacci (n +2)

-- | Check that 'fibonacci' for negative indices is correctly defined.
fibonacciProperty2 :: Word -> Bool
fibonacciProperty2 n = fibonacci (fromIntegral n) == (if even n then negate else id) (fibonacci (- fromIntegral n))

-- | Check that 'fibonacciPair' is a pair of consequent 'fibonacci'.
fibonacciPairProperty :: AnySign Int -> Bool
fibonacciPairProperty (AnySign n) = fibonacciPair n == (fibonacci n, fibonacci (n + 1))

-- | Check that 'fibonacci 0' is 0.
fibonacciSpecialCase0 :: Assertion
fibonacciSpecialCase0 = assertEqual "fibonacci" (fibonacci 0) 0

-- | Check that 'fibonacci 1' is 1.
fibonacciSpecialCase1 :: Assertion
fibonacciSpecialCase1 = assertEqual "fibonacci" (fibonacci 1) 1


-- | Check that 'lucas' matches the definition of Lucas sequence.
lucasProperty1 :: AnySign Int -> Bool
lucasProperty1 (AnySign n) = lucas n + lucas (n + 1) == lucas (n +2)

-- | Check that 'lucas' for negative indices is correctly defined.
lucasProperty2 :: Word -> Bool
lucasProperty2 n = lucas (fromIntegral n) == (if odd n then negate else id) (lucas (- fromIntegral n))

-- | Check that 'lucasPair' is a pair of consequent 'lucas'.
lucasPairProperty :: AnySign Int -> Bool
lucasPairProperty (AnySign n) = lucasPair n == (lucas n, lucas (n + 1))

-- | Check that 'lucas 0' is 2.
lucasSpecialCase0 :: Assertion
lucasSpecialCase0 = assertEqual "lucas" (lucas 0) 2

-- | Check that 'lucas 1' is 1.
lucasSpecialCase1 :: Assertion
lucasSpecialCase1 = assertEqual "lucas" (lucas 1) 1

-- | Check that 'generalLucas' matches its definition.
generalLucasProperty1 :: AnySign Integer -> AnySign Integer -> Word -> Bool
generalLucasProperty1 (AnySign p) (AnySign q) n = un1 == un1' && vn1 == vn1' && un2 == p * un1 - q * un && vn2 == p * vn1 - q * vn
  where
    (un, un1, vn, vn1) = generalLucas p q (fromIntegral n)
    (un1', un2, vn1', vn2) = generalLucas p q (fromIntegral n + 1)

-- | Check that 'generalLucas' 1 (-1) is 'fibonacciPair' plus 'lucasPair'.
generalLucasProperty2 :: Word -> Bool
generalLucasProperty2 n = (un, un1) == fibonacciPair (fromIntegral n) && (vn, vn1) == lucasPair (fromIntegral n)
  where
    (un, un1, vn, vn1) = generalLucas 1 (-1) (fromIntegral n)

-- | Check that 'generalLucas' p _ 0 is (0, 1, 2, p).
generalLucasProperty3 :: AnySign Integer -> AnySign Integer -> Bool
generalLucasProperty3 (AnySign p) (AnySign q) = generalLucas p q 0 == (0, 1, 2, p)

factorialProperty1 :: Word -> Bool
factorialProperty1 n = n > 100000 ||
  sort (map (first unPrime) (factorise (factorial Inf.!! n))) ==
    sort (map (first (toInteger . unPrime)) (factorialFactors n))

factorialProperty2 :: Word -> Bool
factorialProperty2 n = n > 100000 ||
  factorial Inf.!! n ==
    product (map (\(p, k) -> toInteger (unPrime p) ^ k) (factorialFactors n))

testSuite :: TestTree
testSuite = testGroup "Linear"
  [ testGroup "fibonacci"
    [ testSmallAndQuick "matches definition"  fibonacciProperty1
    , testSmallAndQuick "negative indices"    fibonacciProperty2
    , testSmallAndQuick "pair"                fibonacciPairProperty
    , testCase          "fibonacci 0"         fibonacciSpecialCase0
    , testCase          "fibonacci 1"         fibonacciSpecialCase1
    ]
  , testGroup "lucas"
    [ testSmallAndQuick "matches definition"  lucasProperty1
    , testSmallAndQuick "negative indices"    lucasProperty2
    , testSmallAndQuick "pair"                lucasPairProperty
    , testCase          "lucas 0"             lucasSpecialCase0
    , testCase          "lucas 1"             lucasSpecialCase1
    ]
  , testGroup "generalLucas"
    [ testSmallAndQuick "matches definition"  generalLucasProperty1
    , testSmallAndQuick "generalLucas 1 (-1)" generalLucasProperty2
    , testSmallAndQuick "generalLucas _ _ 0"  generalLucasProperty3
    ]
  , testGroup "factorial"
    [ testSmallAndQuick "factorise . factorial = factorialFactors"  factorialProperty1
    , testSmallAndQuick "factorial = factorBack . factorialFactors" factorialProperty2
    ]
  ]
