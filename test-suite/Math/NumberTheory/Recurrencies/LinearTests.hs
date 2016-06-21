-- |
-- Module:      Math.NumberTheory.Recurrencies.LinearTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Recurrencies.Linear
--

{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Recurrencies.LinearTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Math.NumberTheory.Recurrencies.Linear
import Math.NumberTheory.TestUtils

-- | Check that 'fibonacci' matches the definition of Fibonacci sequence.
fibonacciProperty1 :: AnySign Int -> Bool
fibonacciProperty1 (AnySign n) = fibonacci n + fibonacci (n + 1) == fibonacci (n +2)

-- | Check that 'fibonacci' for negative indices is correctly defined.
fibonacciProperty2 :: NonNegative Int -> Bool
fibonacciProperty2 (NonNegative n) = fibonacci n == (if even n then negate else id) (fibonacci (- n))

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
lucasProperty2 :: NonNegative Int -> Bool
lucasProperty2 (NonNegative n) = lucas n == (if odd n then negate else id) (lucas (- n))

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
generalLucasProperty1 :: AnySign Integer -> AnySign Integer -> NonNegative Int -> Bool
generalLucasProperty1 (AnySign p) (AnySign q) (NonNegative n) = un1 == un1' && vn1 == vn1' && un2 == p * un1 - q * un && vn2 == p * vn1 - q * vn
  where
    (un, un1, vn, vn1) = generalLucas p q n
    (un1', un2, vn1', vn2) = generalLucas p q (n + 1)

-- | Check that 'generalLucas' 1 (-1) is 'fibonacciPair' plus 'lucasPair'.
generalLucasProperty2 :: NonNegative Int -> Bool
generalLucasProperty2 (NonNegative n) = (un, un1) == fibonacciPair n && (vn, vn1) == lucasPair n
  where
    (un, un1, vn, vn1) = generalLucas 1 (-1) n

-- | Check that 'generalLucas' p _ 0 is (0, 1, 2, p).
generalLucasProperty3 :: AnySign Integer -> AnySign Integer -> Bool
generalLucasProperty3 (AnySign p) (AnySign q) = generalLucas p q 0 == (0, 1, 2, p)

testSuite :: TestTree
testSuite = testGroup "Lucas"
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
  ]
