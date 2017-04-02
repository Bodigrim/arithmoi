-- |
-- Module:      Math.NumberTheory.Primes.FactorisationTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Primes.Factorisation
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.FactorisationTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.TestUtils

factoriseProperty1 :: Assertion
factoriseProperty1 = assertEqual "0" [] (factorise 1)

factoriseProperty2 :: Positive Integer -> Bool
factoriseProperty2 (Positive n) = (-1, 1) : factorise n == factorise (negate n)

factoriseProperty3 :: Positive Integer -> Bool
factoriseProperty3 (Positive n) = all (isPrime . fst) (factorise n)

factoriseProperty4 :: Positive Integer -> Bool
factoriseProperty4 (Positive n) = product (map (uncurry (^)) (factorise n)) == n

testSuite :: TestTree
testSuite = testGroup "Factorisation"
  [ testGroup "factorise"
    [ testCase          "0"                factoriseProperty1
    , testSmallAndQuick "negate"                  factoriseProperty2
    , testSmallAndQuick          "bases are prime" factoriseProperty3
    , testSmallAndQuick          "factorback" factoriseProperty4
    ]
  ]
