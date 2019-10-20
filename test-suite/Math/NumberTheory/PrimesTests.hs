-- |
-- Module:      Math.NumberTheory.PrimesTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Primes
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.PrimesTests
  ( testSuite
  ) where

import Test.Tasty

import Math.NumberTheory.Primes (primes, unPrime, nextPrime, precPrime)
import Math.NumberTheory.TestUtils

primesSumWonk :: Int -> Int
primesSumWonk upto = sum $ map unPrime [nextPrime 2 .. precPrime upto]

primesSum :: Int -> Int
primesSum upto = sum . takeWhile (<= upto) . map unPrime $ primes

primesSumProperty :: NonNegative Int -> Bool
primesSumProperty (NonNegative n) = n < 2 || primesSumWonk n == primesSum n


testSuite :: TestTree
testSuite = testGroup "Primes"
  [ testSmallAndQuick "primesSum"   primesSumProperty
  ]
