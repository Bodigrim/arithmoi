-- |
-- Module:      Math.NumberTheory.PrimesTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Primes
--

{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.PrimesTests
  ( testSuite
  ) where

import Test.Tasty

import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

primesSumWonk :: Int -> Int
primesSumWonk upto = sum . takeWhile (< upto) . map unPrime . primeList $ primeSieve (toInteger upto)

primesSum :: Int -> Int
primesSum upto = sum . takeWhile (< upto) . map unPrime $ primes

primesSumProperty :: NonNegative Int -> Bool
primesSumProperty (NonNegative n) = primesSumWonk n == primesSum n


testSuite :: TestTree
testSuite = testGroup "Primes"
  [ testSmallAndQuick "primesSum"   primesSumProperty
  ]
