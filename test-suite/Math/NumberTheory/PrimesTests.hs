-- |
-- Module:      Math.NumberTheory.PrimesTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Primes
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.PrimesTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

primesSumWonk :: Int -> Int
primesSumWonk upto = sum . takeWhile (< upto) . map fromInteger . primeList $ primeSieve (toInteger upto)

primesSum :: Int -> Int
primesSum upto = sum . takeWhile (< upto) . map fromInteger $ primes

primesSumProperty :: NonNegative Int -> Bool
primesSumProperty (NonNegative n) = primesSumWonk n == primesSum n


sieveFactorSpecialCase1 :: Assertion
sieveFactorSpecialCase1 = do
    assertEqual "sieveFactor 2048" [(29, 1), (73, 1)] $ sieveFactor (factorSieve 2048) (29*73)
    assertEqual "sieveFactor 15"   [(3, 1),  (5, 2)]  $ sieveFactor (factorSieve 15) (75)

sieveFactorProperty :: Integer -> Bool
sieveFactorProperty n = (n==0) || factorise n == sieveFactor (factorSieve 25) n

testSuite :: TestTree
testSuite = testGroup "Primes"
  [ testSmallAndQuick "primesSum"   primesSumProperty
  , testCase          "sieveFactor special"  sieveFactorSpecialCase1
  , testSmallAndQuick "sieveFactor property" sieveFactorProperty
  ]
