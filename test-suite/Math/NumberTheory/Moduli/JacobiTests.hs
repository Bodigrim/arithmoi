-- |
-- Module:      Math.NumberTheory.Moduli.JacobiTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Moduli.Jacobi
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.JacobiTests
  ( testSuite
  ) where

import Test.Tasty

import Data.Bits
import Data.Functor.Compose
import Data.Semigroup

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 2
jacobiProperty2 :: (Integral a, Bits a) => AnySign a -> (Compose Positive Odd) a -> Bool
jacobiProperty2 (AnySign a) (Compose (Positive (Odd n)))
  =  a + n < a -- check overflow
  || jacobi a n == jacobi (a + n) n

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 3
jacobiProperty3 :: (Integral a, Bits a) => AnySign a -> (Compose Positive Odd) a -> Bool
jacobiProperty3 (AnySign a) (Compose (Positive (Odd n))) = case jacobi a n of
  MinusOne -> a `gcd` n == 1
  Zero     -> a `gcd` n /= 1
  One      -> a `gcd` n == 1

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 4
jacobiProperty4 :: (Integral a, Bits a) => AnySign a -> AnySign a -> (Compose Positive Odd) a -> Bool
jacobiProperty4 (AnySign a) (AnySign b) (Compose (Positive (Odd n))) = jacobi (a * b) n == jacobi a n <> jacobi b n

jacobiProperty4_Integer :: AnySign Integer -> AnySign Integer -> (Compose Positive Odd) Integer -> Bool
jacobiProperty4_Integer = jacobiProperty4

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 5
jacobiProperty5 :: (Integral a, Bits a) => AnySign a -> (Compose Positive Odd) a -> (Compose Positive Odd) a -> Bool
jacobiProperty5 (AnySign a) (Compose (Positive (Odd m))) (Compose (Positive (Odd n))) = jacobi a (m * n) == jacobi a m <> jacobi a n

jacobiProperty5_Integer :: AnySign Integer -> (Compose Positive Odd) Integer -> (Compose Positive Odd) Integer -> Bool
jacobiProperty5_Integer = jacobiProperty5

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 6
jacobiProperty6 :: (Integral a, Bits a) => (Compose Positive Odd) a -> (Compose Positive Odd) a -> Bool
jacobiProperty6 (Compose (Positive (Odd m))) (Compose (Positive (Odd n))) = gcd m n /= 1 || jacobi m n <> jacobi n m == (if m `mod` 4 == 1 || n `mod` 4 == 1 then One else MinusOne)

testSuite :: TestTree
testSuite = testGroup "Jacobi"
  [ testSameIntegralProperty "same modulo n"                jacobiProperty2
  , testSameIntegralProperty "consistent with gcd"          jacobiProperty3
  , testSmallAndQuick        "multiplicative 1"             jacobiProperty4_Integer
  , testSmallAndQuick        "multiplicative 2"             jacobiProperty5_Integer
  , testSameIntegralProperty "law of quadratic reciprocity" jacobiProperty6
  ]
