-- |
-- Module:      Math.NumberTheory.Moduli.JacobiTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Moduli.Jacobi
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.JacobiTests
  ( testSuite
  ) where

import Test.Tasty

import Data.Bits
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import Numeric.Natural

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 2
jacobiProperty2 :: (Integral a, Bits a) => AnySign a -> (MyCompose Positive Odd) a -> Bool
jacobiProperty2 (AnySign a) (MyCompose (Positive (Odd n)))
  =  a + n < a -- check overflow
  || jacobi a n == jacobi (a + n) n

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 3
jacobiProperty3 :: (Integral a, Bits a) => AnySign a -> (MyCompose Positive Odd) a -> Bool
jacobiProperty3 (AnySign a) (MyCompose (Positive (Odd n))) = case jacobi a n of
  MinusOne -> a `gcd` n == 1
  Zero     -> a `gcd` n /= 1
  One      -> a `gcd` n == 1

doesProductOverflow :: Integral a => a -> a -> Bool
doesProductOverflow x y = abs (toInteger (x * y)) < abs (toInteger x * toInteger y)

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 4
jacobiProperty4 :: (Integral a, Bits a) => AnySign a -> AnySign a -> (MyCompose Positive Odd) a -> Bool
jacobiProperty4 (AnySign a) (AnySign b) (MyCompose (Positive (Odd n))) =
  doesProductOverflow a b ||
  jacobi (a * b) n == jacobi a n <> jacobi b n

jacobiProperty4_Int :: AnySign Int -> AnySign Int -> (MyCompose Positive Odd) Int -> Bool
jacobiProperty4_Int = jacobiProperty4

jacobiProperty4_Word :: AnySign Word -> AnySign Word -> (MyCompose Positive Odd) Word -> Bool
jacobiProperty4_Word = jacobiProperty4

jacobiProperty4_Integer :: AnySign Integer -> AnySign Integer -> (MyCompose Positive Odd) Integer -> Bool
jacobiProperty4_Integer = jacobiProperty4

jacobiProperty4_Natural :: AnySign Natural -> AnySign Natural -> (MyCompose Positive Odd) Natural -> Bool
jacobiProperty4_Natural = jacobiProperty4

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 5
jacobiProperty5 :: (Integral a, Bits a) => AnySign a -> (MyCompose Positive Odd) a -> (MyCompose Positive Odd) a -> Bool
jacobiProperty5 (AnySign a) (MyCompose (Positive (Odd m))) (MyCompose (Positive (Odd n))) =
  doesProductOverflow m n ||
  jacobi a (m * n) == jacobi a m <> jacobi a n

jacobiProperty5_Int :: AnySign Int -> (MyCompose Positive Odd) Int -> (MyCompose Positive Odd) Int -> Bool
jacobiProperty5_Int = jacobiProperty5

jacobiProperty5_Word :: AnySign Word -> (MyCompose Positive Odd) Word -> (MyCompose Positive Odd) Word -> Bool
jacobiProperty5_Word = jacobiProperty5

jacobiProperty5_Integer :: AnySign Integer -> (MyCompose Positive Odd) Integer -> (MyCompose Positive Odd) Integer -> Bool
jacobiProperty5_Integer = jacobiProperty5

jacobiProperty5_Natural :: AnySign Natural -> (MyCompose Positive Odd) Natural -> (MyCompose Positive Odd) Natural -> Bool
jacobiProperty5_Natural = jacobiProperty5

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 6
jacobiProperty6 :: (Integral a, Bits a) => (MyCompose Positive Odd) a -> (MyCompose Positive Odd) a -> Bool
jacobiProperty6 (MyCompose (Positive (Odd m))) (MyCompose (Positive (Odd n))) = gcd m n /= 1 || jacobi m n <> jacobi n m == (if m `mod` 4 == 1 || n `mod` 4 == 1 then One else MinusOne)

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 7
jacobiProperty7 :: (Integral a, Bits a) => (MyCompose Positive Odd) a -> Bool
jacobiProperty7 (MyCompose (Positive (Odd n))) =
  jacobi (-1) n == if n `mod` 4 == 1 then One else MinusOne

jacobiProperty7_Int :: (MyCompose Positive Odd) Int -> Bool
jacobiProperty7_Int = jacobiProperty7

jacobiProperty7_Integer :: (MyCompose Positive Odd) Integer -> Bool
jacobiProperty7_Integer = jacobiProperty7

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 8
jacobiProperty8 :: (Integral a, Bits a) => (MyCompose Positive Odd) a -> Bool
jacobiProperty8 (MyCompose (Positive (Odd n))) =
  even n ||
  jacobi 2 n == if n `mod` 8 == 1 || n `mod` 8 == 7 then One else MinusOne

jacobiProperty9 :: (Integral a, Bits a, Bounded a) => (MyCompose Positive Odd) a -> Bool
jacobiProperty9 (MyCompose (Positive (Odd n))) =
  jacobi m n == jacobi (toInteger m) (toInteger n)
  where
    m = minBound

jacobiProperty9_Int :: (MyCompose Positive Odd) Int -> Bool
jacobiProperty9_Int = jacobiProperty9

testSuite :: TestTree
testSuite = testGroup "Jacobi"
  [ testSameIntegralProperty "same modulo n"                jacobiProperty2
  , testSameIntegralProperty "consistent with gcd"          jacobiProperty3
  , testSmallAndQuick        "multiplicative 1 Int"         jacobiProperty4_Int
  , testSmallAndQuick        "multiplicative 1 Word"        jacobiProperty4_Word
  , testSmallAndQuick        "multiplicative 1 Integer"     jacobiProperty4_Integer
  , testSmallAndQuick        "multiplicative 1 Natural"     jacobiProperty4_Natural
  , testSmallAndQuick        "multiplicative 2 Int"         jacobiProperty5_Int
  , testSmallAndQuick        "multiplicative 2 Word"        jacobiProperty5_Word
  , testSmallAndQuick        "multiplicative 2 Integer"     jacobiProperty5_Integer
  , testSmallAndQuick        "multiplicative 2 Natural"     jacobiProperty5_Natural
  , testSameIntegralProperty "law of quadratic reciprocity" jacobiProperty6
  , testSmallAndQuick        "-1 Int"                       jacobiProperty7_Int
  , testSmallAndQuick        "-1 Integer"                   jacobiProperty7_Integer
  , testIntegralProperty     "2"                            jacobiProperty8
  , testSmallAndQuick        "minBound Int"                 jacobiProperty9_Int
  ]
