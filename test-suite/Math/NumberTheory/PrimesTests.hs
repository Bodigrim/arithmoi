-- |
-- Module:      Math.NumberTheory.PrimesTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Primes
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.PrimesTests
  ( testSuite
  ) where

import Test.Tasty

import Data.Bits
import Data.Int
import Data.Proxy
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif

import Math.NumberTheory.Primes
import qualified Math.NumberTheory.Primes.IntSet as PS
import Math.NumberTheory.TestUtils

primesSumWonk :: Int -> Int
primesSumWonk upto = sum $ map unPrime [nextPrime 2 .. precPrime upto]

primesSum :: Int -> Int
primesSum upto = sum . takeWhile (<= upto) . map unPrime $ primes

primesSumProperty :: NonNegative Int -> Bool
primesSumProperty (NonNegative n) = n < 2 || primesSumWonk n == primesSum n

symmetricDifferenceProperty :: [Prime Int] -> [Prime Int] -> Bool
symmetricDifferenceProperty xs ys = z1 == z2
  where
    x = PS.fromList xs
    y = PS.fromList ys
    z1 = (x PS.\\ PS.unPrimeIntSet y) <> (y PS.\\ PS.unPrimeIntSet x)
    z2 = PS.symmetricDifference x y

toPrimeIntegralTest :: forall a b. (Bits a, Integral a, Bits b, Integral b) => Proxy a -> Prime b -> Bool
toPrimeIntegralTest _ p =
  toIntegralSized (unPrime p) == (fmap unPrime (toPrimeIntegral p) :: Maybe a)

testSuite :: TestTree
testSuite = testGroup "Primes"
  [ testSmallAndQuick "primesSum"   primesSumProperty
  , testSmallAndQuick "symmetricDifference" symmetricDifferenceProperty
  , testGroup "toPrimeIntegral"
    [ testSmallAndQuick "Int -> Integer" $ toPrimeIntegralTest @Integer @Int     Proxy
    , testSmallAndQuick "Int -> Int8"    $ toPrimeIntegralTest @Int8    @Int     Proxy
    , testSmallAndQuick "Integer -> Int" $ toPrimeIntegralTest @Int     @Integer Proxy
    ]
  ]
