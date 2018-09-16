-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--
-- Tests for Math.NumberTheory.DirichletCharacters
--

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.DirichletCharactersTests where

import Test.Tasty

import Data.Proxy
import Numeric.Natural

import Data.List (sort)
import GHC.TypeNats.Compat (SomeNat(..), someNatVal)

import Math.NumberTheory.DirichletCharacters (generators)
import Math.NumberTheory.Moduli (Mod, getNatVal)
import Math.NumberTheory.TestUtils (testSmallAndQuick, Positive(..))

generatingTest :: Positive Natural -> Bool
generatingTest (Positive 1) = [1] == generators 1
generatingTest (Positive n) =
  case someNatVal n of
    SomeNat (_ :: Proxy m) -> [a | a <- [1..n], gcd a n == 1] == generated
      where generated = sort $ map (getNatVal . product) $ traverse helper [fromIntegral g :: Mod m | g <- generators n]

helper :: (Eq a, Num a) => a -> [a]
helper m = 1: (takeWhile (/= 1) $ iterate (*m) m)

testSuite :: TestTree
testSuite = testGroup "DirichletCharacters"
  [ testSmallAndQuick "check generators work" generatingTest
  ]
