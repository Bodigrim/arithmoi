-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.InverseTests
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.ArithmeticFunctions.Inverse
--

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctions.InverseTests
  ( testSuite
  ) where

import Test.Tasty
import Data.Semigroup
import Data.Semiring (Semiring(..))
import qualified Data.Set as S
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.ArithmeticFunctions.Inverse
import Math.NumberTheory.Euclidean
import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

totientProperty1 :: forall a. (Semiring a, Euclidean a, UniqueFactorisation a, Ord a) => Positive a -> Bool
totientProperty1 (Positive x) = Product x `S.member` (inverseTotient (S.singleton . Product) (totient x))

totientProperty2 :: (Semiring a, Euclidean a, UniqueFactorisation a, Ord a) => Positive a -> Bool
totientProperty2 (Positive x) = all (== x) (S.map (totient . getProduct) (inverseTotient (S.singleton . Product) x))

testSuite :: TestTree
testSuite = testGroup "Inverse"
  [ testGroup "Totient"
    [ testGroup "forward"
      [ testSmallAndQuick "Int"     (totientProperty1 :: Positive Int     -> Bool)
      , testSmallAndQuick "Word"    (totientProperty1 :: Positive Word    -> Bool)
      , testSmallAndQuick "Integer" (totientProperty1 :: Positive Integer -> Bool)
      , testSmallAndQuick "Natural" (totientProperty1 :: Positive Natural -> Bool)
      ]
    , testGroup "backward"
      [ testSmallAndQuick "Int"     (totientProperty2 :: Positive Int     -> Bool)
      , testSmallAndQuick "Word"    (totientProperty2 :: Positive Word    -> Bool)
      , testSmallAndQuick "Integer" (totientProperty2 :: Positive Integer -> Bool)
      , testSmallAndQuick "Natural" (totientProperty2 :: Positive Natural -> Bool)
      ]
    ]
  ]
