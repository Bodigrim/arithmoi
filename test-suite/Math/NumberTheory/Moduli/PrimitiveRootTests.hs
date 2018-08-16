-- |
-- Module:      Math.NumberTheory.Moduli.PrimitiveRootTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Moduli.PrimitiveRoot
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.PrimitiveRootTests
  ( testSuite
  ) where

import Test.Tasty

import qualified Data.Set as S
import Data.List (genericTake, genericLength)
import Data.Maybe
import Control.Arrow (first)
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions (totient)
import qualified Math.NumberTheory.GCD as GCD
import Math.NumberTheory.Moduli.Class (Mod, SomeMod(..), modulo)
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Prefactored (fromFactors, prefFactors, prefValue, Prefactored)
import Math.NumberTheory.TestUtils
import Math.NumberTheory.UniqueFactorisation

cyclicGroupProperty1 :: (Integral a, UniqueFactorisation a, Show a) => AnySign a -> Bool
cyclicGroupProperty1 (AnySign n) = case cyclicGroupFromModulo n of
  Nothing -> True
  Just cg -> prefValue (cyclicGroupToModulo cg) == n

-- | Multiplicative groups modulo primes are always cyclic.
cyclicGroupProperty2 :: (Integral a, UniqueFactorisation a) => Positive a -> Bool
cyclicGroupProperty2 (Positive n) = case isPrime n of
  Nothing -> True
  Just _  -> isJust (cyclicGroupFromModulo n)

-- | Multiplicative groups modulo double primes are always cyclic.
cyclicGroupProperty3 :: (Integral a, UniqueFactorisation a) => Positive a -> Bool
cyclicGroupProperty3 (Positive n) = case isPrime n of
  Nothing -> True
  Just _  -> 2 * n < n {- overflow check -}
          || isJust (cyclicGroupFromModulo n)

allUnique :: Ord a => [a] -> Bool
allUnique = go S.empty
  where
    go _ []         = True
    go acc (x : xs) = if x `S.member` acc then False else go (S.insert x acc) xs

isPrimitiveRoot'Property1
  :: (Eq a, Integral a, UniqueFactorisation a)
  => AnySign a -> CyclicGroup a -> Bool
isPrimitiveRoot'Property1 (AnySign n) cg
  = gcd (toInteger n) (prefValue (castPrefactored (cyclicGroupToModulo cg))) == 1
  || not (isPrimitiveRoot' cg n)

castPrefactored :: Integral a => Prefactored a -> Prefactored Integer
castPrefactored = fromFactors . GCD.splitIntoCoprimes . map (first toInteger) . GCD.toList . prefFactors

isPrimitiveRootProperty1 :: AnySign Integer -> Positive Natural -> Bool
isPrimitiveRootProperty1 (AnySign n) (Positive m)
  = case n `modulo` m of
    SomeMod n' -> gcd n (toInteger m) == 1
               || not (isPrimitiveRoot n')
    InfMod{}   -> False

isPrimitiveRootProperty2 :: Positive Natural -> Bool
isPrimitiveRootProperty2 (Positive m)
  = isNothing (cyclicGroupFromModulo m)
  || case 0 `modulo` m of
    SomeMod (_ :: Mod t) -> any isPrimitiveRoot [(minBound :: Mod t) .. maxBound]
    InfMod{}             -> False

isPrimitiveRootProperty3 :: AnySign Integer -> Positive Natural -> Bool
isPrimitiveRootProperty3 (AnySign n) (Positive m)
  = case n `modulo` m of
    SomeMod n' -> not (isPrimitiveRoot n')
               || allUnique (genericTake (totient m - 1) (iterate (* n') 1))
    InfMod{}   -> False

isPrimitiveRootProperty4 :: AnySign Integer -> Positive Natural -> Bool
isPrimitiveRootProperty4 (AnySign n) (Positive m)
  = isJust (cyclicGroupFromModulo m)
  || case n `modulo` m of
    SomeMod n' -> not (isPrimitiveRoot n')
    InfMod{}   -> False

isPrimitiveRootProperty5 :: Positive Natural -> Bool
isPrimitiveRootProperty5 (Positive m)
  = isNothing (cyclicGroupFromModulo m)
  || case 0 `modulo` m of
       SomeMod (_ :: Mod t) -> genericLength (filter isPrimitiveRoot [(minBound :: Mod t) .. maxBound]) == totient (totient m)
       InfMod{}             -> False

testSuite :: TestTree
testSuite = testGroup "Primitive root"
  [ testGroup "CyclicGroup"
    [ testIntegralProperty "cyclicGroupToModulo . cyclicGroupFromModulo" cyclicGroupProperty1
    , testIntegralProperty "cyclic group mod p" cyclicGroupProperty2
    , testIntegralProperty "cyclic group mod 2p" cyclicGroupProperty3
    ]
  , testGroup "isPrimitiveRoot'"
    [ testGroup "primitive root is coprime with modulo"
      [ testSmallAndQuick "Integer" (isPrimitiveRoot'Property1 :: AnySign Integer -> CyclicGroup Integer -> Bool)
      , testSmallAndQuick "Natural" (isPrimitiveRoot'Property1 :: AnySign Natural -> CyclicGroup Natural -> Bool)
      , testSmallAndQuick "Int"     (isPrimitiveRoot'Property1 :: AnySign Int     -> CyclicGroup Int     -> Bool) -- dubious test
      , testSmallAndQuick "Word"    (isPrimitiveRoot'Property1 :: AnySign Word    -> CyclicGroup Word    -> Bool) -- dubious test
      ]
    ]
  , testGroup "isPrimitiveRoot"
    [ testSmallAndQuick "primitive root is coprime with modulo"            isPrimitiveRootProperty1
    , testSmallAndQuick "cyclic group has a primitive root"                isPrimitiveRootProperty2
    , testSmallAndQuick "primitive root generates cyclic group"            isPrimitiveRootProperty3
    , testSmallAndQuick "no primitive root in non-cyclic group"            isPrimitiveRootProperty4
    , testSmallAndQuick "cyclic group has right number of primitive roots" isPrimitiveRootProperty5
    ]
  ]
