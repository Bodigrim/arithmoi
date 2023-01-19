-- |
-- Module:      Math.NumberTheory.Moduli.PrimitiveRootTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Moduli.PrimitiveRoot
--

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.PrimitiveRootTests
  ( testSuite
  ) where

import Prelude hiding (gcd)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Euclidean
import Data.List (genericTake, genericLength)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Mod
import Data.Proxy
import qualified Data.Set as S
import GHC.TypeNats (SomeNat(..), someNatVal)
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

cyclicGroupProperty1 :: (Euclidean a, Integral a, UniqueFactorisation a) => Positive a -> Bool
cyclicGroupProperty1 (Positive n) = case cyclicGroupFromModulo n of
  Nothing -> True
  Just (Some cg) -> factorBack (unSFactors (cyclicGroupToSFactors cg)) == n

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

cyclicGroupSpecialCase1 :: Assertion
cyclicGroupSpecialCase1 = assertBool "should be non-cyclic" $ isNothing $ cyclicGroupFromModulo (8 :: Integer)

allUnique :: Ord a => [a] -> Bool
allUnique = go S.empty
  where
    go _ []         = True
    go acc (x : xs) = not (x `S.member` acc) && go (S.insert x acc) xs

isPrimitiveRoot'Property1
  :: forall a. (Euclidean a, Integral a, UniqueFactorisation a)
  => AnySign a
  -> Positive Natural
  -> Bool
isPrimitiveRoot'Property1 (AnySign n) (Positive m) = case someNatVal m of
  SomeNat (_ :: Proxy m) -> case cyclicGroup :: Maybe (CyclicGroup a m) of
    Nothing -> True
    Just cg -> case isPrimitiveRoot cg (fromIntegral n) of
      Nothing -> True
      Just rt -> gcd m (unMod (multElement (unPrimitiveRoot rt))) == 1

isPrimitiveRootProperty1 :: AnySign Integer -> Positive Natural -> Bool
isPrimitiveRootProperty1 (AnySign n) (Positive m) = case someNatVal m of
  SomeNat (_ :: Proxy m) -> case cyclicGroup :: Maybe (CyclicGroup Integer m) of
    Nothing -> True
    Just cg -> gcd n (toInteger m) == 1
            || isNothing (isPrimitiveRoot cg (fromInteger n))

isPrimitiveRootProperty2 :: Positive Natural -> Bool
isPrimitiveRootProperty2 (Positive m) = case someNatVal m of
  SomeNat (_ :: Proxy m) -> case cyclicGroup :: Maybe (CyclicGroup Integer m) of
    Nothing -> True
    Just cg -> any (isJust . isPrimitiveRoot cg) [minBound..maxBound]

isPrimitiveRootProperty3 :: AnySign Integer -> Positive Natural -> Bool
isPrimitiveRootProperty3 (AnySign n) (Positive m) = case someNatVal m of
  SomeNat (_ :: Proxy m) -> case cyclicGroup :: Maybe (CyclicGroup Integer m) of
    Nothing -> True
    Just cg -> let n' = fromInteger n
      in isNothing (isPrimitiveRoot cg n')
      || allUnique (genericTake (totient m - 1) (iterate (* n') 1))

isPrimitiveRootProperty5 :: Positive Natural -> Bool
isPrimitiveRootProperty5 (Positive m) = case someNatVal m of
  SomeNat (_ :: Proxy m) -> case cyclicGroup :: Maybe (CyclicGroup Integer m) of
    Nothing -> True
    Just cg -> genericLength (mapMaybe (isPrimitiveRoot cg) [minBound..maxBound]) == totient (totient m)

testSuite :: TestTree
testSuite = testGroup "Primitive root"
  [ testGroup "CyclicGroup"
    [ testIntegralProperty "cyclicGroupFromModulo" cyclicGroupProperty1
    , testIntegralProperty "cyclic group mod p" cyclicGroupProperty2
    , testIntegralProperty "cyclic group mod 2p" cyclicGroupProperty3
    , testCase "cyclic group mod 8" cyclicGroupSpecialCase1
    ]
  , testGroup "isPrimitiveRoot'"
    [ testGroup "primitive root is coprime with modulo"
      [ testSmallAndQuick "Integer" (isPrimitiveRoot'Property1 :: AnySign Integer -> Positive Natural -> Bool)
      , testSmallAndQuick "Natural" (isPrimitiveRoot'Property1 :: AnySign Natural -> Positive Natural -> Bool)
      , testSmallAndQuick "Int"     (isPrimitiveRoot'Property1 :: AnySign Int     -> Positive Natural -> Bool)
      , testSmallAndQuick "Word"    (isPrimitiveRoot'Property1 :: AnySign Word    -> Positive Natural -> Bool)
      ]
    ]
  , testGroup "isPrimitiveRoot"
    [ testSmallAndQuick "primitive root is coprime with modulo"            isPrimitiveRootProperty1
    , testSmallAndQuick "cyclic group has a primitive root"                isPrimitiveRootProperty2
    , testSmallAndQuick "primitive root generates cyclic group"            isPrimitiveRootProperty3
    , testSmallAndQuick "cyclic group has right number of primitive roots" isPrimitiveRootProperty5
    ]
  ]
