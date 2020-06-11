-- |
-- Module:      Math.NumberTheory.Moduli.ClassTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Moduli.Class
--

{-# LANGUAGE DataKinds       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.ClassTests
  ( testSuite
  ) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Data.Maybe
import Numeric.Natural

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

invertMod :: Integer -> Integer -> Maybe SomeMod
invertMod x m = invertSomeMod (x `modulo` fromInteger m)

powerMod :: Integral a => Integer -> a -> Integer -> SomeMod
powerMod b e m = (b `modulo` fromInteger m) ^ e

-- | Check that 'invertMod' inverts numbers modulo.
invertModProperty :: AnySign Integer -> Positive Integer -> Bool
invertModProperty (AnySign k) (Positive m) = case invertMod k m of
  Nothing            -> k `rem` m == 0 || gcd k m > 1
  Just InfMod{}      -> False
  Just (SomeMod inv) -> gcd k m == 1 && k * getVal inv `mod` m == 1

-- | Check that 'powerMod' is multiplicative by first argument.
powerModProperty2 :: (Integral a) => NonNegative a -> AnySign Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty2 (NonNegative e) (AnySign b1) (AnySign b2) (Positive m)
  =  e < 0 && (isNothing (invertMod b1 m) || isNothing (invertMod b2 m))
  || pm1 * pm2 == pm12
  where
    pm1  = powerMod b1  e m
    pm2  = powerMod b2  e m
    pm12 = powerMod (b1 * b2) e m

-- | Check that 'powerMod' is additive by second argument.
powerModProperty3 :: (Integral a) => NonNegative a -> NonNegative a -> AnySign Integer -> Positive Integer -> Bool
powerModProperty3 (NonNegative e1) (NonNegative e2) (AnySign b) (Positive m)
  =  (e1 < 0 || e2 < 0) && isNothing (invertMod b m)
  || e2 >= 0 && e1 + e2 < e1 -- check overflow
  || e1 >= 0 && e1 + e2 < e2 -- check overflow
  || e2 <= 0 && e1 + e2 > e1 -- check overflow
  || e1 <= 0 && e1 + e2 > e2 -- check overflow
  || pm1 * pm2 == pm12
  where
    pm1  = powerMod b e1 m
    pm2  = powerMod b e2 m
    pm12 = powerMod b (e1 + e2) m

-- | Specialized to trigger 'powerModInteger'.
powerModProperty2_Integer :: NonNegative Integer -> AnySign Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty2_Integer = powerModProperty2

-- | Specialized to trigger 'powerModInteger'.
powerModProperty3_Integer :: NonNegative Integer -> NonNegative Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty3_Integer = powerModProperty3

someModAddProperty :: Integer -> Positive Natural -> Integer -> Positive Natural -> Bool
someModAddProperty x1 (Positive m1) x2 (Positive m2) = case x1 `modulo` m1 + x2 `modulo` m2 of
  SomeMod z -> getMod z == m3 && getVal z == x3
  InfMod{}  -> False
  where
    m3 = toInteger $ m1 `gcd` m2
    x3 = (x1 + x2) `mod` m3

someModSubProperty :: Integer -> Positive Natural -> Integer -> Positive Natural -> Bool
someModSubProperty x1 (Positive m1) x2 (Positive m2) = case x1 `modulo` m1 - x2 `modulo` m2 of
  SomeMod z -> getMod z == m3 && getVal z == x3
  InfMod{}  -> False
  where
    m3 = toInteger $ m1 `gcd` m2
    x3 = (x1 - x2) `mod` m3

someModMulProperty :: Integer -> Positive Natural -> Integer -> Positive Natural -> Bool
someModMulProperty x1 (Positive m1) x2 (Positive m2) = case (x1 `modulo` m1) * (x2 `modulo` m2) of
  SomeMod z -> getMod z == m3 && getVal z == x3
  InfMod{}  -> False
  where
    m3 = toInteger $ m1 `gcd` m2
    x3 = (x1 * x2) `mod` m3

sameSomeModMulProperty :: Integer -> Integer -> Positive Natural -> Bool
sameSomeModMulProperty x1 x2 (Positive m) = case (x1 `modulo` m) * (x2 `modulo` m) of
  SomeMod z -> getMod z == toInteger m && getVal z == x3
  InfMod{}  -> False
  where
    x3 = (x1 * x2) `mod` toInteger m

sameSomeModMulHugeProperty :: Integer -> Integer -> Positive (Huge Natural) -> Bool
sameSomeModMulHugeProperty x1 x2 (Positive (Huge m)) = case (x1 `modulo` m) * (x2 `modulo` m) of
  SomeMod z -> getMod z == toInteger m && getVal z == x3
  InfMod{}  -> False
  where
    x3 = (x1 * x2) `mod` toInteger m

sameSomeModMulHugeAllProperty :: Huge Integer -> Huge Integer -> Positive (Huge Natural) -> Bool
sameSomeModMulHugeAllProperty (Huge x1) (Huge x2) (Positive (Huge m)) = case (x1 `modulo` m) * (x2 `modulo` m) of
  SomeMod z -> getMod z == toInteger m && getVal z == x3
  InfMod{}  -> False
  where
    x3 = (x1 * x2) `mod` toInteger m

someModNegProperty :: Integer -> Positive Natural -> Bool
someModNegProperty x1 (Positive m1) = case negate (x1 `modulo` m1) of
  SomeMod z -> getMod z == m3 && getVal z == x3
  InfMod{}  -> False
  where
    m3 = toInteger m1
    x3 = negate x1 `mod` m3

someModAbsSignumProperty :: Integer -> Positive Natural -> Bool
someModAbsSignumProperty x (Positive m) = z == abs z * signum z
  where
    z = x `modulo` m

infModAddProperty :: Integer -> Positive Natural -> Integer -> Bool
infModAddProperty x1 (Positive m1) x2 = case x1 `modulo` m1 + fromInteger x2 of
  SomeMod z -> getMod z == m3 && getVal z == x3
  InfMod{}  -> False
  where
    m3 = toInteger m1
    x3 = (x1 + x2) `mod` m3

infModSubProperty :: Integer -> Positive Natural -> Integer -> Bool
infModSubProperty x1 (Positive m1) x2 = case x1 `modulo` m1 - fromInteger x2 of
  SomeMod z -> getMod z == m3 && getVal z == x3
  InfMod{}  -> False
  where
    m3 = toInteger m1
    x3 = (x1 - x2) `mod` m3

infModMulProperty :: Integer -> Positive Natural -> Integer -> Bool
infModMulProperty x1 (Positive m1) x2 = case x1 `modulo` m1 * fromInteger x2 of
  SomeMod z -> getMod z == m3 && getVal z == x3
  InfMod{}  -> False
  where
    m3 = toInteger m1
    x3 = (x1 * x2) `mod` m3

getValModProperty :: Integer -> Positive Natural -> Bool
getValModProperty x (Positive m) = case z of
  SomeMod t -> z == getVal t `modulo` getNatMod t && z == toInteger (getNatVal t) `modulo` fromInteger (getMod t)
  InfMod{} -> False
  where
    z = x `modulo` m

testSuite :: TestTree
testSuite = testGroup "Class"
  [ testSmallAndQuick "invertMod" invertModProperty
  , testGroup "powerMod"
    [ testGroup "generic"
      [ testIntegralProperty "multiplicative by base"   powerModProperty2
      , testSameIntegralProperty "additive by exponent" powerModProperty3
      ]
    , testGroup "Integer"
      [ testSmallAndQuick "multiplicative by base"  powerModProperty2_Integer
      , testSmallAndQuick "additive by exponent"    powerModProperty3_Integer
      ]
    ]
  , testGroup "Same SomeMod"
    [ testSmallAndQuick "mul" sameSomeModMulProperty
    , QC.testProperty "mul huge" sameSomeModMulHugeProperty
    , QC.testProperty "mul huge all" sameSomeModMulHugeAllProperty
    ]
  , testGroup "SomeMod"
    [ testSmallAndQuick "add" someModAddProperty
    , testSmallAndQuick "sub" someModSubProperty
    , testSmallAndQuick "mul" someModMulProperty
    , testSmallAndQuick "neg" someModNegProperty
    , testSmallAndQuick "abs" someModAbsSignumProperty
    ]
  , testGroup "InfMod"
    [ testSmallAndQuick "add" infModAddProperty
    , testSmallAndQuick "sub" infModSubProperty
    , testSmallAndQuick "mul" infModMulProperty
    ]
  , testSmallAndQuick "getVal/getMod" getValModProperty
  ]
