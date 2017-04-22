-- |
-- Module:      Math.NumberTheory.Moduli.ClassTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Moduli.Class
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.ClassTests
  ( testSuite
  ) where

import Test.Tasty

import Data.Bits
import Data.Maybe

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

invertMod :: Integer -> Integer -> Maybe SomeMod
invertMod x m = invertSomeMod (x `modulo` fromInteger m)

powerMod :: Integral a => Integer -> a -> Integer -> SomeMod
powerMod b e m = (b `modulo` fromInteger m) ^ e

-- | Check that 'invertMod' inverts numbers modulo.
invertModProperty :: AnySign Integer -> Positive Integer -> Bool
invertModProperty (AnySign k) (Positive m) = case invertMod k m of
  Nothing            -> k `mod` m == 0 || gcd k m > 1
  Just InfMod{}      -> False
  Just (SomeMod inv) -> gcd k m == 1 && k * getVal inv `mod` m == 1

-- | Check that 'powerMod' is multiplicative by first argument.
powerModProperty2 :: (Integral a, Bits a) => NonNegative a -> AnySign Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty2 (NonNegative e) (AnySign b1) (AnySign b2) (Positive m)
  =  e < 0 && (isNothing (invertMod b1 m) || isNothing (invertMod b2 m))
  || pm1 * pm2 == pm12
  where
    pm1  = powerMod b1  e m
    pm2  = powerMod b2  e m
    pm12 = powerMod (b1 * b2) e m

-- | Check that 'powerMod' is additive by second argument.
powerModProperty3 :: (Integral a, Bits a) => NonNegative a -> NonNegative a -> AnySign Integer -> Positive Integer -> Bool
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
  ]
