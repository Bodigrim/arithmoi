-- |
-- Module:      Math.NumberTheory.Powers.ModularTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.Modular
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.ModularTests
  ( testSuite
  ) where

import Test.Tasty
#if MIN_VERSION_base(4,8,0)
import Test.Tasty.HUnit
#endif

import Numeric.Natural

import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.TestUtils

#include "MachDeps.h"

powMod' :: Integer -> Natural -> Integer -> Integer
powMod' = powMod

-- | Check that 'powMod' fits between 0 and m - 1.
powModProperty1 :: NonNegative Natural -> AnySign Integer -> Positive Integer -> Bool
powModProperty1 (NonNegative e) (AnySign b) (Positive m)
  = let v = powMod' b e m in 0 <= v && v < m

-- | Check that 'powMod'' is multiplicative by first argument.
powModProperty2 :: NonNegative Natural -> AnySign Integer -> AnySign Integer -> Positive Integer -> Bool
powModProperty2 (NonNegative e) (AnySign b1) (AnySign b2) (Positive m)
  = (powMod' b1 e m * powMod' b2 e m) `mod` m == powMod' (b1 * b2) e m

-- | Check that 'powMod' is additive by second argument.
powModProperty3 :: NonNegative Natural -> NonNegative Natural -> AnySign Integer -> Positive Integer -> Bool
powModProperty3 (NonNegative e1) (NonNegative e2) (AnySign b) (Positive m)
  = (powMod' b e1 m * powMod' b e2 m) `mod` m == powMod' b (e1 + e2) m

#if __GLASGOW_HASKELL__ > 709
-- | Specialized to trigger 'powModInt'.
powModProperty_Int :: AnySign Int -> NonNegative Int -> Positive Int -> Bool
powModProperty_Int (AnySign b) (NonNegative e) (Positive m) = powModInt b e m == fromInteger (powMod' (fromIntegral b) (fromIntegral e) (fromIntegral m))

-- | Specialized to trigger 'powModWord'.
powModProperty_Word :: AnySign Word -> NonNegative Word -> Positive Word -> Bool
powModProperty_Word (AnySign b) (NonNegative e) (Positive m) = powModWord b e m == fromInteger (powMod' (fromIntegral b) (fromIntegral e) (fromIntegral m))
#endif

-- | Specialized to trigger 'powModInteger'.
powModProperty_Integer :: AnySign Integer -> NonNegative Integer -> Positive Integer -> Bool
powModProperty_Integer (AnySign b) (NonNegative e) (Positive m) = powMod b e m == fromInteger (powMod' (fromIntegral b) (fromIntegral e) (fromIntegral m))

-- | Specialized to trigger 'powModNatural'.
powModProperty_Natural :: AnySign Natural -> NonNegative Natural -> Positive Natural -> Bool
powModProperty_Natural (AnySign b) (NonNegative e) (Positive m) = powMod b e m == fromInteger (powMod' (fromIntegral b) (fromIntegral e) (fromIntegral m))

#if WORD_SIZE_IN_BITS == 64 && __GLASGOW_HASKELL__ > 709
-- | Large modulo m such that m^2 overflows.
powModSpecialCase1_Int :: Assertion
powModSpecialCase1_Int =
  assertEqual "powModInt" (powModInt 3 101 (2^60-1)) 1018105167100379328

-- | Large modulo m such that m^2 overflows.
powModSpecialCase1_Word :: Assertion
powModSpecialCase1_Word =
  assertEqual "powModWord" (powModWord 3 101 (2^60-1)) 1018105167100379328
#endif

testSuite :: TestTree
testSuite = testGroup "Modular"
  [ testGroup "powMod"
    [ testSmallAndQuick "range"                  powModProperty1
    , testSmallAndQuick "multiplicative by base" powModProperty2
    , testSmallAndQuick "additive by exponent"   powModProperty3

#if __GLASGOW_HASKELL__ > 709
    , testSmallAndQuick "powModInt"              powModProperty_Int
    , testSmallAndQuick "powModWord"             powModProperty_Word
#endif
    , testSmallAndQuick "powModInteger"          powModProperty_Integer
    , testSmallAndQuick "powModNatural"          powModProperty_Natural

#if WORD_SIZE_IN_BITS == 64 && __GLASGOW_HASKELL__ > 709
    , testCase          "large modulo :: Int"    powModSpecialCase1_Int
    , testCase          "large modulo :: Word"   powModSpecialCase1_Word
#endif
    ]
  ]
