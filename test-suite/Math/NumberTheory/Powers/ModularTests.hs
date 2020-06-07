-- |
-- Module:      Math.NumberTheory.Powers.ModularTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Powers.Modular
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.ModularTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Bits
import Numeric.Natural

import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.TestUtils

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

-- | Specialized to trigger 'powModInt'.
powModPropertyInt :: AnySign Int -> NonNegative Int -> Positive Int -> Bool
powModPropertyInt (AnySign b) (NonNegative e) (Positive m) = powModInt b e m == fromInteger (powMod' (fromIntegral b) (fromIntegral e) (fromIntegral m))

-- | Specialized to trigger 'powModWord'.
powModPropertyWord :: AnySign Word -> NonNegative Word -> Positive Word -> Bool
powModPropertyWord (AnySign b) (NonNegative e) (Positive m) = powModWord b e m == fromInteger (powMod' (fromIntegral b) (fromIntegral e) (fromIntegral m))

-- | Specialized to trigger 'powModInteger'.
powModPropertyInteger :: AnySign Integer -> NonNegative Integer -> Positive Integer -> Bool
powModPropertyInteger (AnySign b) (NonNegative e) (Positive m) = powMod b e m == fromInteger (powMod' b (fromIntegral e) m)

-- | Specialized to trigger 'powModNatural'.
powModPropertyNatural :: AnySign Natural -> NonNegative Natural -> Positive Natural -> Bool
powModPropertyNatural (AnySign b) (NonNegative e) (Positive m) = powMod b e m == fromInteger (powMod' (fromIntegral b) e (fromIntegral m))

-- | Large modulo m such that m^2 overflows.
powModSpecialCase1Int :: Assertion
powModSpecialCase1Int =
  assertEqual "powModInt" (powModInt 3 101 (2^60-1)) 1018105167100379328

-- | Large modulo m such that m^2 overflows.
powModSpecialCase1Word :: Assertion
powModSpecialCase1Word =
  assertEqual "powModWord" (powModWord 3 101 (2^60-1)) 1018105167100379328

testSuite :: TestTree
testSuite = testGroup "Modular"
  [ testGroup "powMod" $
    [ testSmallAndQuick "range"                  powModProperty1
    , testSmallAndQuick "multiplicative by base" powModProperty2
    , testSmallAndQuick "additive by exponent"   powModProperty3

    , testSmallAndQuick "powModInt"              powModPropertyInt
    , testSmallAndQuick "powModWord"             powModPropertyWord
    , testSmallAndQuick "powModInteger"          powModPropertyInteger
    , testSmallAndQuick "powModNatural"          powModPropertyNatural
    ] ++ if finiteBitSize (0 :: Word) /= 64 then [] else
    [ testCase          "large modulo :: Int"    powModSpecialCase1Int
    , testCase          "large modulo :: Word"   powModSpecialCase1Word
    ]
  ]
