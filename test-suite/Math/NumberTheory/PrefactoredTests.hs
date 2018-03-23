-- |
-- Module:      Math.NumberTheory.PrefactoredTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Prefactored
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.PrefactoredTests
  ( testSuite
  ) where

import Test.Tasty

import Control.Arrow (second)
import Data.Bits (Bits)
import Data.List (tails)
import Numeric.Natural

import Math.NumberTheory.GCD (coprime, splitIntoCoprimes)
import Math.NumberTheory.Prefactored
import Math.NumberTheory.TestUtils

isValid :: (Eq a, Bits a, Integral a) => Prefactored a -> Bool
isValid pref
  = abs n == abs (product (map (uncurry (^)) fs))
  && and [ coprime g h | ((g, _) : gs) <- tails fs, (h, _) <- gs ]
  where
    n  = prefValue   pref
    fs = prefFactors pref

fromValueProperty :: Integer -> Bool
fromValueProperty n = isValid pref && prefValue pref == n
  where
    pref = fromValue n

fromFactorsProperty :: [(Integer, Power Word)] -> Bool
fromFactorsProperty fs' = isValid pref && abs (prefValue pref) == abs (product (map (uncurry (^)) fs))
  where
    fs   = map (second getPower) fs'
    pref = fromFactors (splitIntoCoprimes fs)

plusProperty :: Integer -> Integer -> Bool
plusProperty x y = isValid z && prefValue z == x + y
  where
    z = fromValue x + fromValue y

minusProperty :: Integer -> Integer -> Bool
minusProperty x y = isValid z && prefValue z == x - y
  where
    z = fromValue x - fromValue y

minusNaturalProperty :: Natural -> Natural -> Bool
minusNaturalProperty x y = x < y || (isValid z && prefValue z == x - y)
  where
    z = fromValue x - fromValue y

multiplyProperty :: Integer -> Integer -> Bool
multiplyProperty x y = isValid z && prefValue z == x * y
  where
    z = fromValue x * fromValue y

negateProperty :: Integer -> Bool
negateProperty x = isValid z && prefValue z == negate x
  where
    z = negate (fromValue x)

absSignumProperty :: Integer -> Bool
absSignumProperty x = isValid z && prefValue z == x
  where
    z = abs (fromValue x) * signum (fromValue x)

fromIntegerProperty :: Integer -> Bool
fromIntegerProperty n = isValid pref && prefValue pref == n
  where
    pref = fromInteger n

testSuite :: TestTree
testSuite = testGroup "Prefactored"
  [ testSmallAndQuick "fromValue"   fromValueProperty
  , testSmallAndQuick "fromFactors" fromFactorsProperty
  , testGroup "Num instance"
    [ testSmallAndQuick "plus"         plusProperty
    , testSmallAndQuick "minus"        minusProperty
    , testSmallAndQuick "minusNatural" minusNaturalProperty
    , testSmallAndQuick "multiply"     multiplyProperty
    , testSmallAndQuick "negate"       negateProperty
    , testSmallAndQuick "absSignum"    absSignumProperty
    , testSmallAndQuick "fromInteger"  fromIntegerProperty
    ]
  ]
