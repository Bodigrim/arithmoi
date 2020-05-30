-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.MertensTests
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.ArithmeticFunctions.Mertens
--

{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctions.MertensTests
  ( testSuite
  ) where

import Test.Tasty

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.ArithmeticFunctions.Mertens
import Math.NumberTheory.ArithmeticFunctions.SieveBlock
import Math.NumberTheory.TestUtils

moebiusConfig :: SieveBlockConfig Moebius
moebiusConfig = SieveBlockConfig
  { sbcEmpty                = MoebiusP
  , sbcAppend               = (<>)
  , sbcFunctionOnPrimePower = const $ \case
      0 -> MoebiusP
      1 -> MoebiusN
      _ -> MoebiusZ
  }

mertensDiffPointwise :: Word -> Word -> Int
mertensDiffPointwise lo len = sum $ map (runMoebius . moebius) [lo + 1 .. lo + len]

mertensDiffBlockSpecial :: Word -> Word -> Int
mertensDiffBlockSpecial lo len = U.sum $ U.map runMoebius
  $ runMoebiusOverBlock (lo + 1) len

mertensDiffBlockUnboxed :: Word -> Word -> Int
mertensDiffBlockUnboxed lo len = U.sum $ U.map runMoebius
  $ sieveBlockUnboxed moebiusConfig (lo + 1) len

mertensDiffBlockBoxed :: Word -> Word -> Int
mertensDiffBlockBoxed lo len = V.sum $ V.map runMoebius
  $ sieveBlock moebiusConfig (lo + 1) len

mertensDiff :: Word -> Word -> Int
mertensDiff lo len = mertens (lo + len) - mertens lo

propertyCompare :: (Word -> Word -> Int) -> Word -> Word -> Bool
propertyCompare func lo' len' = mertensDiff lo len == func lo len
  where
    lo  = lo'  `rem` 10000000
    len = len' `rem` 1000

testSuite :: TestTree
testSuite = testGroup "Mertens"
  [ testSmallAndQuick "pointwise"     $ propertyCompare mertensDiffPointwise
  , testSmallAndQuick "block special" $ propertyCompare mertensDiffBlockSpecial
  , testSmallAndQuick "block unboxed" $ propertyCompare mertensDiffBlockUnboxed
  , testSmallAndQuick "block boxed"   $ propertyCompare mertensDiffBlockBoxed
  ]
