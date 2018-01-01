-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.SieveBlockTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.ArithmeticFunctions.SieveBlock
--

{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctions.SieveBlockTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
#if __GLASGOW_HASKELL__ < 709
import Data.Word
#endif

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.ArithmeticFunctions.SieveBlock

pointwiseTest :: (Eq a, Show a) => ArithmeticFunction Word a -> Word -> Word -> IO ()
pointwiseTest f lowIndex len = assertEqual "pointwise"
    (runFunctionOverBlock f lowIndex len)
    (V.generate (fromIntegral len) (runFunction f . (+ lowIndex) . fromIntegral))

unboxedTest :: (Eq a, U.Unbox a, Show a) => SieveBlockConfig a -> IO ()
unboxedTest config = assertEqual "unboxed"
    (sieveBlock config 1 1000)
    (U.convert $ sieveBlockUnboxed config 1 1000)

multiplicativeConfig :: (Word -> Word -> Word) -> SieveBlockConfig Word
multiplicativeConfig f = SieveBlockConfig
  { sbcEmpty                = 1
  , sbcAppend               = (*)
  , sbcFunctionOnPrimePower = f
  }

moebiusConfig :: SieveBlockConfig Moebius
moebiusConfig = SieveBlockConfig
  { sbcEmpty = MoebiusP
  , sbcAppend = (<>)
  , sbcFunctionOnPrimePower = const $ \case
      0 -> MoebiusP
      1 -> MoebiusN
      _ -> MoebiusZ
  }

testSuite :: TestTree
testSuite = testGroup "SieveBlock"
  [ testGroup "pointwise"
    [ testCase "divisors"   $ pointwiseTest divisorsA   1 1000
    , testCase "tau"        $ pointwiseTest tauA        1 1000
    , testCase "totient"    $ pointwiseTest totientA    1 1000
    , testCase "moebius"    $ pointwiseTest moebiusA    1 1000
    , testCase "smallOmega" $ pointwiseTest smallOmegaA 1 1000
    , testCase "bigOmega"   $ pointwiseTest bigOmegaA   1 1000
    , testCase "carmichael" $ pointwiseTest carmichaelA 1 1000
    ]
  , testGroup "unboxed"
    [ testCase "id"      $ unboxedTest $ multiplicativeConfig (^)
    , testCase "tau"     $ unboxedTest $ multiplicativeConfig (const id)
    , testCase "moebius" $ unboxedTest moebiusConfig
    , testCase "totient" $ unboxedTest $ multiplicativeConfig (\p a -> (p - 1) * p ^ (a - 1))
    ]
  ]
