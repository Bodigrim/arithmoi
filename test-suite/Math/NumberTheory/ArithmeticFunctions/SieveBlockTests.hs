-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.SieveBlockTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
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

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.Vector as V

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.ArithmeticFunctions.SieveBlock

pointwiseTest :: (Eq a, Show a) => ArithmeticFunction Word a -> Word -> Word -> IO ()
pointwiseTest f lowIndex len = assertEqual "pointwise"
    (runFunctionOverBlock f lowIndex len)
    (V.generate (fromIntegral len) (runFunction f . (+ lowIndex) . fromIntegral))

moebiusTest :: Word -> Word -> Bool
moebiusTest m n
  = m == 0
  || sieveBlockUnboxed moebiusConfig m n
  == runMoebiusOverBlock m n

moebiusSpecialCases :: [TestTree]
moebiusSpecialCases = map (uncurry pairToTest)
  [ (1, 1)
  , (1, 2)
  , (208, 298)
  , (1, 12835)
  , (10956, 4430)
  , (65, 16171)
  , (120906, 19456)
  , (33800000, 27002)
  , (17266222643, 5051)
  , (1000158, 48758)
  , (1307265, 3725)
  , (2600000, 14686)
  , (4516141422507 - 100000, 100001)
  , (1133551497049257 - 100000, 100001)
  -- too long for regular runs
  -- , (1157562178759482171 - 100000, 100001)
  ]
  where
    pairToTest :: Word -> Word -> TestTree
    pairToTest m n = testCase (show m ++ "," ++ show n) $ assertBool "should be equal" $ moebiusTest m n

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
  , testGroup "special moebius" moebiusSpecialCases
  ]
