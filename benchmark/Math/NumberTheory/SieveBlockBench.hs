{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SieveBlockBench
  ( benchSuite
  ) where

import Gauge.Main
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Math.NumberTheory.ArithmeticFunctions.Moebius
import Math.NumberTheory.ArithmeticFunctions.SieveBlock

blockLen :: Word
blockLen = 1000000

totientHelper :: Word -> Word -> Word
totientHelper p 1 = p - 1
totientHelper p 2 = (p - 1) * p
totientHelper p k = (p - 1) * p ^ (k - 1)

totientBlockConfig :: SieveBlockConfig Word
totientBlockConfig = SieveBlockConfig
  { sbcEmpty                = 1
  , sbcAppend               = (*)
  , sbcFunctionOnPrimePower = totientHelper
  }

carmichaelHelper :: Word -> Word -> Word
carmichaelHelper 2 1 = 1
carmichaelHelper 2 2 = 2
carmichaelHelper 2 k = 2 ^ (k - 2)
carmichaelHelper p 1 = p - 1
carmichaelHelper p 2 = (p - 1) * p
carmichaelHelper p k = (p - 1) * p ^ (k - 1)

carmichaelBlockConfig :: SieveBlockConfig Word
carmichaelBlockConfig = SieveBlockConfig
  { sbcEmpty                = 1
  -- There is a specialized 'gcd' for Word, but not 'lcm'.
  , sbcAppend               = (\x y -> (x `quot` (gcd x y)) * y)
  , sbcFunctionOnPrimePower = carmichaelHelper
  }

moebiusConfig :: SieveBlockConfig Moebius
moebiusConfig = SieveBlockConfig
  { sbcEmpty                = MoebiusP
  , sbcAppend               = (<>)
  , sbcFunctionOnPrimePower = const $ \case
      0 -> MoebiusP
      1 -> MoebiusN
      _ -> MoebiusZ
  }

benchSuite :: Benchmark
benchSuite = bgroup "SieveBlock"
  [ bgroup "totient"
    [ bench "boxed"   $ nf (V.sum . sieveBlock        totientBlockConfig 1) blockLen
    , bench "unboxed" $ nf (U.sum . sieveBlockUnboxed totientBlockConfig 1) blockLen
    ]
  , bgroup "carmichael"
    [ bench "boxed"   $ nf (V.sum . sieveBlock        carmichaelBlockConfig 1) blockLen
    , bench "unboxed" $ nf (U.sum . sieveBlockUnboxed carmichaelBlockConfig 1) blockLen
    ]
  , bgroup "moebius"
    [ bench "boxed"   $ nf (V.sum . V.map runMoebius . sieveBlock        moebiusConfig 1 :: Word -> Int) blockLen
    , bench "unboxed" $ nf (U.sum . U.map runMoebius . sieveBlockUnboxed moebiusConfig 1 :: Word -> Int) blockLen
    , bench "special" $ nf (U.sum . U.map runMoebius . sieveBlockMoebius 1 :: Word -> Int) blockLen
    ]
  ]
