{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-deprecations  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SieveBlockBench
  ( benchSuite
  ) where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
#if __GLASGOW_HASKELL__ < 709
import Data.Word
#endif

import Math.NumberTheory.ArithmeticFunctions.SieveBlock
import Math.NumberTheory.Primes.Factorisation (totientSieve, sieveTotient, carmichaelSieve, sieveCarmichael)

blockLen :: Word
blockLen = 10^6

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

sumOldTotientSieve :: Word -> Word
sumOldTotientSieve len' = sum $ map (fromInteger . sieveTotient sieve) [1 .. len]
  where
    len = toInteger len'
    sieve = totientSieve len

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

sumOldCarmichaelSieve :: Word -> Word
sumOldCarmichaelSieve len' = sum $ map (fromInteger . sieveCarmichael sieve) [1 .. len]
  where
    len = toInteger len'
    sieve = carmichaelSieve len

benchSuite :: Benchmark
benchSuite = bgroup "SieveBlock"
  [ bgroup "totient"
    [ bench "old"     $ nf sumOldTotientSieve blockLen
    , bench "boxed"   $ nf (V.sum . sieveBlock        totientBlockConfig 1) blockLen
    , bench "unboxed" $ nf (U.sum . sieveBlockUnboxed totientBlockConfig 1) blockLen
    ]
  , bgroup "carmichael"
    [ bench "old"     $ nf sumOldCarmichaelSieve blockLen
    , bench "boxed"   $ nf (V.sum . sieveBlock        carmichaelBlockConfig 1) blockLen
    , bench "unboxed" $ nf (U.sum . sieveBlockUnboxed carmichaelBlockConfig 1) blockLen
    ]
  ]
