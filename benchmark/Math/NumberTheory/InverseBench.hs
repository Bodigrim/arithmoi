{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.InverseBench
  ( benchSuite
  ) where

import Test.Tasty.Bench
import Data.Bits (Bits)
import Data.Euclidean
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions.Inverse
import Math.NumberTheory.Primes

fact :: (Enum a, Num a) => a
fact = product [1..13]

tens :: Num a => a
tens = 10 ^ 18

countInverseTotient :: (Ord a, Integral a, Euclidean a, UniqueFactorisation a) => a -> Word
countInverseTotient = inverseTotient (const 1)

countInverseSigma :: (Integral a, Euclidean a, UniqueFactorisation a, Enum (Prime a), Bits a) => a -> Word
countInverseSigma = inverseSigma (const 1)

benchSuite :: Benchmark
benchSuite = bgroup "Inverse"
  [ bgroup "Totient"
    [ bgroup "factorial"
      [ bench "Int"     $ nf (countInverseTotient @Int)     fact
      , bench "Word"    $ nf (countInverseTotient @Word)    fact
      , bench "Integer" $ nf (countInverseTotient @Integer) fact
      , bench "Natural" $ nf (countInverseTotient @Natural) fact
      ]
    , bgroup "power of 10"
      [ bench "Int"     $ nf (countInverseTotient @Int)     tens
      , bench "Word"    $ nf (countInverseTotient @Word)    tens
      , bench "Integer" $ nf (countInverseTotient @Integer) tens
      , bench "Natural" $ nf (countInverseTotient @Natural) tens
      ]
    ]
  , bgroup "Sigma1"
    [ bgroup "factorial"
      [ bench "Int"     $ nf (countInverseSigma @Int)     fact
      , bench "Word"    $ nf (countInverseSigma @Word)    fact
      , bench "Integer" $ nf (countInverseSigma @Integer) fact
      , bench "Natural" $ nf (countInverseSigma @Natural) fact
      ]
    , bgroup "power of 10"
      [ bench "Int"     $ nf (countInverseSigma @Int)     tens
      , bench "Word"    $ nf (countInverseSigma @Word)    tens
      , bench "Integer" $ nf (countInverseSigma @Integer) tens
      , bench "Natural" $ nf (countInverseSigma @Natural) tens
      ]
    ]
  ]
