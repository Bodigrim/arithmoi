{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Math.NumberTheory.PowersBench
  ( benchSuite
  ) where

import Criterion.Main
import System.Random

import Math.NumberTheory.Logarithms (integerLog2)
import Math.NumberTheory.Powers.Squares.Internal

genInteger :: Int -> Int -> Integer
genInteger salt bits
    = head
    . dropWhile ((< bits) . integerLog2)
    . scanl (\a r -> a * 2^31 + abs r) 1
    . randoms
    . mkStdGen
    $ salt + bits

compareRoots :: Int -> Benchmark
compareRoots bits = bgroup ("sqrt" ++ show bits)
  [ bench "new" $ nf (fst . karatsubaSqrt) n
  , bench "old" $ nf isqrtA n
  ]
  where
    n = genInteger 0 bits

benchSuite = bgroup "Powers" $ map compareRoots [2300, 2400 .. 2600]
