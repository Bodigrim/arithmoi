{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.PrimitiveRootsBench
  ( benchSuite
  ) where

import Gauge.Main
import Data.Maybe

import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Primes

primRootWrap :: Integer -> Word -> Integer -> Bool
primRootWrap p k g = isPrimitiveRoot' (CGOddPrimePower p' k) g
  where p' = fromJust $ isPrime p

primRootWrap2 :: Integer -> Word -> Integer -> Bool
primRootWrap2 p k g = isPrimitiveRoot' (CGDoubleOddPrimePower p' k) g
  where p' = fromJust $ isPrime p

cyclicWrap :: Integer -> Maybe (CyclicGroup Integer)
cyclicWrap = cyclicGroupFromModulo

benchSuite :: Benchmark
benchSuite = bgroup "PrimRoot"
  [ bgroup "groupFromModulo"
    [ bench "3^20000"             $ nf cyclicWrap (3^20000)             -- prime to large power
    , bench "10000000000000061"   $ nf cyclicWrap (10^16 + 61)          -- large prime
    , bench "2*3^20000"           $ nf cyclicWrap (2*3^20000)           -- twice prime to large power
    , bench "10000000000000046"   $ nf cyclicWrap (10^16 + 46)          -- twice large prime
    , bench "224403121196654400"  $ nf cyclicWrap (224403121196654400)  -- highly composite
    ]
  , bgroup "check prim roots"
    [ bench "3^20000"             $ nf (primRootWrap  3              20000) 2 -- prime to large power
    , bench "10000000000000061"   $ nf (primRootWrap  (10^16 + 61)   1)     3 -- large prime
    , bench "10000000000000061^2" $ nf (primRootWrap  (10^16 + 61)   2)     3 -- large prime squared
    , bench "2*3^20000"           $ nf (primRootWrap2 3              20000) 5 -- twice prime to large power
    , bench "10000000000000046"   $ nf (primRootWrap2 (5*10^15 + 23) 1)     5 -- twice large prime
    ]
  ]
