{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SequenceBench
  ( benchSuite
  ) where

import Gauge.Main

import Data.Array.Unboxed
import Data.Bits
import Data.Maybe

import Math.NumberTheory.Primes

filterIsPrime :: (Integer, Integer) -> Integer
filterIsPrime (p, q) = sum $ takeWhile (<= p + q) $ dropWhile (< p) $ filter (isJust . isPrime) (map toPrim [toIdx p .. toIdx (p + q)])

eratosthenes :: (Integer, Integer) -> Integer
eratosthenes (p, q) = sum (map unPrime [nextPrime p .. precPrime (p + q)])

atkin :: (Integer, Integer) -> Integer
atkin (p, q) = toInteger $ sum $ atkinPrimeList $ atkinSieve (fromInteger p) (fromInteger q)

filterIsPrimeBench :: Benchmark
filterIsPrimeBench = bgroup "filterIsPrime" $
  [ bench (show (10^x, 10^y)) $ nf filterIsPrime (10^x, 10^x)
  | x <- [5..8]
  , y <- [3..x-1]
  ]

sieveBench :: Benchmark
sieveBench = bgroup "sieve" $ concat
  [ [ bench ("eratosthenes/" ++ show (10^x, 10^y)) $ nf eratosthenes (10^x, 10^y)
    , bench ("atkin/"        ++ show (10^x, 10^y)) $ nf atkin        (10^x, 10^y)
    ]
  | (x, y) <- map (10,) [6..9] ++ map (,7) [10..16]
  ]

benchSuite :: Benchmark
benchSuite = bgroup "Sequence"
    [ sieveBench
    , filterIsPrimeBench
    ]

-------------------------------------------------------------------------------
-- Utils copypasted from internal modules

rho :: Int -> Int
rho i = residues ! i

residues :: UArray Int Int
residues = listArray (0,7) [7,11,13,17,19,23,29,31]

toIdx :: Integral a => a -> Int
toIdx n = 8*fromIntegral q+r2
  where
    (q,r) = (n-7) `quotRem` 30
    r1 = fromIntegral r `quot` 3
    r2 = min 7 (if r1 > 5 then r1-1 else r1)

toPrim :: Integral a => Int -> a
toPrim ix = 30*fromIntegral k + fromIntegral (rho i)
  where
    i = ix .&. 7
    k = ix `shiftR` 3
