{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SequenceBench
  ( benchSuite
  ) where

import Test.Tasty.Bench

import Data.Bits
import qualified Data.Vector.Unboxed as U

import Math.NumberTheory.Primes (Prime(..), nextPrime, precPrime)
import Math.NumberTheory.Primes.Testing

filterIsPrime :: (Integer, Integer) -> Integer
filterIsPrime (p, q) = sum $ takeWhile (<= q) $ dropWhile (< p) $ filter isPrime (map toPrim [toIdx p .. toIdx q])

eratosthenes :: (Integer, Integer) -> Integer
eratosthenes (p, q) = sum (map unPrime [nextPrime p .. precPrime q])

filterIsPrimeBench :: Benchmark
filterIsPrimeBench = bgroup "filterIsPrime" $
  [ bench (show (10^x, 10^y)) $ nf filterIsPrime (10^x, 10^x + 10^y)
  | x <- [5..8]
  , y <- [3..x-1]
  ]

eratosthenesBench :: Benchmark
eratosthenesBench = bgroup "eratosthenes" $
  [ bench (show (10^x, 10^y)) $ nf eratosthenes (10^x, 10^x + 10^y)
  | x <- [10..17]
  , y <- [6..x-1]
  , x == 10 || y == 7
  ]

benchSuite :: Benchmark
benchSuite = bgroup "Sequence"
    [ filterIsPrimeBench
    , eratosthenesBench
    ]

-------------------------------------------------------------------------------
-- Utils copypasted from internal modules

rho :: Int -> Int
rho i = residues U.! i

residues :: U.Vector Int
residues = U.fromList [7,11,13,17,19,23,29,31]

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
