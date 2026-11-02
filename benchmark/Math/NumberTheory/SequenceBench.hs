{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SequenceBench
  ( benchSuite
  ) where

import Test.Tasty.Bench

import Data.Bits
import Data.Ord (Down(..))
import qualified Data.Vector.Unboxed as U

import Math.NumberTheory.Primes (Prime(..), nextPrime, precPrime)
import Math.NumberTheory.Primes.Testing

filterIsPrime :: (Integer, Integer) -> Integer
filterIsPrime (p, q) = sum $ takeWhile (<= q) $ dropWhile (< p) $ filter isPrime (map toPrim [toIdx p .. toIdx q])

filterIsPrimeBench :: Benchmark
filterIsPrimeBench = bgroup "filterIsPrime" $
  [ bench (show (10^x, 10^y)) $ nf filterIsPrime (10^x, 10^x + 10^y)
  | x <- [5..8]
  , y <- [3..x-1]
  ]

-- The sums are taken in Word, so that the whole range up to 2^64 - 1 is covered
-- (the sums themselves wrap around, which is harmless for a benchmark).

eratosthenes :: (Word, Word) -> Word
eratosthenes (p, q) = sum (map unPrime [nextPrime p .. precPrime q])

-- Decreasing sequence of the same primes.
eratosthenesDown :: (Word, Word) -> Word
eratosthenesDown (p, q) = sum [ unPrime r | Down r <- [Down (precPrime q) .. Down (nextPrime p)] ]

-- Windows [x, x + w] for x from 10^10 to the top of the range, and for the top window
-- [2^64 - 1 - w, 2^64 - 1].  The widths run from where testing every candidate is used,
-- through the cross-over to the sieve (about 6 * 10^4 at 10^17 and 2 * 10^5 at 2^64), to ranges
-- of several chunks (a chunk covers about 1.6 * 10^7 values).
windows :: [(String, Word, Word)]
windows =
  [ (show' x w, x, x + w)
  | w <- widths
  , x <- [ 10 ^ k | k <- [10, 12, 14, 16, 17, 18, 19 :: Int] ]
  ] ++
  [ ("2^64-1-" ++ show w ++ " .. 2^64-1", maxBound - w, maxBound) | w <- widths ]
  where
    widths = [10 ^ 4, 10 ^ 5, 10 ^ 6, 10 ^ 7, 5 * 10 ^ 7] :: [Word]
    show' x w = show (x, w)

eratosthenesBench :: Benchmark
eratosthenesBench = bgroup "eratosthenes"
  [ bench name $ nf eratosthenes (lo, hi) | (name, lo, hi) <- windows ]

eratosthenesDownBench :: Benchmark
eratosthenesDownBench = bgroup "eratosthenes decreasing"
  [ bench name $ nf eratosthenesDown (lo, hi) | (name, lo, hi) <- windows ]

benchSuite :: Benchmark
benchSuite = bgroup "Sequence"
    [ filterIsPrimeBench
    , eratosthenesBench
    , eratosthenesDownBench
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
