-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Indexing
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Auxiliary stuff, conversion between number and index,
-- remainders modulo 30 and related things.
{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.Primes.Sieve.Indexing
  ( idxPr
  , toPrim
  , rho
  ) where

import Data.Bits

import Math.NumberTheory.Unsafe

{-# INLINE idxPr #-}
idxPr :: Integral a => a -> (Int, Int)
idxPr n0
  | n0 < 7 = (0, 0)
  | otherwise = (fromIntegral bytes0, rm3)
  where
    n =
      if (fromIntegral n0 .&. 1 == (1 :: Int))
        then n0
        else (n0 - 1)
    (bytes0, rm0) = (n - 7) `quotRem` 30
    rm1 = fromIntegral rm0
    rm2 = rm1 `quot` 3
    rm3 =
      min
        7
        (if rm2 > 5
           then rm2 - 1
           else rm2)

{-# INLINE toPrim #-}
toPrim :: Num a => Int -> a
toPrim ix = 30 * fromIntegral k + fromIntegral (rho i)
  where
    i = ix .&. 7
    k = ix `shiftR` 3

{-# INLINE rho #-}
rho :: Int -> Int
rho = unsafeIndex residues

residues :: Vector Int
residues = fromList [7, 11, 13, 17, 19, 23, 29, 31]
