-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Indexing
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Sieve.Indexing
    ( idxPr
    , toPrim
    , toIdx
    , rho
    , delta
    , tau
    , byte
    , idx
    , mu
    , nu
    ) where

import Data.Array.Unboxed
import Data.Bits

import Math.NumberTheory.Unsafe

-- Auxiliary stuff, conversion between number and index,
-- remainders modulo 30 and related things.

-- {-# SPECIALISE idxPr :: Integer -> (Int,Int),
--                         Int -> (Int,Int),
--                         Word -> (Int,Int)
--   #-}
{-# INLINE idxPr #-}
idxPr :: Integral a => a -> (Int,Int)
idxPr n0
    | n0 < 7    = (0, 0)
    | otherwise = (fromIntegral bytes0, rm3)
  where
    n = if (fromIntegral n0 .&. 1 == (1 :: Int))
            then n0 else (n0-1)
    (bytes0,rm0) = (n-7) `quotRem` 30
    rm1 = fromIntegral rm0
    rm2 = rm1 `quot` 3
    rm3 = min 7 (if rm2 > 5 then rm2-1 else rm2)

-- {-# SPECIALISE toPrim :: Int -> Integer,
--                          Int -> Int,
--                          Int -> Word,
--                          Int -> Word16
--     #-}
{-# INLINE toPrim #-}
toPrim :: Integral a => Int -> a
toPrim ix = 30*fromIntegral k + fromIntegral (rho i)
  where
    i = ix .&. 7
    k = ix `shiftR` 3

-- Assumes n >= 7, gcd n 30 == 1
{-# INLINE toIdx #-}
toIdx :: Integral a => a -> Int
toIdx n = 8*fromIntegral q+r2
  where
    (q,r) = (n-7) `quotRem` 30
    r1 = fromIntegral r `quot` 3
    r2 = min 7 (if r1 > 5 then r1-1 else r1)

{-# INLINE rho #-}
rho :: Int -> Int
rho i = unsafeAt residues i

residues :: UArray Int Int
residues = listArray (0,7) [7,11,13,17,19,23,29,31]

{-# INLINE delta #-}
delta :: Int -> Int
delta i = unsafeAt deltas i

deltas :: UArray Int Int
deltas = listArray (0,7) [4,2,4,2,4,6,2,6]

{-# INLINE tau #-}
tau :: Int -> Int
tau i = unsafeAt taus i

taus :: UArray Int Int
taus = listArray (0,63)
        [  7,  4,  7,  4,  7, 12,  3, 12
        , 12,  6, 11,  6, 12, 18,  5, 18
        , 14,  7, 13,  7, 14, 21,  7, 21
        , 18,  9, 19,  9, 18, 27,  9, 27
        , 20, 10, 21, 10, 20, 30, 11, 30
        , 25, 12, 25, 12, 25, 36, 13, 36
        , 31, 15, 31, 15, 31, 47, 15, 47
        , 33, 17, 33, 17, 33, 49, 17, 49
        ]

{-# INLINE byte #-}
byte :: Int -> Int
byte i = unsafeAt startByte i

startByte :: UArray Int Int
startByte = listArray (0,7) [1,3,5,9,11,17,27,31]

{-# INLINE idx #-}
idx :: Int -> Int
idx i = unsafeAt startIdx i

startIdx :: UArray Int Int
startIdx = listArray (0,7) [4,7,4,4,7,4,7,7]

{-# INLINE mu #-}
mu :: Int -> Int
mu i = unsafeAt mArr i

{-# INLINE nu #-}
nu :: Int -> Int
nu i = unsafeAt nArr i

mArr :: UArray Int Int
mArr = listArray (0,63)
        [ 1,  2,  2,  3,  4,  5,  6,  7
        , 2,  3,  4,  6,  6,  8, 10, 11
        , 2,  4,  5,  7,  8,  9, 12, 13
        , 3,  6,  7,  9, 10, 12, 16, 17
        , 4,  6,  8, 10, 11, 14, 18, 19
        , 5,  8,  9, 12, 14, 17, 22, 23
        , 6, 10, 12, 16, 18, 22, 27, 29
        , 7, 11, 13, 17, 19, 23, 29, 31
        ]

nArr :: UArray Int Int
nArr = listArray (0,63)
        [ 4, 3, 7, 6, 2, 1, 5, 0
        , 3, 7, 5, 0, 6, 2, 4, 1
        , 7, 5, 4, 1, 0, 6, 3, 2
        , 6, 0, 1, 4, 5, 7, 2, 3
        , 2, 6, 0, 5, 7, 3, 1, 4
        , 1, 2, 6, 7, 3, 4, 0, 5
        , 5, 4, 3, 2, 1, 0, 7, 6
        , 0, 1, 2, 3, 4, 5, 6, 7
        ]
