-- |
-- Module:      Math.NumberTheory.Powers.Squares.Internal
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Internal functions dealing with square roots. End-users should not import this module.

{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

module Math.NumberTheory.Powers.Squares.Internal
  ( karatsubaSqrt
  , isqrtA
  ) where

#include "MachDeps.h"

import Data.Bits

import GHC.Base
import GHC.Integer
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms (integerLog2#)

import Math.NumberTheory.Logarithms (integerLog2)
#if __GLASGOW_HASKELL__ < 707
import Math.NumberTheory.Utils (isTrue#)
#endif

-- Find approximation to square root in 'Integer', then
-- find the integer square root by the integer variant
-- of Heron's method. Takes only a handful of steps
-- unless the input is really large.
{-# SPECIALISE isqrtA :: Integer -> Integer #-}
isqrtA :: Integral a => a -> a
isqrtA 0 = 0
isqrtA n = heron n (fromInteger . appSqrt . fromIntegral $ n)

-- Heron's method for integers. First make one step to ensure
-- the value we're working on is @>= r@, then we have
-- @k == r@ iff @k <= step k@.
{-# SPECIALISE heron :: Integer -> Integer -> Integer #-}
heron :: Integral a => a -> a -> a
heron n a = go (step a)
      where
        step k = (k + n `quot` k) `quot` 2
        go k
            | m < k     = go m
            | otherwise = k
              where
                m = step k

-- threshold for shifting vs. direct fromInteger
-- we shift when we expect more than 256 bits
#if WORD_SIZE_IN_BITS == 64
#define THRESH 5
#else
#define THRESH 9
#endif

-- Find a fairly good approximation to the square root.
-- At most one off for small Integers, about 48 bits should be correct
-- for large Integers.
appSqrt :: Integer -> Integer
appSqrt (S# i#) = S# (double2Int# (sqrtDouble# (int2Double# i#)))
#if __GLASGOW_HASKELL__ < 709
appSqrt n@(J# s# _)
    | isTrue# (s# <# THRESH#) = floor (sqrt $ fromInteger n :: Double)
#else
appSqrt n@(Jp# bn#)
    | isTrue# ((sizeofBigNat# bn#) <# THRESH#) =
          floor (sqrt $ fromInteger n :: Double)
#endif
    | otherwise = case integerLog2# n of
                    l# -> case uncheckedIShiftRA# l# 1# -# 47# of
                            h# -> case shiftRInteger n (2# *# h#) of
                                    m -> case floor (sqrt $ fromInteger m :: Double) of
                                            r -> shiftLInteger r h#
#if __GLASGOW_HASKELL__ >= 709
-- There's already a check for negative in integerSquareRoot,
-- but integerSquareRoot' is exported directly too.
appSqrt _ = error "integerSquareRoot': negative argument"
#endif


-- Integer square root with remainder, using the Karatsuba Square Root
-- algorithm from
-- Paul Zimmermann. Karatsuba Square Root. [Research Report] RR-3805, 1999,
-- pp.8. <inria-00072854>

karatsubaSqrt :: Integer -> (Integer, Integer)
karatsubaSqrt 0 = (0, 0)
karatsubaSqrt n
    | lgN < 2300 =
        let s = isqrtA n in (s, n - s * s)
    | otherwise =
        if lgN .&. 2 /= 0 then
            karatsubaStep k (karatsubaSplit k n)
        else
            -- before we split n into 4 part we must ensure that the first part
            -- is at least 2^k/4, since this doesn't happen here we scale n by
            -- multiplying it by 4
            let n' = n `unsafeShiftL` 2
                (s, r) = karatsubaStep k (karatsubaSplit k n')
                r' | s .&. 1 == 0 = r
                   | otherwise = r + double s - 1
            in  (s `unsafeShiftR` 1, r' `unsafeShiftR` 2)
  where
    k = lgN `unsafeShiftR` 2 + 1
    lgN = integerLog2 n

karatsubaStep :: Int -> (Integer, Integer, Integer, Integer) -> (Integer, Integer)
karatsubaStep k (a3, a2, a1, a0)
    | r >= 0 = (s, r)
    | otherwise = (s - 1, r + double s - 1)
  where
    r = cat u a0 - q * q
    s = s' `unsafeShiftL` k + q
    (q, u) = cat r' a1 `quotRem` double s'
    (s', r') = karatsubaSqrt (cat a3 a2)
    cat x y = x `unsafeShiftL` k .|. y
    {-# INLINE cat #-}

karatsubaSplit :: Int -> Integer -> (Integer, Integer, Integer, Integer)
karatsubaSplit k n0 = (a3, a2, a1, a0)
  where
    a3 = n3
    n3 = n2 `unsafeShiftR` k
    a2 = n2 .&. m
    n2 = n1 `unsafeShiftR` k
    a1 = n1 .&. m
    n1 = n0 `unsafeShiftR` k
    a0 = n0 .&. m
    m = 1 `unsafeShiftL` k - 1

double :: Bits a => a -> a
double x = x `unsafeShiftL` 1
{-# INLINE double #-}
