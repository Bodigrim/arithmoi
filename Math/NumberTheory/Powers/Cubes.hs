-- |
-- Module:      Math.NumberTheory.Powers.Cubes
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Description: Deprecated
--
-- Functions dealing with cubes. Moderately efficient calculation of integer
-- cube roots and testing for cubeness.

{-# LANGUAGE MagicHash, BangPatterns, CPP, FlexibleContexts #-}

module Math.NumberTheory.Powers.Cubes
  {-# DEPRECATED "Use Math.NumberTheory.Roots" #-}
    ( integerCubeRoot
    , integerCubeRoot'
    , exactCubeRoot
    , isCube
    , isCube'
    , isPossibleCube
    ) where

#include "MachDeps.h"

import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import GHC.Base
import GHC.Integer
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms (integerLog2#)

import Numeric.Natural

import Math.NumberTheory.Roots

-- | Calculate the integer cube root of a nonnegative integer @n@,
--   that is, the largest integer @r@ such that @r^3 <= n@.
--   The precondition @n >= 0@ is not checked.
{-# RULES
"integerCubeRoot'/Int"     integerCubeRoot' = cubeRootInt'
"integerCubeRoot'/Word"    integerCubeRoot' = cubeRootWord
"integerCubeRoot'/Integer" integerCubeRoot' = cubeRootIgr
  #-}
{-# INLINE [1] integerCubeRoot' #-}
integerCubeRoot' :: Integral a => a -> a
integerCubeRoot' 0 = 0
integerCubeRoot' n = newton3 n (approxCuRt n)

-- | Test whether a nonnegative integer is a cube.
--   Before 'integerCubeRoot' is calculated, a few tests
--   of remainders modulo small primes weed out most non-cubes.
--   For testing many numbers, most of which aren't cubes,
--   this is much faster than @let r = cubeRoot n in r*r*r == n@.
--   The condition @n >= 0@ is /not/ checked.
{-# SPECIALISE isCube' :: Int -> Bool,
                          Word -> Bool,
                          Integer -> Bool,
                          Natural -> Bool
  #-}
isCube' :: Integral a => a -> Bool
isCube' !n = isPossibleCube n
             && (r*r*r == n)
      where
        r    = integerCubeRoot' n

-- | Test whether a nonnegative number is possibly a cube.
--   Only about 0.08% of all numbers pass this test.
--   The precondition @n >= 0@ is /not/ checked.
{-# SPECIALISE isPossibleCube :: Int -> Bool,
                                 Word -> Bool,
                                 Integer -> Bool,
                                 Natural -> Bool
  #-}
isPossibleCube :: Integral a => a -> Bool
isPossibleCube !n
    =  V.unsafeIndex cr512 (fromIntegral n .&. 511)
    && V.unsafeIndex cubeRes837 (fromIntegral (n `rem` 837))
    && V.unsafeIndex cubeRes637 (fromIntegral (n `rem` 637))
    && V.unsafeIndex cubeRes703 (fromIntegral (n `rem` 703))

----------------------------------------------------------------------
--                         Utility Functions                        --
----------------------------------------------------------------------

-- Special case for 'Int', a little faster.
-- For @n <= 2^64@, the truncated 'Double' is never
-- more than one off. Things might overflow for @n@
-- close to @maxBound@, so check for overflow.
cubeRootInt' :: Int -> Int
cubeRootInt' 0 = 0
cubeRootInt' n
    | n < c || c < 0    = r-1
    | 0 < d && d < n    = r+1
    | otherwise         = r
      where
        x = fromIntegral n :: Double
        r = truncate (x ** (1/3))
        c = r*r*r
        d = c+3*r*(r+1)

cubeRootWord :: Word -> Word
cubeRootWord 0 = 0
cubeRootWord w
#if WORD_SIZE_IN_BITS == 64
    | r > 2642245       = 2642245
#else
    | r > 1625          = 1625
#endif
    | w < c             = r-1
    | c < w && e < w && c < e  = r+1
    | otherwise         = r
      where
        r = truncate ((fromIntegral w) ** (1/3) :: Double)
        c = r*r*r
        d = 3*r*(r+1)
        e = c+d

cubeRootIgr :: Integer -> Integer
cubeRootIgr 0 = 0
cubeRootIgr n = newton3 n (approxCuRt n)

{-# SPECIALISE newton3 :: Integer -> Integer -> Integer #-}
newton3 :: Integral a => a -> a -> a
newton3 n a = go (step a)
      where
        step k = (2*k + n `quot` (k*k)) `quot` 3
        go k
            | m < k     = go m
            | otherwise = k
              where
                m = step k

{-# SPECIALISE approxCuRt :: Integer -> Integer #-}
approxCuRt :: Integral a => a -> a
approxCuRt 0 = 0
approxCuRt n = fromInteger $ appCuRt (fromIntegral n)

-- threshold for shifting vs. direct fromInteger
-- we shift when we expect more than 256 bits
#if WORD_SIZE_IN_BITS == 64
#define THRESH 5
#else
#define THRESH 9
#endif

-- | approximate cube root, about 50 bits should be correct for large numbers
appCuRt :: Integer -> Integer
appCuRt (S# i#) = case double2Int# (int2Double# i# **## (1.0## /## 3.0##)) of
                    r# -> S# r#
appCuRt n@(Jp# bn#)
    | isTrue# ((sizeofBigNat# bn#) <# THRESH#) =
          floor (fromInteger n ** (1.0/3.0) :: Double)
    | otherwise = case integerLog2# n of
                    l# -> case (l# `quotInt#` 3#) -# 51# of
                            h# -> case shiftRInteger n (3# *# h#) of
                                    m -> case floor (fromInteger m ** (1.0/3.0) :: Double) of
                                           r -> shiftLInteger r h#
-- There's already handling for negative in integerCubeRoot,
-- but integerCubeRoot' is exported directly too.
appCuRt _ = error "integerCubeRoot': negative argument"

-- not very discriminating, but cheap, so it's an overall gain
cr512 :: V.Vector Bool
cr512 = runST $ do
    ar <- MV.replicate 512 True
    let note s i
            | i < 512   = MV.unsafeWrite ar i False >> note s (i+s)
            | otherwise = return ()
    note 4 2
    note 8 4
    note 32 16
    note 64 32
    note 256 128
    MV.unsafeWrite ar 256 False
    V.unsafeFreeze ar

-- Remainders modulo @3^3 * 31@
cubeRes837 :: V.Vector Bool
cubeRes837 = runST $ do
    ar <- MV.replicate 837 False
    let note 837 = return ()
        note k = MV.unsafeWrite ar ((k*k*k) `rem` 837) True >> note (k+1)
    note 0
    V.unsafeFreeze ar

-- Remainders modulo @7^2 * 13@
cubeRes637 :: V.Vector Bool
cubeRes637 = runST $ do
    ar <- MV.replicate 637 False
    let note 637 = return ()
        note k = MV.unsafeWrite ar ((k*k*k) `rem` 637) True >> note (k+1)
    note 0
    V.unsafeFreeze ar

-- Remainders modulo @19 * 37@
cubeRes703 :: V.Vector Bool
cubeRes703 = runST $ do
    ar <- MV.replicate 703 False
    let note 703 = return ()
        note k = MV.unsafeWrite ar ((k*k*k) `rem` 703) True >> note (k+1)
    note 0
    V.unsafeFreeze ar
