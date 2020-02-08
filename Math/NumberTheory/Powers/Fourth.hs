-- |
-- Module:      Math.NumberTheory.Powers.Squares
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Description: Deprecated
--
-- Functions dealing with fourth powers. Efficient calculation of integer fourth
-- roots and efficient testing for being a square's square.

{-# LANGUAGE MagicHash, CPP, FlexibleContexts #-}

module Math.NumberTheory.Powers.Fourth
  {-# DEPRECATED "Use Math.NumberTheory.Roots" #-}
    ( integerFourthRoot
    , integerFourthRoot'
    , exactFourthRoot
    , isFourthPower
    , isFourthPower'
    , isPossibleFourthPower
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

-- | Calculate the integer fourth root of a nonnegative number,
--   that is, the largest integer @r@ with @r^4 <= n@.
--   Throws an error on negaitve input.
{-# SPECIALISE integerFourthRoot :: Int -> Int,
                                    Word -> Word,
                                    Integer -> Integer,
                                    Natural -> Natural
  #-}
integerFourthRoot :: Integral a => a -> a
integerFourthRoot = integerRoot (4 :: Word)

-- | Calculate the integer fourth root of a nonnegative number,
--   that is, the largest integer @r@ with @r^4 <= n@.
--   The condition is /not/ checked.
{-# RULES
"integerFourthRoot'/Int"     integerFourthRoot' = biSqrtInt
"integerFourthRoot'/Word"    integerFourthRoot' = biSqrtWord
"integerFourthRoot'/Integer" integerFourthRoot' = biSqrtIgr
  #-}
{-# INLINE [1] integerFourthRoot' #-}
integerFourthRoot' :: Integral a => a -> a
integerFourthRoot' 0 = 0
integerFourthRoot' n = newton4 n (approxBiSqrt n)

-- | Returns @Nothing@ if @n@ is not a fourth power,
--   @Just r@ if @n == r^4@ and @r >= 0@.
{-# SPECIALISE exactFourthRoot :: Int -> Maybe Int,
                                  Word -> Maybe Word,
                                  Integer -> Maybe Integer,
                                  Natural -> Maybe Natural
  #-}
exactFourthRoot :: Integral a => a -> Maybe a
exactFourthRoot = exactRoot (4 :: Word)

-- | Test whether an integer is a fourth power.
--   First nonnegativity is checked, then the unchecked
--   test is called.
{-# SPECIALISE isFourthPower :: Int -> Bool,
                                Word -> Bool,
                                Integer -> Bool,
                                Natural -> Bool
  #-}
isFourthPower :: Integral a => a -> Bool
isFourthPower = isKthPower (4 :: Word)

-- | Test whether a nonnegative number is a fourth power.
--   The condition is /not/ checked. If a number passes the
--   'isPossibleFourthPower' test, its integer fourth root
--   is calculated.
{-# SPECIALISE isFourthPower' :: Int -> Bool,
                                 Word -> Bool,
                                 Integer -> Bool,
                                 Natural -> Bool
  #-}
isFourthPower' :: Integral a => a -> Bool
isFourthPower' n = isPossibleFourthPower n && r2*r2 == n
  where
    r = integerFourthRoot' n
    r2 = r*r

-- | Test whether a nonnegative number is a possible fourth power.
--   The condition is /not/ checked.
--   This eliminates about 99.958% of numbers.
{-# SPECIALISE isPossibleFourthPower :: Int -> Bool,
                                        Word -> Bool,
                                        Integer -> Bool,
                                        Natural -> Bool
  #-}
isPossibleFourthPower :: Integral a => a -> Bool
isPossibleFourthPower n
  =  V.unsafeIndex biSqRes256 (fromIntegral n .&. 255)
  && V.unsafeIndex biSqRes425 (fromIntegral (n `rem` 425))
  && V.unsafeIndex biSqRes377 (fromIntegral (n `rem` 377))

{-# SPECIALISE newton4 :: Integer -> Integer -> Integer #-}
newton4 :: Integral a => a -> a -> a
newton4 n a = go (step a)
      where
        step k = (3*k + n `quot` (k*k*k)) `quot` 4
        go k
            | m < k     = go m
            | otherwise = k
              where
                m = step k

{-# SPECIALISE approxBiSqrt :: Integer -> Integer #-}
approxBiSqrt :: Integral a => a -> a
approxBiSqrt = fromInteger . appBiSqrt . fromIntegral

-- threshold for shifting vs. direct fromInteger
-- we shift when we expect more than 384 bits
#if WORD_SIZE_IN_BITS == 64
#define THRESH 7
#else
#define THRESH 13
#endif

-- Find a fairly good approximation to the fourth root.
-- About 48 bits should be correct for large Integers.
appBiSqrt :: Integer -> Integer
appBiSqrt (S# i#) = S# (double2Int# (sqrtDouble# (sqrtDouble# (int2Double# i#))))
appBiSqrt n@(Jp# bn#)
    | isTrue# ((sizeofBigNat# bn#) <# THRESH#) =
          floor (sqrt . sqrt $ fromInteger n :: Double)
    | otherwise = case integerLog2# n of
                    l# -> case uncheckedIShiftRA# l# 2# -# 47# of
                            h# -> case shiftRInteger n (4# *# h#) of
                                    m -> case floor (sqrt $ sqrt $ fromInteger m :: Double) of
                                            r -> shiftLInteger r h#
-- There's already a check for negative in integerFourthRoot,
-- but integerFourthRoot' is exported directly too.
appBiSqrt _ = error "integerFourthRoot': negative argument"


biSqRes256 :: V.Vector Bool
biSqRes256 = runST $ do
    ar <- MV.replicate 256 False
    let note 257 = return ()
        note i = MV.unsafeWrite ar i True >> note (i+16)
    MV.unsafeWrite ar 0 True
    MV.unsafeWrite ar 16 True
    note 1
    V.unsafeFreeze ar

biSqRes425 :: V.Vector Bool
biSqRes425 = runST $ do
    ar <- MV.replicate 425 False
    let note 154 = return ()
        note i = MV.unsafeWrite ar ((i*i*i*i) `rem` 425) True >> note (i+1)
    note 0
    V.unsafeFreeze ar

biSqRes377 :: V.Vector Bool
biSqRes377 = runST $ do
    ar <- MV.replicate 377 False
    let note 144 = return ()
        note i = MV.unsafeWrite ar ((i*i*i*i) `rem` 377) True >> note (i+1)
    note 0
    V.unsafeFreeze ar

biSqrtInt :: Int -> Int
biSqrtInt 0 = 0
biSqrtInt n
#if WORD_SIZE_IN_BITS == 64
    | r > 55108 = 55108
#else
    | r > 215   = 215
#endif
    | n < r4    = r-1
    | otherwise = r
      where
        x :: Double
        x = fromIntegral n
        -- timed faster than x**0.25, never too small
        r = truncate (sqrt (sqrt x))
        r2 = r*r
        r4 = r2*r2

biSqrtWord :: Word -> Word
biSqrtWord 0 = 0
biSqrtWord n
#if WORD_SIZE_IN_BITS == 64
    | r > 65535 = 65535
#else
    | r > 255   = 255
#endif
    | n < r4    = r-1
    | otherwise = r
      where
        x :: Double
        x = fromIntegral n
        r = truncate (sqrt (sqrt x))
        r2 = r*r
        r4 = r2*r2

biSqrtIgr :: Integer -> Integer
biSqrtIgr 0 = 0
biSqrtIgr n = newton4 n (approxBiSqrt n)
