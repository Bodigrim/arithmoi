-- |
-- Module:      Math.NumberTheory.Powers.Squares
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Functions dealing with fourth powers. Efficient calculation of integer fourth
-- roots and efficient testing for being a square's square.
{-# LANGUAGE MagicHash, CPP, FlexibleContexts #-}
module Math.NumberTheory.Powers.Fourth
    ( integerFourthRoot
    , integerFourthRoot'
    , exactFourthRoot
    , isFourthPower
    , isFourthPower'
    , isPossibleFourthPower
    ) where

#include "MachDeps.h"

import GHC.Base
import GHC.Integer
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms (integerLog2#)

import Data.Array.Unboxed
import Data.Array.ST

import Data.Bits

import Math.NumberTheory.Unsafe
#if __GLASGOW_HASKELL__ < 707
import Math.NumberTheory.Utils (isTrue#)
#endif

-- | Calculate the integer fourth root of a nonnegative number,
--   that is, the largest integer @r@ with @r^4 <= n@.
--   Throws an error on negaitve input.
{-# SPECIALISE integerFourthRoot :: Int -> Int,
                                    Integer -> Integer,
                                    Word -> Word
  #-}
integerFourthRoot :: Integral a => a -> a
integerFourthRoot n
    | n < 0     = error "integerFourthRoot: negative argument"
    | otherwise = integerFourthRoot' n

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
                                  Integer -> Maybe Integer,
                                  Word -> Maybe Word
  #-}
exactFourthRoot :: Integral a => a -> Maybe a
exactFourthRoot 0 = Just 0
exactFourthRoot n
    | n < 0     = Nothing
    | isPossibleFourthPower n && r2*r2 == n = Just r
    | otherwise = Nothing
      where
        r = integerFourthRoot' n
        r2 = r*r

-- | Test whether an integer is a fourth power.
--   First nonnegativity is checked, then the unchecked
--   test is called.
{-# SPECIALISE isFourthPower :: Int -> Bool,
                                Integer -> Bool,
                                Word -> Bool
  #-}
isFourthPower :: Integral a => a -> Bool
isFourthPower 0 = True
isFourthPower n = n > 0 && isFourthPower' n

-- | Test whether a nonnegative number is a fourth power.
--   The condition is /not/ checked. If a number passes the
--   'isPossibleFourthPower' test, its integer fourth root
--   is calculated.
{-# SPECIALISE isFourthPower' :: Int -> Bool,
                                 Integer -> Bool,
                                 Word -> Bool
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
                                        Integer -> Bool,
                                        Word -> Bool
  #-}
isPossibleFourthPower :: Integral a => a -> Bool
isPossibleFourthPower n =
        biSqRes256 `unsafeAt` (fromIntegral n .&. 255)
      && biSqRes425 `unsafeAt` (fromIntegral (n `rem` 425))
      && biSqRes377 `unsafeAt` (fromIntegral (n `rem` 377))

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
#if __GLASGOW_HASKELL__ < 709
appBiSqrt n@(J# s# _)
    | isTrue# (s# <# THRESH#)   = floor (sqrt . sqrt $ fromInteger n :: Double)
#else
appBiSqrt n@(Jp# bn#)
    | isTrue# ((sizeofBigNat# bn#) <# THRESH#) =
          floor (sqrt . sqrt $ fromInteger n :: Double)
#endif
    | otherwise = case integerLog2# n of
                    l# -> case uncheckedIShiftRA# l# 2# -# 47# of
                            h# -> case shiftRInteger n (4# *# h#) of
                                    m -> case floor (sqrt $ sqrt $ fromInteger m :: Double) of
                                            r -> shiftLInteger r h#
#if __GLASGOW_HASKELL__ >= 709
-- There's already a check for negative in integerFourthRoot,
-- but integerFourthRoot' is exported directly too.
appBiSqrt _ = error "integerFourthRoot': negative argument"
#endif


biSqRes256 :: UArray Int Bool
biSqRes256 = runSTUArray $ do
    ar <- newArray (0,255) False
    let note 257 = return ar
        note i = unsafeWrite ar i True >> note (i+16)
    unsafeWrite ar 0 True
    unsafeWrite ar 16 True
    note 1

biSqRes425 :: UArray Int Bool
biSqRes425 = runSTUArray $ do
    ar <- newArray (0,424) False
    let note 154 = return ar
        note i = unsafeWrite ar ((i*i*i*i) `rem` 425) True >> note (i+1)
    note 0

biSqRes377 :: UArray Int Bool
biSqRes377 = runSTUArray $ do
    ar <- newArray (0,376) False
    let note 144 = return ar
        note i = unsafeWrite ar ((i*i*i*i) `rem` 377) True >> note (i+1)
    note 0

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
