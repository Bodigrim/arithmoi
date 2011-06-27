-- |
-- Module:      Math.NumberTheory.GCD
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module exports GCD and coprimality test using the binary gcd algorithm
-- and GCD with the extended Euclidean algorithm.
--
-- Unlike the "Prelude", this module uses @gcd 0 0 = 0@.
--
-- Using a lookup table for the number of trailing zeros, the binary gcd algorithm
-- can perform considerably faster than the Euclidean algorithm on average,
-- the performance relations are very hardware-dependent though.
--
-- When using this module, always compile with optimisations turned on to
-- benefit from GHC's primops.
{-# LANGUAGE CPP, BangPatterns #-}
module Math.NumberTheory.GCD
    ( binaryGCD
    , extendedGCD
    , coprime
    ) where

import Data.Bits
import Data.Word
import Data.Int

import Math.NumberTheory.GCD.LowLevel (gcdInt, gcdWord, coprimeInt, coprimeWord)
import Math.NumberTheory.Utils

#include "MachDeps.h"

{-# RULES
"binaryGCD/Int"     binaryGCD = gcdInt
"binaryGCD/Word"    binaryGCD = gcdWord
"binaryGCD/Int8"    forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Int8
"binaryGCD/Int16"   forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Int16
"binaryGCD/Int32"   forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Int32
"binaryGCD/Word8"   forall a b. binaryGCD a b = fromIntegral (gcdWord (fromIntegral a) (fromIntegral b)) :: Word8
"binaryGCD/Word16"  forall a b. binaryGCD a b = fromIntegral (gcdWord (fromIntegral a) (fromIntegral b)) :: Word16
"binaryGCD/Word32"  forall a b. binaryGCD a b = fromIntegral (gcdWord (fromIntegral a) (fromIntegral b)) :: Word32
  #-}
#if WORD_SIZE_IN_BITS == 64
{-# RULES
"binaryGCD/Int64"   forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Int64
"binaryGCD/Word64"  forall a b. binaryGCD a b = fromIntegral (gcdWord (fromIntegral a) (fromIntegral b)) :: Word64
  #-}
#else
{-# SPECIALISE binaryGCD :: Word64 -> Word64 -> Word64,
                            Int64 -> Int64 -> Int64 #-}
#endif
{-# SPECIALISE binaryGCD :: Integer -> Integer -> Integer #-}
-- | Calculate the greatest common divisor using the binary gcd algorithm.
--   Depending on type and hardware, that can be considerably faster than
--   @'Prelude.gcd'@ but it may also be significantly slower.
--
--   There are specialised functions for @'Int'@ and @'Word'@ and rewrite rules
--   for those and @IntN@ and @WordN@, @N <= WORD_SIZE_IN_BITS@, to use the
--   specialised variants. These types are worth benchmarking, others probably not.
--
--   It is very slow for 'Integer' (and probably every type except the abovementioned),
--   I recommend not using it for those.
--
--   Relies on twos complement or sign and magnitude representaion for signed types.
binaryGCD :: (Integral a, Bits a) => a -> a -> a
binaryGCD a 0 = abs a
binaryGCD 0 b = abs b
binaryGCD a b =
    case shiftToOddCount a of
      (!za, !oa) ->
        case shiftToOddCount b of
          (!zb, !ob) -> gcdOdd (abs oa) (abs ob) `shiftL` min za zb

{-# SPECIALISE extendedGCD :: Int -> Int -> (Int, Int, Int),
                              Word -> Word -> (Word, Word, Word),
                              Integer -> Integer -> (Integer, Integer, Integer)
  #-}
-- | Calculate the greatest common divisor of two numbers and coefficients
--   for the linear combination.
--
--   Satisfies:
--
-- > case extendedGCD a b of
-- >   (d, u, v) -> u*a + v*b == d
-- >
-- > d == gcd a b
-- >
-- > abs u < abs b || abs b <= 1
-- >
-- > abs v < abs a || abs a <= 1
--
--   (except if one of @a@ and @b@ is 'minBound' of a signed type).
extendedGCD :: Integral a => a -> a -> (a, a, a)
extendedGCD a b = (d, u, v)
  where
    (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
    u | a < 0     = negate x
      | otherwise = x
    v | b < 0     = negate y
      | otherwise = y
    eGCD !n1 o1 !n2 o2 r s
      | s == 0    = (r, o1, o2)
      | otherwise = case r `quotRem` s of
                      (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t

{-# RULES
"coprime/Int"       coprime = coprimeInt
"coprime/Word"      coprime = coprimeWord
"coprime/Int8"      forall a b. coprime a (b :: Int8)  = coprimeInt (fromIntegral a) (fromIntegral b)
"coprime/Int16"     forall a b. coprime a (b :: Int16) = coprimeInt (fromIntegral a) (fromIntegral b)
"coprime/Int32"     forall a b. coprime a (b :: Int32) = coprimeInt (fromIntegral a) (fromIntegral b)
"coprime/Word8"     forall a b. coprime a (b :: Word8)  = coprimeWord (fromIntegral a) (fromIntegral b)
"coprime/Word16"    forall a b. coprime a (b :: Word16) = coprimeWord (fromIntegral a) (fromIntegral b)
"coprime/Word32"    forall a b. coprime a (b :: Word32) = coprimeWord (fromIntegral a) (fromIntegral b)
  #-}
#if WORD_SIZE_IN_BITS == 64
{-# RULES
"coprime/Int64"     forall a b. coprime a (b :: Int64) = coprimeInt (fromIntegral a) (fromIntegral b)
"coprime/Word64"    forall a b. coprime a (b :: Word64) = coprimeWord (fromIntegral a) (fromIntegral b)
  #-}
#else
{-# SPECIALISE coprime :: Word64 -> Word64 -> Bool,
                          Int64 -> Int64 -> Bool #-}
#endif
{-# SPECIALISE coprime :: Integer -> Integer -> Bool #-}
-- | Test whether two numbers are coprime using an abbreviated binary gcd algorithm.
--   A little bit faster than checking @binaryGCD a b == 1@ if one of the arguments
--   is even, much faster if both are even.
--
--   The remarks about performance at 'binaryGCD' apply here too, use this function
--   only at the types with rewrite rules.
--
--   Relies on twos complement or sign and magnitude representaion for signed types.
coprime :: (Integral a, Bits a) => a -> a -> Bool
coprime a b =
  (a' == 1 || b' == 1)
  || (a' /= 0 && b' /= 0 && ((a .|. b) .&. 1) == 1
      && gcdOdd (abs (shiftToOdd a')) (abs (shiftToOdd b')) == 1)
    where
      a' = abs a
      b' = abs b

-- Auxiliaries

-- gcd of two odd numbers
{-# SPECIALISE gcdOdd :: Integer -> Integer -> Integer #-}
#if WORD_SIZE_IN_BITS < 64
{-# SPECIALISE gcdOdd :: Int64 -> Int64 -> Int64,
                         Word64 -> Word64 -> Word64
  #-}
#endif
{-# INLINE gcdOdd #-}
gcdOdd :: (Integral a, Bits a) => a -> a -> a
gcdOdd a b
  | a == 1 || b == 1    = 1
  | a < b               = oddGCD b a
  | a > b               = oddGCD a b
  | otherwise           = a

{-# SPECIALISE oddGCD :: Integer -> Integer -> Integer #-}
#if WORD_SIZE_IN_BITS < 64
{-# SPECIALISE oddGCD :: Int64 -> Int64 -> Int64,
                         Word64 -> Word64 -> Word64
  #-}
#endif
oddGCD :: (Integral a, Bits a) => a -> a -> a
oddGCD a b =
    case shiftToOdd (a-b) of
      1 -> 1
      c | c < b     -> oddGCD b c
        | c > b     -> oddGCD c b
        | otherwise -> c
