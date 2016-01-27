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
-- Efficiently counting the number of trailing zeros, the binary gcd algorithm
-- can perform considerably faster than the Euclidean algorithm on average.
-- For 'Int', GHC has a rewrite rule to use GMP's fast gcd, depending on
-- hardware and\/or GMP version, that can be faster or slower than the binary
-- algorithm (on my 32-bit box, binary is faster, on my 64-bit box, GMP).
-- For 'Word' and the sized @IntN\/WordN@ types, there is no rewrite rule (yet)
-- in GHC, and the binary algorithm performs consistently (so far as my tests go)
-- much better (if this module's rewrite rules fire).
--
-- When using this module, always compile with optimisations turned on to
-- benefit from GHC's primops and the rewrite rules.
{-# LANGUAGE CPP, BangPatterns, MagicHash #-}
module Math.NumberTheory.GCD
    ( binaryGCD
    , extendedGCD
    , coprime
    ) where

import Data.Bits
import GHC.Word
import GHC.Int

import Math.NumberTheory.GCD.LowLevel
import Math.NumberTheory.Utils

#include "MachDeps.h"

{-# RULES
"binaryGCD/Int"     binaryGCD = gcdInt
"binaryGCD/Word"    binaryGCD = gcdWord
"binaryGCD/Int8"    binaryGCD = gi8
"binaryGCD/Int16"   binaryGCD = gi16
"binaryGCD/Int32"   binaryGCD = gi32
"binaryGCD/Word8"   binaryGCD = gw8
"binaryGCD/Word16"  binaryGCD = gw16
"binaryGCD/Word32"  binaryGCD = gw32
  #-}
#if WORD_SIZE_IN_BITS == 64
gi64 :: Int64 -> Int64 -> Int64
gi64 (I64# x#) (I64# y#) = I64# (gcdInt# x# y#)

gw64 :: Word64 -> Word64 -> Word64
gw64 (W64# x#) (W64# y#) = W64# (gcdWord# x# y#)

{-# RULES
"binaryGCD/Int64"   binaryGCD = gi64
"binaryGCD/Word64"  binaryGCD = gw64
  #-}
#endif
{-# INLINE [1] binaryGCD #-}
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
binaryGCD = binaryGCDImpl

#if WORD_SIZE_IN_BITS < 64
{-# SPECIALISE binaryGCDImpl :: Word64 -> Word64 -> Word64,
                                Int64 -> Int64 -> Int64 #-}
#endif
{-# SPECIALISE binaryGCDImpl :: Integer -> Integer -> Integer #-}
binaryGCDImpl :: (Integral a, Bits a) => a -> a -> a
binaryGCDImpl a 0 = abs a
binaryGCDImpl 0 b = abs b
binaryGCDImpl a b =
    case shiftToOddCount a' of
      (!za, !oa) ->
        case shiftToOddCount b' of
          (!zb, !ob) -> gcdOdd (abs oa) (abs ob) `shiftL` min za zb
    where
      a' = abs a
      b' = abs b

{-# SPECIALISE extendedGCD :: Int -> Int -> (Int, Int, Int),
                              Word -> Word -> (Word, Word, Word),
                              Integer -> Integer -> (Integer, Integer, Integer)
  #-}
-- | Calculate the greatest common divisor of two numbers and coefficients
--   for the linear combination.
--
--   For signed types satisfies:
--
-- > case extendedGCD a b of
-- >   (d, u, v) -> u*a + v*b == d
-- >                && d == gcd a b
--
--   For unsigned and bounded types the property above holds, but since @u@ and @v@ must also be unsigned,
--   the result may look weird. E. g., on 64-bit architecture
--
-- > extendedGCD (2 :: Word) (3 :: Word) == (1, 2^64-1, 1)
--
--   For unsigned and unbounded types (like 'Numeric.Natural.Natural') the result is undefined.
--
--   For signed types we also have
--
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
"coprime/Int8"      coprime = ci8
"coprime/Int16"     coprime = ci16
"coprime/Int32"     coprime = ci32
"coprime/Word8"     coprime = cw8
"coprime/Word16"    coprime = cw16
"coprime/Word32"    coprime = cw32
  #-}
#if WORD_SIZE_IN_BITS == 64
ci64 :: Int64 -> Int64 -> Bool
ci64 (I64# x#) (I64# y#) = coprimeInt# x# y#

cw64 :: Word64 -> Word64 -> Bool
cw64 (W64# x#) (W64# y#) = coprimeWord# x# y#

{-# RULES
"coprime/Int64"     coprime = ci64
"coprime/Word64"    coprime = cw64
  #-}
#endif
{-# INLINE [1] coprime #-}
-- | Test whether two numbers are coprime using an abbreviated binary gcd algorithm.
--   A little bit faster than checking @binaryGCD a b == 1@ if one of the arguments
--   is even, much faster if both are even.
--
--   The remarks about performance at 'binaryGCD' apply here too, use this function
--   only at the types with rewrite rules.
--
--   Relies on twos complement or sign and magnitude representaion for signed types.
coprime :: (Integral a, Bits a) => a -> a -> Bool
coprime = coprimeImpl

-- Separate implementation to give the rules a chance to fire by not inlining
-- before phase 1, and yet have a specialisation for the types without rules
#if WORD_SIZE_IN_BITS < 64
{-# SPECIALISE coprimeImpl :: Word64 -> Word64 -> Bool,
                              Int64 -> Int64 -> Bool #-}
#endif
{-# SPECIALISE coprimeImpl :: Integer -> Integer -> Bool #-}
coprimeImpl :: (Integral a, Bits a) => a -> a -> Bool
coprimeImpl a b =
  (a' == 1 || b' == 1)
  || (a' /= 0 && b' /= 0 && ((a .|. b) .&. 1) == 1
      && gcdOdd (abs (shiftToOdd a')) (abs (shiftToOdd b')) == 1)
    where
      a' = abs a
      b' = abs b

-- Auxiliaries

-- gcd of two odd numbers
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

-------------------------------------------------------------------------------
--                Blech! Getting the rules to fire isn't easy.               --
-------------------------------------------------------------------------------

gi8 :: Int8 -> Int8 -> Int8
gi8 (I8# x#) (I8# y#) = I8# (gcdInt# x# y#)

gi16 :: Int16 -> Int16 -> Int16
gi16 (I16# x#) (I16# y#) = I16# (gcdInt# x# y#)

gi32 :: Int32 -> Int32 -> Int32
gi32 (I32# x#) (I32# y#) = I32# (gcdInt# x# y#)

gw8 :: Word8 -> Word8 -> Word8
gw8 (W8# x#) (W8# y#) = W8# (gcdWord# x# y#)

gw16 :: Word16 -> Word16 -> Word16
gw16 (W16# x#) (W16# y#) = W16# (gcdWord# x# y#)

gw32 :: Word32 -> Word32 -> Word32
gw32 (W32# x#) (W32# y#) = W32# (gcdWord# x# y#)

ci8 :: Int8 -> Int8 -> Bool
ci8 (I8# x#) (I8# y#) = coprimeInt# x# y#

ci16 :: Int16 -> Int16 -> Bool
ci16 (I16# x#) (I16# y#) = coprimeInt# x# y#

ci32 :: Int32 -> Int32 -> Bool
ci32 (I32# x#) (I32# y#) = coprimeInt# x# y#

cw8 :: Word8 -> Word8 -> Bool
cw8 (W8# x#) (W8# y#) = coprimeWord# x# y#

cw16 :: Word16 -> Word16 -> Bool
cw16 (W16# x#) (W16# y#) = coprimeWord# x# y#

cw32 :: Word32 -> Word32 -> Bool
cw32 (W32# x#) (W32# y#) = coprimeWord# x# y#
