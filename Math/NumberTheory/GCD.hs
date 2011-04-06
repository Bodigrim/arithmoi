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
{-# LANGUAGE BangPatterns #-}
module Math.NumberTheory.GCD
    ( binaryGCD
    , extendedGCD
    , coprime
    ) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.Base (unsafeAt, unsafeWrite)

import Math.NumberTheory.GCD.LowLevel (gcdInt, gcdWord, coprimeInt, coprimeWord)

{-# RULES
"binaryGCD/Int"     binaryGCD = gcdInt
"binaryGCD/Word"    binaryGCD = gcdWord
"binaryGCD/Int8"    forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Int8
"binaryGCD/Int16"   forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Int16
"binaryGCD/Int32"   forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Int32
"binaryGCD/Word8"   forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Word8
"binaryGCD/Word16"  forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Word16
"binaryGCD/Word32"  forall a b. binaryGCD a b = fromIntegral (gcdInt (fromIntegral a) (fromIntegral b)) :: Word32
  #-}
{-# SPECIALISE binaryGCD :: Integer -> Integer -> Integer,
                            Word64 -> Word64 -> Word64,
                            Int64 -> Int64 -> Int64 #-}
-- | Calculate the greatest common divisor using the binary gcd algorithm.
--   Depending on type and hardware, that can be considerably faster than
--   'Prelude.gcd'.
--
--   It is likely to be faster for 'Int', 'Word' and the @IntN@ and @WordN@ types for
--   @N <= 32@ since those get rewritten to the low-level functions using primops.
--
--   'Int64' and 'Word64' performance should be bearable.
--   It is very slow for 'Integer' (and probably every type except the abovementioned),
--   I recommend not using it for those.
--
--   Relies on twos complement or sign and magnitude representaion for signed types.
binaryGCD :: (Integral a, Bits a) => a -> a -> a
binaryGCD a 0 = abs a
binaryGCD 0 b = abs b
binaryGCD a b =
    case shiftToOddCount 0 a of
      (!za, !oa) ->
        case shiftToOddCount 0 b of
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
{-# SPECIALISE coprime :: Integer -> Integer -> Bool,
                          Int64 -> Int64 -> Bool,
                          Word64 -> Word64 -> Bool#-}
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
{-# SPECIALISE gcdOdd :: Integer -> Integer -> Integer,
                         Int64 -> Int64 -> Int64,
                         Word64 -> Word64 -> Word64
  #-}
{-# INLINE gcdOdd #-}
gcdOdd :: (Integral a, Bits a) => a -> a -> a
gcdOdd a b
  | a == 1 || b == 1    = 1
  | a < b               = oddGCD b a
  | a > b               = oddGCD a b
  | otherwise           = a

{-# SPECIALISE oddGCD :: Int -> Int -> Int,
                         Word -> Word -> Word,
                         Integer -> Integer -> Integer
  #-}
oddGCD :: (Integral a, Bits a) => a -> a -> a
oddGCD a b =
    case shiftToOdd (a-b) of
      1 -> 1
      c | c < b     -> oddGCD b c
        | c > b     -> oddGCD c b
        | otherwise -> c

{-# SPECIALISE shiftToOdd :: Int64 -> Int64,
                             Word64 -> Word64,
                             Integer -> Integer
  #-}
shiftToOdd :: (Integral a, Bits a) => a -> a
shiftToOdd n =
    case zeros n of
      8 -> shiftToOdd (n `shiftR` 8)
      k -> n `shiftR` k

{-# SPECIALISE shiftToOddCount :: Int -> Word64 -> (Int, Word64),
                                  Int -> Int64 -> (Int, Int64),
                                  Int -> Integer -> (Int, Integer)
  #-}
shiftToOddCount :: (Integral a, Bits a) => Int -> a -> (Int, a)
shiftToOddCount z n =
    case zeros n of
      8 -> shiftToOddCount (z+8) (n `shiftR` 8)
      k -> (z+k, n `shiftR` k)

{-# SPECIALISE zeros :: Int64 -> Int,
                        Word64 -> Int,
                        Integer -> Int
  #-}
zeros :: Integral a => a -> Int
zeros n = trailingZerosArr `unsafeAt` (fromIntegral n .&. 255)

trailingZerosArr :: UArray Int Int
trailingZerosArr = runSTUArray $ do
    arr <- newArray (0,255) 0
    unsafeWrite arr 0 8
    let fill step z idx
          | idx < 256   = unsafeWrite arr idx z >> fill step z (idx+step)
          | step < 256  = fill (2*step) (z+1) step
          | otherwise   = return arr
    fill 4 1 2
