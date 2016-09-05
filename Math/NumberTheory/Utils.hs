-- |
-- Module:      Math.NumberTheory.Utils
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Some utilities for bit twiddling.
--
{-# LANGUAGE CPP, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Utils
    ( shiftToOddCount
    , shiftToOdd
    , shiftToOdd#
    , shiftToOddCount#
    , bitCountWord
    , bitCountInt
    , bitCountWord#
    , uncheckedShiftR
    , splitOff
#if __GLASGOW_HASKELL__ < 707
    , isTrue#
#endif
    ) where

#include "MachDeps.h"

import GHC.Base

import GHC.Integer
import GHC.Integer.GMP.Internals

import Data.Bits

#if WORD_SIZE_IN_BITS == 64
#define m5 0x5555555555555555
#define m3 0x3333333333333333
#define mf 0x0F0F0F0F0F0F0F0F
#define m1 0x0101010101010101
#define sd 56
#else
#define m5 0x55555555
#define m3 0x33333333
#define mf 0x0F0F0F0F
#define m1 0x01010101
#define sd 24
#endif

uncheckedShiftR :: Word -> Int -> Word
uncheckedShiftR (W# w#) (I# i#) = W# (uncheckedShiftRL# w# i#)

-- | Remove factors of @2@ and count them. If
--   @n = 2^k*m@ with @m@ odd, the result is @(k, m)@.
--   Precondition: argument not @0@ (not checked).
{-# RULES
"shiftToOddCount/Int"       shiftToOddCount = shiftOCInt
"shiftToOddCount/Word"      shiftToOddCount = shiftOCWord
"shiftToOddCount/Integer"   shiftToOddCount = shiftOCInteger
  #-}
{-# INLINE [1] shiftToOddCount #-}
shiftToOddCount :: Integral a => a -> (Int, a)
shiftToOddCount n = case shiftOCInteger (fromIntegral n) of
                      (z, o) -> (z, fromInteger o)

-- | Specialised version for @'Word'@.
--   Precondition: argument strictly positive (not checked).
shiftOCWord :: Word -> (Int, Word)
shiftOCWord (W# w#) = case shiftToOddCount# w# of
                        (# z# , u# #) -> (I# z#, W# u#)

-- | Specialised version for @'Int'@.
--   Precondition: argument nonzero (not checked).
shiftOCInt :: Int -> (Int, Int)
shiftOCInt (I# i#) = case shiftToOddCount# (int2Word# i#) of
                        (# z#, u# #) -> (I# z#, I# (word2Int# u#))

-- | Specialised version for @'Integer'@.
--   Precondition: argument nonzero (not checked).
shiftOCInteger :: Integer -> (Int, Integer)
shiftOCInteger n@(S# i#) =
    case shiftToOddCount# (int2Word# i#) of
      (# z#, w# #)
        | isTrue# (z# ==# 0#) -> (0, n)
        | otherwise -> (I# z#, S# (word2Int# w#))
#if __GLASGOW_HASKELL__ < 709
shiftOCInteger n@(J# _ ba#) = case count 0# 0# of
#else
shiftOCInteger n@(Jp# bn#) = case bigNatZeroCount bn# of
                                 0#  -> (0, n)
                                 z#  -> (I# z#, n `shiftRInteger` z#)
shiftOCInteger n@(Jn# bn#) = case bigNatZeroCount bn# of
#endif
                                 0#  -> (0, n)
                                 z#  -> (I# z#, n `shiftRInteger` z#)
#if __GLASGOW_HASKELL__ < 709
  where
    count a# i# =
          case indexWordArray# ba# i# of
             0## -> count (a# +# WORD_SIZE_IN_BITS#) (i# +# 1#)
             w#  -> a# +# trailZeros# w#
#endif

#if __GLASGOW_HASKELL__ >= 709
-- | Count trailing zeros in a @'BigNat'@.
--   Precondition: argument nonzero (not checked, Integer invariant).
bigNatZeroCount :: BigNat -> Int#
bigNatZeroCount bn# = count 0# 0#
  where
    count a# i# =
          case indexBigNat# bn# i# of
            0## -> count (a# +# WORD_SIZE_IN_BITS#) (i# +# 1#)
            w#  -> a# +# trailZeros# w#
#endif

-- | Remove factors of @2@. If @n = 2^k*m@ with @m@ odd, the result is @m@.
--   Precondition: argument not @0@ (not checked).
{-# RULES
"shiftToOdd/Int"       shiftToOdd = shiftOInt
"shiftToOdd/Word"      shiftToOdd = shiftOWord
"shiftToOdd/Integer"   shiftToOdd = shiftOInteger
  #-}
{-# INLINE [1] shiftToOdd #-}
shiftToOdd :: Integral a => a -> a
shiftToOdd n = fromInteger (shiftOInteger (fromIntegral n))

-- | Specialised version for @'Int'@.
--   Precondition: argument nonzero (not checked).
shiftOInt :: Int -> Int
shiftOInt (I# i#) = I# (word2Int# (shiftToOdd# (int2Word# i#)))

-- | Specialised version for @'Word'@.
--   Precondition: argument nonzero (not checked).
shiftOWord :: Word -> Word
shiftOWord (W# w#) = W# (shiftToOdd# w#)

-- | Specialised version for @'Int'@.
--   Precondition: argument nonzero (not checked).
shiftOInteger :: Integer -> Integer
shiftOInteger (S# i#) = S# (word2Int# (shiftToOdd# (int2Word# i#)))
#if __GLASGOW_HASKELL__ < 709
shiftOInteger n@(J# _ ba#) = case count 0# 0# of
#else
shiftOInteger n@(Jn# bn#) = case bigNatZeroCount bn# of
                                 0#  -> n
                                 z#  -> n `shiftRInteger` z#
shiftOInteger n@(Jp# bn#) = case bigNatZeroCount bn# of
#endif
                                 0#  -> n
                                 z#  -> n `shiftRInteger` z#
#if __GLASGOW_HASKELL__ < 709
  where
    count a# i# =
          case indexWordArray# ba# i# of
            0## -> count (a# +# WORD_SIZE_IN_BITS#) (i# +# 1#)
            w#  -> a# +# trailZeros# w#
#endif

-- | Shift argument right until the result is odd.
--   Precondition: argument not @0@, not checked.
shiftToOdd# :: Word# -> Word#
shiftToOdd# w# = case trailZeros# w# of
                   k# -> uncheckedShiftRL# w# k#

-- | Like @'shiftToOdd#'@, but count the number of places to shift too.
shiftToOddCount# :: Word# -> (# Int#, Word# #)
shiftToOddCount# w# = case trailZeros# w# of
                        k# -> (# k#, uncheckedShiftRL# w# k# #)

-- | Number of 1-bits in a @'Word#'@.
bitCountWord# :: Word# -> Int#
bitCountWord# w# = case bitCountWord (W# w#) of
                     I# i# -> i#

-- | Number of 1-bits in a @'Word'@.
bitCountWord :: Word -> Int
bitCountWord = popCount

-- | Number of 1-bits in an @'Int'@.
bitCountInt :: Int -> Int
bitCountInt = popCount

-- | Number of trailing zeros in a @'Word#'@, wrong for @0@.
{-# INLINE trailZeros# #-}
trailZeros# :: Word# -> Int#
trailZeros# w =
    case xor# w (w `minusWord#` 1##) `uncheckedShiftRL#` 1# of
      v0 ->
        case v0 `minusWord#` (uncheckedShiftRL# v0 1# `and#` m5##) of
          v1 ->
            case (v1 `and#` m3##) `plusWord#` (uncheckedShiftRL# v1 2# `and#` m3##) of
              v2 ->
                case (v2 `plusWord#` uncheckedShiftRL# v2 4#) `and#` mf## of
                  v3 -> word2Int# (uncheckedShiftRL# (v3 `timesWord#` m1##) sd#)

-- {-# SPECIALISE splitOff :: Integer -> Integer -> (Int, Integer),
--                            Int -> Int -> (Int, Int),
--                            Word -> Word -> (Int, Word)
--   #-}
{-# INLINABLE splitOff #-}
splitOff :: Integral a => a -> a -> (Int, a)
splitOff p n = go 0 n
  where
    go !k m = case m `quotRem` p of
                (q,r) | r == 0 -> go (k+1) q
                      | otherwise -> (k,m)

#if __GLASGOW_HASKELL__ < 707
-- The times they are a-changing. The types of primops too :(
isTrue# :: Bool -> Bool
isTrue# = id
#endif
