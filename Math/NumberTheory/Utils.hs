-- |
-- Module:      Math.NumberTheory.Utils
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
--
-- Some utilities for bit twiddling.
{-# LANGUAGE CPP, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Utils
    ( shiftToOddCount
    , shiftToOdd#
    , shiftToOddCount#
    , bitCountWord
    , bitCountInt
    , bitCountWord#
    ) where

#include "MachDeps.h"

import GHC.Base
import GHC.Word

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


-- | Remove factors of @2@ and count them. If
--   @n = 2^k*m@ with @m@ odd, the result is @(k, m)@.
--   Precondition: argument strictly positive (not checked).
{-# RULES
"shiftToOddCount/Int"       shiftToOddCount = shiftOCInt
"shiftToOddCount/Word"      shiftToOddCount = shiftOCWord
"shiftToOddCount/Integer"   shiftToOddCount = shiftOCInteger
  #-}
shiftToOddCount :: (Integral a, Bits a) => a -> (Int, a)
shiftToOddCount n = case shiftOCInteger (fromIntegral n) of
                      (z, o) -> (z, fromInteger o)

-- | Specialised version for @'Word'@.
--   Precondition: argument strictly positive (not checked).
shiftOCWord :: Word -> (Int, Word)
shiftOCWord (W# w#) = case shiftToOddCount# 0# w# of
                        (# z# , u# #) -> (I# z#, W# u#)

-- | Specialised version for @'Int'@.
--   Precondition: argument strictly positive (not checked).
shiftOCInt :: Int -> (Int, Int)
shiftOCInt (I# i#) = case shiftToOddCount# 0# (int2Word# i#) of
                        (# z#, u# #) -> (I# z#, I# (word2Int# u#))

-- | Specialised version for @'Integer'@.
--   Precondition: argument strictly positive (not checked).
shiftOCInteger :: Integer -> (Int, Integer)
shiftOCInteger n@(S# i#) =
    case shiftToOddCount# 0# (int2Word# i#) of
      (# z#, w# #)
        | z# ==# 0# -> (0, n)
        | otherwise -> (I# z#, S# (word2Int# w#))
shiftOCInteger n@(J# _ ba#) = case count 0# 0# of
                                 0#  -> (0, n)
                                 z#  -> (I# z#, n `shiftRInteger` z#)
  where
    count a# i# =
          case indexWordArray# ba# i# of
            0## -> count (a# +# WORD_SIZE_IN_BITS#) (i# +# 1#)
            w#  -> case shiftToOddCount# a# w# of
                    (# z#, _ #) -> z#


-- | Shift argument right until the result is odd.
--   Precondition: argument not @0@, not checked.
shiftToOdd# :: Word# -> Word#
shiftToOdd# w# =
    case zeroCountArr of
      BA arr# ->
        case indexInt8Array# arr# (word2Int# (w# `and#` 255##)) of
          8# -> shiftToOdd# (w# `uncheckedShiftRL#` 8#)
          k# -> w# `uncheckedShiftRL#` k#

-- | Like @'shiftToOdd#'@, but count the number of places to shift too.
--   First argument is number of places already shifted.
shiftToOddCount# :: Int# -> Word# -> (# Int#, Word# #)
shiftToOddCount# z# w# =
    case zeroCountArr of
      BA arr# ->
        case indexInt8Array# arr# (word2Int# (w# `and#` 255##)) of
          8# -> shiftToOddCount# (z# +# 8#) (w# `uncheckedShiftRL#` 8#)
          k# -> (# z# +# k#, w# `uncheckedShiftRL#` k# #)

-- | Number of 1-bits in a @'Word#'@.
bitCountWord# :: Word# -> Int#
bitCountWord# w# = case bitCountWord (W# w#) of
                     I# i# -> i#

-- | Number of 1-bits in a @'Word'@.
bitCountWord :: Word -> Int
bitCountWord w = case w - (shiftR w 1 .&. m5) of
                   !w1 -> case (w1 .&. m3) + (shiftR w1 2 .&. m3) of
                            !w2 -> case (w2 + shiftR w2 4) .&. mf of
                                     !w3 -> fromIntegral (shiftR (w3 * m1) sd)

-- | Number of 1-bits in an @'Int'@.
bitCountInt :: Int -> Int
bitCountInt = bitCountWord . fromIntegral

-- | Datatype for the lookup table.
data BA = BA ByteArray#

-- | Number of trailing zero bits in a byte.
zeroCountArr :: BA
zeroCountArr =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 8# s1 of
                s2 ->
                  let fillA step val idx st
                        | idx <# 256# = case writeInt8Array# mba idx val st of
                                          nx -> fillA step val (idx +# step) nx
                        | step <# 256# = fillA (2# *# step) (val +# 1#) step  st
                        | otherwise   = st
                  in case fillA 2# 0# 1# s2 of
                       s3 -> case unsafeFreezeByteArray# mba s3 of
                                (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b
