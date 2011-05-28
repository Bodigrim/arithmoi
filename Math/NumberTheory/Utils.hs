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

-- import Data.Array.Unboxed
-- import Data.Array.ST
-- import Data.Array.Base (unsafeAt, unsafeWrite)
--
--
--
-- trailZeros :: UArray Int Int
-- trailZeros = runST $ do
--   arr <- newArray (0,255) 0
--   unsafeWrite arr 0 8
--   let fill step val idx
--         | idx < 256     = unsafeWrite arr idx val >> fill step val (idx+step)
--         | step < 256    = fill (2*step) (val+1) step
--         | otherwise     = return arr
--   fill 4 1 2

{-# RULES
"shiftToOddCount/Int"       shiftToOddCount = shiftOCInt
"shiftToOddCount/Word"      shiftToOddCount = shiftOCWord
"shiftToOddCount/Integer"   shiftToOddCount = shiftOCInteger
  #-}
shiftToOddCount :: (Integral a, Bits a) => a -> (Int, a)
shiftToOddCount n = case shiftOCInteger (fromIntegral n) of
                      (z, o) -> (z, fromInteger o)

shiftOCWord :: Word -> (Int, Word)
shiftOCWord (W# w#) = case shiftToOddCount# 0# w# of
                        (# z# , u# #) -> (I# z#, W# u#)

shiftOCInt :: Int -> (Int, Int)
shiftOCInt (I# i#) = case shiftToOddCount# 0# (int2Word# i#) of
                        (# z#, u# #) -> (I# z#, I# (word2Int# u#))

-- Precondition: argument strictly positive
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


-- Shift argument right until the result is odd
-- Precondition: argument strictly positive
shiftToOdd# :: Word# -> Word#
shiftToOdd# w# =
    case zeroCountArr of
      BA arr# ->
        case indexInt8Array# arr# (word2Int# (w# `and#` 255##)) of
          8# -> shiftToOdd# (w# `uncheckedShiftRL#` 8#)
          k# -> w# `uncheckedShiftRL#` k#

-- like shiftToOdd#, but count the number of places to shift too
shiftToOddCount# :: Int# -> Word# -> (# Int#, Word# #)
shiftToOddCount# z# w# =
    case zeroCountArr of
      BA arr# ->
        case indexInt8Array# arr# (word2Int# (w# `and#` 255##)) of
          8# -> shiftToOddCount# (z# +# 8#) (w# `uncheckedShiftRL#` 8#)
          k# -> (# z# +# k#, w# `uncheckedShiftRL#` k# #)

bitCountWord# :: Word# -> Int#
bitCountWord# w# =
    case bitCountArr of
      BA arr# ->
        let go c# 0##   = c#
            go c# x#    = go (c# +# indexInt8Array# arr# (word2Int# (x# `and#` 255##)))
                                (x# `uncheckedShiftRL#` 8#)
        in go 0# w#

bitCountWord :: Word -> Int
bitCountWord (W# w#) = I# (bitCountWord# w#)

bitCountInt :: Int -> Int
bitCountInt (I# i#) = I# (bitCountWord# (int2Word# i#))

-- datatype for the lookup tables
data BA = BA ByteArray#

-- Number of trailing zero bits in a byte
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

-- Number of 1 bits in a byte
bitCountArr :: BA
bitCountArr =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 0# s1 of
                s2 ->
                  let loop idx st
                        | idx <# 256#   = case readInt8Array# mba (idx `andInt#` (idx -# 1#)) st of
                                            (# s', c #) -> case writeInt8Array# mba idx (c +# 1#) s' of
                                                            nx -> loop (idx +# 1#) nx
                        | otherwise     = st
                  in case loop 1# s2 of
                      s3 -> case unsafeFreezeByteArray# mba s3 of
                              (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b

-- Utility

andInt# :: Int# -> Int# -> Int#
andInt# i j = word2Int# (int2Word# i `and#` int2Word# j)
