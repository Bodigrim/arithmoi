{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
{- HLINT ignore "Use id" -}

-- Gourdon prime counting: page size, the 30-wheel (8 residues per 30 values, one byte per 30 values), integer roots
-- and two tiny helpers shared by all the modules.

{-# OPTIONS_GHC -O2 -fmax-simplifier-iterations=20 -fregs-graph #-}

module Math.NumberTheory.Primes.Sieve.WheelIndexing
  ( fi, unIO
  , pageBytes, blkShift, blkWords, tbl
  , whlNdxP1, whlRsd, whlGap, whlRndup, whlRndupNdx, div30, toWhlNdx, ndxToPrm, toOddNdx
  , icbrt#, isqrt#, isqrtW, icbrtW, isqrtI, icbrtI
  ) where

import Data.Bits
import Data.Word ( Word64 )
import GHC.Exts
import GHC.Word ( Word64(W64#) )
import GHC.IO ( IO(..) )

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO m) = m
{-# INLINE unIO #-}

pageBytes :: Int
pageBytes = 524288

-- block size for delta counts: 2^blkShift 64-bit words
blkShift :: Int
blkShift = 3

blkWords :: Int
blkWords = 1 `unsafeShiftL` blkShift

-- byte from a literal table
tbl :: Addr# -> Int -> Int
tbl a (I# i) = I# (word2Int# (word8ToWord# (indexWord8OffAddr# a i)))
{-# INLINE tbl #-}

------------------------------------------------------------------------
-- Wheel constants and conversions  (arithmoi: separate module)
------------------------------------------------------------------------

-- (wheel position index of v mod 30) + 1   (position -1 for residue 0 is stored as 0)
whlNdxP1 :: Int -> Int
whlNdxP1 = tbl "\0\1\1\1\1\1\1\2\2\2\2\3\3\4\4\4\4\5\5\6\6\6\6\7\7\7\7\7\7\8"#
{-# INLINE whlNdxP1 #-}

-- residues coprime to 30
whlRsd :: Int -> Int
whlRsd = tbl "\1\7\11\13\17\19\23\29"#
{-# INLINE whlRsd #-}

-- gaps between residues, two wheels long
whlGap :: Int -> Int
whlGap = tbl "\6\4\2\4\2\4\6\2\6\4\2\4\2\4\6\2"#
{-# INLINE whlGap #-}

-- next residue (round up) over two wheels, and its wheel position
whlRndup :: Int -> Int
whlRndup = tbl "\1\1\7\7\7\7\7\7\11\11\11\11\13\13\17\17\17\17\19\19\23\23\23\23\29\29\29\29\29\29\31\31\37\37\37\37\37\37\41\41\41\41\43\43\47\47\47\47\49\49\53\53\53\53\59\59\59\59\59\59\61\61"#
{-# INLINE whlRndup #-}

whlRndupNdx :: Int -> Int
whlRndupNdx = tbl "\0\0\1\1\1\1\1\1\2\2\2\2\3\3\4\4\4\4\5\5\6\6\6\6\7\7\7\7\7\7\0\0\1\1\1\1\1\1\2\2\2\2\3\3\4\4\4\4\5\5\6\6\6\6\7\7\7\7\7\7\0"#
{-# INLINE whlRndupNdx #-}

-- x div 30 for any non-negative Int, by multiplication (x/30 = ((x>>1) * 0x8888888888888889) >> 67)
div30 :: Int -> Int
div30 (I# x) =
  case timesWord2# (uncheckedShiftRL# (int2Word# x) 1#) 0x8888888888888889## of
    (# hi, _ #) -> I# (word2Int# (uncheckedShiftRL# hi 3#))
{-# INLINE div30 #-}

-- wheel (bit) index of the largest wheel position <= x; -1 for x = 0
toWhlNdx :: Int -> Int
toWhlNdx x = let !q = div30 x
                 !r = x - q * 30
             in (q `unsafeShiftL` 3) + whlNdxP1 r - 1
{-# INLINE toWhlNdx #-}

-- value of wheel index i
ndxToPrm :: Int -> Int
ndxToPrm i = (i `unsafeShiftR` 3) * 30 + whlRsd (i .&. 7)
{-# INLINE ndxToPrm #-}

-- index into the odd-only tables for odd value x (x = 0 gives -1)
toOddNdx :: Int -> Int
toOddNdx x = (x - 1) `unsafeShiftR` 1
{-# INLINE toOddNdx #-}

------------------------------------------------------------------------
-- Integer roots (Word64# digit-by-digit: exact for every Word64)
------------------------------------------------------------------------

icbrt# :: Word64# -> Word64#
icbrt# x0# = go 63# x0# (wordToWord64# 0##)
  where
    one#   = wordToWord64# 1##
    three# = wordToWord64# 3##
    go :: Int# -> Word64# -> Word64# -> Word64#
    go s# x# y#
      | isTrue# (s# <# 0#) = y#
      | otherwise =
          let !y2# = uncheckedShiftL64# y# 1#
              !c#  = plusWord64# (timesWord64# three# (timesWord64# y2# (plusWord64# y2# one#))) one#
          in if isTrue# (geWord64# (uncheckedShiftRL64# x# s#) c#)
               then go (s# -# 3#) (subWord64# x# (uncheckedShiftL64# c# s#)) (plusWord64# y2# one#)
               else go (s# -# 3#) x# y2#

isqrt# :: Word64# -> Word64#
isqrt# n# = go 32# n# (wordToWord64# 0##) (wordToWord64# 0x4000000000000000##)
  where
    go :: Int# -> Word64# -> Word64# -> Word64# -> Word64#
    go 0# _  r# _  = r#
    go k# x# r# b# =
      let !t#  = plusWord64# r# b#
          !r1# = uncheckedShiftRL64# r# 1#
          !b2# = uncheckedShiftRL64# b# 2#
      in if isTrue# (geWord64# x# t#)
           then go (k# -# 1#) (subWord64# x# t#) (plusWord64# r1# b#) b2#
           else go (k# -# 1#) x# r1# b2#

isqrtW, icbrtW :: Word64 -> Word64
isqrtW (W64# x) = W64# (isqrt# x)
icbrtW (W64# x) = W64# (icbrt# x)

isqrtI, icbrtI :: Int -> Int
isqrtI x = fi (isqrtW (fi x))
icbrtI x = fi (icbrtW (fi x))