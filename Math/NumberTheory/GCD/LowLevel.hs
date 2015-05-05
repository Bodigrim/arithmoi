-- |
-- Module:      Math.NumberTheory.GCD.LowLevel
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Low level gcd and coprimality functions using the binary gcd algorithm.
-- Normally, accessing these via the higher level interface of "Math.NumberTheory.GCD"
-- should be sufficient.
--
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
module Math.NumberTheory.GCD.LowLevel
  ( -- * Specialised GCDs
    gcdInt
  , gcdWord
    -- ** GCDs for unboxed types
  , gcdInt#
  , gcdWord#
    -- * Specialised tests for coprimality
  , coprimeInt
  , coprimeWord
    -- ** Coprimality tests for unboxed types
  , coprimeInt#
  , coprimeWord#
    -- * Specialised extended GCDs
  , egcdInt
  , egcdWord
    -- ** Extended GCDs for unboxed types
  , egcdInt#
  , egcdWord#
  ) where

import GHC.Base

#if __GLASGOW_HASKELL__ < 705
import GHC.Word (Word(..))      -- Moved to GHC.Types
#endif

import Math.NumberTheory.Utils

-- | Greatest common divisor of two 'Int's, calculated with the binary gcd algorithm.
gcdInt :: Int -> Int -> Int
gcdInt (I# a#) (I# b#) = I# (gcdInt# a# b#)

-- | Test whether two 'Int's are coprime, using an abbreviated binary gcd algorithm.
coprimeInt :: Int -> Int -> Bool
coprimeInt (I# a#) (I# b#) = coprimeInt# a# b#

-- | Greatest common divisor of two 'Word's, calculated with the binary gcd algorithm.
gcdWord :: Word -> Word -> Word
gcdWord (W# a#) (W# b#) = W# (gcdWord# a# b#)

-- | Test whether two 'Word's are coprime, using an abbreviated binary gcd algorithm.
coprimeWord :: Word -> Word -> Bool
coprimeWord (W# a#) (W# b#) = coprimeWord# a# b#

-- | Greatest common divisor of two 'Int#'s, calculated with the binary gcd algorithm.
gcdInt# :: Int# -> Int# -> Int#
gcdInt# a# b# = word2Int# (gcdWord# (int2Word# (absInt# a#)) (int2Word# (absInt# b#)))


-- | Test whether two 'Int#'s are coprime.
coprimeInt# :: Int# -> Int# -> Bool
coprimeInt# a# b# = coprimeWord# (int2Word# (absInt# a#)) (int2Word# (absInt# b#))

-- | Greatest common divisor of two 'Word#'s, calculated with the binary gcd algorithm.
gcdWord# :: Word# -> Word# -> Word#
gcdWord# a# 0## = a#
gcdWord# 0## b# = b#
gcdWord# a# b#  =
    case shiftToOddCount# a# of
      (# za#, oa# #) ->
        case shiftToOddCount# b# of
          (# zb#, ob# #) -> gcdWordOdd# oa# ob# `uncheckedShiftL#` (if isTrue# (za# <# zb#) then za# else zb#)

-- | Test whether two 'Word#'s are coprime.
coprimeWord# :: Word# -> Word# -> Bool
coprimeWord# a# b# =
  (isTrue# (a# `eqWord#` 1##) || isTrue# (b# `eqWord#` 1##))
  || (isTrue# (((a# `or#` b#) `and#` 1##) `eqWord#` 1##) -- not both even
      && ((isTrue# (a# `neWord#` 0##) && isTrue# (b# `neWord#` 0##)) -- neither is zero
      && isTrue# (gcdWordOdd# (shiftToOdd# a#) (shiftToOdd# b#) `eqWord#` 1##)))

-- Various auxiliary functions

-- calculate the gcd of two odd numbers
{-# INLINE gcdWordOdd# #-}
gcdWordOdd# :: Word# -> Word# -> Word#
gcdWordOdd# a# b#
  | isTrue# (a# `eqWord#` 1##) || isTrue# (b# `eqWord#` 1##)    = 1##
  | isTrue# (a# `eqWord#` b#)                                   = a#
  | isTrue# (a# `ltWord#` b#)                                   = oddGCD# b# a#
  | otherwise                                                   = oddGCD# a# b#

-- calculate the gcd of two odd numbers using the binary gcd algorithm
-- Precondition: first argument strictly larger than second (which should be greater than 1)
oddGCD# :: Word# -> Word# -> Word#
oddGCD# a# b# =
    case shiftToOdd# (a# `minusWord#` b#) of
      1## -> 1##
      c#  | isTrue# (c# `ltWord#` b#)   -> oddGCD# b# c#
          | isTrue# (c# `gtWord#` b#)   -> oddGCD# c# b#
          | otherwise                   -> c#

absInt# :: Int# -> Int#
absInt# i#
  | isTrue# (i# <# 0#)  = negateInt# i#
  | otherwise           = i#

-- | Binary extended GCD for @'Int'@ values.
egcdInt :: Int -> Int -> (Int, Int, Int)
egcdInt (I# x#) (I# y#) = (I# a#, I# b#, I# v#)
  where
    (# a#, b#, v# #) = egcdInt# x# y#

egcdInt# :: Int# -> Int# -> (# Int#, Int#, Int# #)
egcdInt# x# y# = (# mulSign# a# x#, mulSign# b# y#, word2Int# v# #)
  where
    (# a#, b#, v# #) = egcdWord# (int2Word# (absInt# x#)) (int2Word# (absInt# y#))
    mulSign# w# i#
      | isTrue# (i# <# 0#) = negateInt# w#
      | otherwise          = w#

-- | Binary extended GCD for @'Word'@.
-- Note that linear coefficients returned by @'egcdWord'@ are signed
-- @'Int'@ values.
egcdWord :: Word -> Word -> (Int, Int, Word)
egcdWord (W# x#) (W# y#) = (I# a#, I# b#, W# v#)
  where
    (# a#, b#, v# #) = egcdWord# x# y#

egcdWord# :: Word# -> Word# -> (# Int#, Int#, Word# #)
egcdWord# 0## y#  = (# 0#, 1#, y# #)
egcdWord# x#  0## = (# 1#, 0#, x# #)
egcdWord# x#  y#  = (# ra#, rb#, rv# #)
  where
    (# ra#, rb#, rv# #) = loop# x'# y'# 1# 0# 0# 1#

    lx# = x# `and#` (not# x# `plusWord#` 1##)
    ly# = y# `and#` (not# y# `plusWord#` 1##)
    g# | isTrue# (lx# `ltWord#` ly#) = lx#
       | otherwise                   = ly#
    x'# = x# `quotWord#` g#
    y'# = y# `quotWord#` g#
    ix# = word2Int# x'#
    iy# = word2Int# y'#

    loop# u# v# a# b# c# d#
      | isTrue# (u# `eqWord#` 0##)  = (# c#, d#, g# `timesWord#` v# #)
      | otherwise =
          let (# u'#, a'#, b'# #) = step# u# a# b#
              (# v'#, c'#, d'# #) = step# v# c# d#
          in if isTrue# (u'# `geWord#` v'#)
               then loop# (u'# `minusWord#` v'#) v'# (a'# -# c'#) (b'# -# d'#) c'# d'#
               else loop# u'# (v'# `minusWord#` u'#) a'# b'# (c'# -# a'#) (d'# -# b'#)

    step# w# p# q#
      | isTrue# ((w# `and#` 1##) `eqWord#` 1##)
        = (# w#, p#, q# #)
      | isTrue# (((p# `orI#` q#) `andI#` 1#) ==# 1#)
        = step# (w# `uncheckedShiftRL#` 1#) ((p# +# iy#) `uncheckedIShiftRA#` 1#) ((q# -# ix#) `uncheckedIShiftRA#` 1#)
      | otherwise
        = step# (w# `uncheckedShiftRL#` 1#) (p# `uncheckedIShiftRA#` 1#) (q# `uncheckedIShiftRA#` 1#)
