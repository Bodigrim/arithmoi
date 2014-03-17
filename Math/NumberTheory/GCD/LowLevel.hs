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
  ) where

#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts.Compat
#else
import GHC.Base
#endif
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
          (# zb#, ob# #) -> gcdWordOdd# oa# ob# `uncheckedShiftL#` (if za# <# zb# then za# else zb#)

-- | Test whether two 'Word#'s are coprime.
coprimeWord# :: Word# -> Word# -> Bool
coprimeWord# a# b# =
  (a# `eqWord#` 1## || b# `eqWord#` 1##)
  || ((((a# `or#` b#) `and#` 1##) `eqWord#` 1##) -- not both even
      && ((a# `neWord#` 0## && b# `neWord#` 0##) -- neither is zero
      && gcdWordOdd# (shiftToOdd# a#) (shiftToOdd# b#) `eqWord#` 1##))

-- Various auxiliary functions

-- calculate the gcd of two odd numbers
{-# INLINE gcdWordOdd# #-}
gcdWordOdd# :: Word# -> Word# -> Word#
gcdWordOdd# a# b#
  | a# `eqWord#` 1## || b# `eqWord#` 1##  = 1##
  | a# `eqWord#` b#                       = a#
  | a# `ltWord#` b#                       = oddGCD# b# a#
  | otherwise                             = oddGCD# a# b#

-- calculate the gcd of two odd numbers using the binary gcd algorithm
-- Precondition: first argument strictly larger than second (which should be greater than 1)
oddGCD# :: Word# -> Word# -> Word#
oddGCD# a# b# =
    case shiftToOdd# (a# `minusWord#` b#) of
      1## -> 1##
      c#  | c# `ltWord#` b# -> oddGCD# b# c#
          | c# `gtWord#` b# -> oddGCD# c# b#
          | otherwise       -> c#

absInt# :: Int# -> Int#
absInt# i#
  | i# <# 0#    = negateInt# i#
  | otherwise   = i#
