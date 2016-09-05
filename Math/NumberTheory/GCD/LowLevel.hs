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
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
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

import GHC.Base

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
