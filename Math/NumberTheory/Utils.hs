-- |
-- Module:      Math.NumberTheory.Utils
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Some utilities, mostly for bit twiddling.
--

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}

module Math.NumberTheory.Utils
    ( SomeKnown(..)
    , shiftToOddCount
    , shiftToOdd
    , shiftToOdd#
    , shiftToOddCount#
    , shiftToOddCountBigNat
    , splitOff
    , splitOff#

    , mergeBy

    , recipMod

    , toWheel30
    , fromWheel30
    , withSomeKnown
    , intVal
    ) where

import Prelude hiding (mod, quotRem)
import qualified Prelude as P

import Data.Bits
import Data.Euclidean
import Data.List.Infinite (Infinite(..))
import Data.Semiring (Semiring(..), isZero)
import GHC.Base
import GHC.Num.BigNat
import GHC.Num.Integer
import GHC.Num.Natural
import qualified Math.NumberTheory.Utils.FromIntegral as UT
import GHC.Natural
import GHC.TypeNats
import Math.NumberTheory.Utils.FromIntegral (intToWord)

-- | Remove factors of @2@ and count them. If
--   @n = 2^k*m@ with @m@ odd, the result is @(k, m)@.
--   Precondition: argument not @0@ (not checked).
{-# RULES
"shiftToOddCount/Int"       shiftToOddCount = shiftOCInt
"shiftToOddCount/Word"      shiftToOddCount = shiftOCWord
"shiftToOddCount/Integer"   shiftToOddCount = shiftOCInteger
"shiftToOddCount/Natural"   shiftToOddCount = shiftOCNatural
  #-}
{-# INLINE [1] shiftToOddCount #-}
shiftToOddCount :: Integral a => a -> (Word, a)
shiftToOddCount n = case shiftOCInteger (toInteger n) of
                      (z, o) -> (z, fromInteger o)

-- | Specialised version for @'Word'@.
--   Precondition: argument strictly positive (not checked).
shiftOCWord :: Word -> (Word, Word)
shiftOCWord (W# w#) = case shiftToOddCount# w# of
                        (# z# , u# #) -> (W# z#, W# u#)

-- | Specialised version for @'Int'@.
--   Precondition: argument nonzero (not checked).
shiftOCInt :: Int -> (Word, Int)
shiftOCInt (I# i#) = case shiftToOddCount# (int2Word# i#) of
                        (# z#, u# #) -> (W# z#, I# (word2Int# u#))

-- | Specialised version for @'Integer'@.
--   Precondition: argument nonzero (not checked).
shiftOCInteger :: Integer -> (Word, Integer)
shiftOCInteger n@(IS i#) =
    case shiftToOddCount# (int2Word# i#) of
      (# 0##, _ #) -> (0, n)
      (# z#, w# #) -> (W# z#, integerFromWord# w#)
shiftOCInteger n@(IP bn#) = case bigNatZeroCount bn# of
                                 0## -> (0, n)
                                 z#  -> (W# z#, integerFromBigNat# (bn# `bigNatShiftR#` z#))
shiftOCInteger n@(IN bn#) = case bigNatZeroCount bn# of
                                 0## -> (0, n)
                                 z#  -> (W# z#, integerFromBigNatNeg# (bn# `bigNatShiftR#` z#))

-- | Specialised version for @'Natural'@.
--   Precondition: argument nonzero (not checked).
shiftOCNatural :: Natural -> (Word, Natural)
shiftOCNatural n@(NatS# i#) =
    case shiftToOddCount# i# of
      (# 0##, _ #) -> (0, n)
      (# z#, w# #) -> (W# z#, NatS# w#)
shiftOCNatural n@(NatJ# (BN# bn#)) = case bigNatZeroCount bn# of
                                 0## -> (0, n)
                                 z#  -> (W# z#, naturalFromBigNat# (bn# `bigNatShiftR#` z#))

shiftToOddCountBigNat :: BigNat# -> (# Word, BigNat# #)
shiftToOddCountBigNat bn# = case bigNatZeroCount bn# of
  0## -> (# 0, bn# #)
  z#  -> (# W# z#, bn# `bigNatShiftR#` z# #)

-- | Count trailing zeros in a @'BigNat'@.
--   Precondition: argument nonzero (not checked, Integer invariant).
bigNatZeroCount :: BigNat# -> Word#
bigNatZeroCount bn# = count 0## 0#
  where
    !(W# bitSize#) = intToWord (finiteBitSize (0 :: Word))
    count a# i# =
          case bigNatIndex# bn# i# of
            0## -> count (a# `plusWord#` bitSize#) (i# +# 1#)
            w#  -> a# `plusWord#` ctz# w#

-- | Remove factors of @2@. If @n = 2^k*m@ with @m@ odd, the result is @m@.
--   Precondition: argument not @0@ (not checked).
{-# RULES
"shiftToOdd/Int"       shiftToOdd = shiftOInt
"shiftToOdd/Word"      shiftToOdd = shiftOWord
"shiftToOdd/Integer"   shiftToOdd = shiftOInteger
  #-}
{-# INLINE [1] shiftToOdd #-}
shiftToOdd :: Integral a => a -> a
shiftToOdd n = fromInteger (shiftOInteger (toInteger n))

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
shiftOInteger (IS i#) = integerFromWord# (shiftToOdd# (int2Word# i#))
shiftOInteger n@(IP bn#) = case bigNatZeroCount bn# of
                                 0## -> n
                                 z#  -> integerFromBigNat# (bn# `bigNatShiftR#` z#)
shiftOInteger n@(IN bn#) = case bigNatZeroCount bn# of
                                 0## -> n
                                 z#  -> integerFromBigNatNeg# (bn# `bigNatShiftR#` z#)

-- | Shift argument right until the result is odd.
--   Precondition: argument not @0@, not checked.
shiftToOdd# :: Word# -> Word#
shiftToOdd# w# = uncheckedShiftRL# w# (word2Int# (ctz# w#))

-- | Like @'shiftToOdd#'@, but count the number of places to shift too.
shiftToOddCount# :: Word# -> (# Word#, Word# #)
shiftToOddCount# w# = case ctz# w# of
                        k# -> (# k#, uncheckedShiftRL# w# (word2Int# k#) #)

splitOff :: (Eq a, GcdDomain a) => a -> a -> (Word, a)
splitOff p n
  | isZero n  = (0, zero) -- prevent infinite loop
  | otherwise = go 0 n
  where
    go !k m = case m `divide` p of
      Just q -> go (k + 1) q
      _      -> (k, m)
{-# INLINABLE splitOff #-}

-- | It is difficult to convince GHC to unbox output of 'splitOff' and 'splitOff.go',
-- so we fallback to a specialized unboxed version to minimize allocations.
splitOff# :: Word# -> Word# -> (# Word#, Word# #)
splitOff# _ 0## = (# 0##, 0## #)
splitOff# p n = go 0## n
  where
    go k m = case m `quotRemWord#` p of
      (# q, 0## #) -> go (k `plusWord#` 1##) q
      _            -> (# k, m #)
{-# INLINABLE splitOff# #-}

-- | Merges two ordered lists into an ordered list. Checks for neither its
-- precondition or postcondition.
mergeBy :: (a -> a -> Ordering) -> Infinite a -> Infinite a -> Infinite a
mergeBy cmp = loop
  where
    loop ( x:< xs) (y :< ys)
      = case cmp x y of
         GT -> y :< loop (x :< xs) ys
         _  -> x :< loop xs (y :< ys)

-- | Work around https://ghc.haskell.org/trac/ghc/ticket/14085
recipMod :: Integer -> Integer -> Maybe Integer
recipMod x m = case integerRecipMod# (x `P.mod` m) (fromInteger m) of
  (# | _ #) -> Nothing
  (# y | #) -> Just (toInteger y)

-------------------------------------------------------------------------------
-- Helpers for mapping to rough numbers and back.
-- Copypasted from Data.BitStream.WheelMapping

toWheel30 :: (Integral a, Bits a) => a -> a
toWheel30 i = q `shiftL` 3 + (r + r `shiftR` 4) `shiftR` 2
  where
    (q, r) = i `P.quotRem` 30

fromWheel30 :: (Num a, Bits a) => a -> a
fromWheel30 i = ((i `shiftL` 2 - i `shiftR` 2) .|. 1)
              + ((i `shiftL` 1 - i `shiftR` 1) .&. 2)

-------------------------------------------------------------------------------
-- Helpers for dealing with data types parametrised by natural numbers.

data SomeKnown (f :: Nat -> Type) where
  SomeKnown :: KnownNat k => f k -> SomeKnown f

withSomeKnown :: (forall k. KnownNat k => f k -> a) -> SomeKnown f -> a
withSomeKnown f (SomeKnown x) = f x

intVal :: KnownNat k => a k -> Int
intVal = UT.naturalToInt . natVal
