-- |
-- Module:      Math.NumberTheory.Powers.Integer
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Slightly faster power function for Integer base and Int exponent.
--
{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Powers.Integer
    ( integerPower
    ) where

import GHC.Base

import Math.NumberTheory.Logarithms.Internal ( wordLog2# )

-- | Power of an 'Integer' by the left-to-right repeated squaring algorithm.
--   This needs two multiplications in each step while the right-to-left
--   algorithm needs only one multiplication for 0-bits, but here the
--   two factors always have approximately the same size, which on average
--   gains a bit.
integerPower :: Integer -> Int -> Integer
integerPower b (I# e#)
  | e# ==# 0#   = 1
  | e# ==# 1#   = b
  | otherwise   = go (wordLog2# w# -# 1#) b (b*b)
    where
      !w# = int2Word# e#
      go 0# l h = if (w# `and#` 1##) `eqWord#` 0## then l*l else (l*h)
      go i# l h
        | w# `hasBit#` i#   = go (i# -# 1#) (l*h) (h*h)
        | otherwise         = go (i# -# 1#) (l*l) (l*h)

-- | A raw version of testBit for 'Word#'.
hasBit# :: Word# -> Int# -> Bool
hasBit# w# i# = ((w# `uncheckedShiftRL#` i#) `and#` 1##) `neWord#` 0##
