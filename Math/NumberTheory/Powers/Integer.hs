-- |
-- Module:      Math.NumberTheory.Powers.Integer
-- Copyright:   (c) 2011-2014 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Potentially faster power function for 'Integer' base and 'Int'
-- or 'Word' exponent.
--
{-# LANGUAGE MagicHash, BangPatterns, CPP #-}
module Math.NumberTheory.Powers.Integer
    ( integerPower
    , integerWordPower
    ) where

import GHC.Base
import GHC.Integer.Logarithms (wordLog2#)

#if __GLASGOW_HASKELL__ < 707
import Math.NumberTheory.Utils (isTrue#)
#endif

-- | Power of an 'Integer' by the left-to-right repeated squaring algorithm.
--   This needs two multiplications in each step while the right-to-left
--   algorithm needs only one multiplication for 0-bits, but here the
--   two factors always have approximately the same size, which on average
--   gains a bit when the result is large.
--
--   For small results, it is unlikely to be any faster than '(^)', quite
--   possibly slower (though the difference shouldn't be large), and for
--   exponents with few bits set, the same holds. But for exponents with
--   many bits set, the speedup can be significant.
--
--   /Warning:/ No check for the negativity of the exponent is performed,
--   a negative exponent is interpreted as a large positive exponent.
integerPower :: Integer -> Int -> Integer
integerPower b (I# e#) = power b (int2Word# e#)

-- | Same as 'integerPower', but for exponents of type 'Word'.
integerWordPower :: Integer -> Word -> Integer
integerWordPower b (W# w#) = power b w#

power :: Integer -> Word# -> Integer
power b w#
  | isTrue# (w# `eqWord#` 0##) = 1
  | isTrue# (w# `eqWord#` 1##) = b
  | otherwise           = go (wordLog2# w# -# 1#) b (b*b)
    where
      go 0# l h = if isTrue# ((w# `and#` 1##) `eqWord#` 0##) then l*l else (l*h)
      go i# l h
        | w# `hasBit#` i#   = go (i# -# 1#) (l*h) (h*h)
        | otherwise         = go (i# -# 1#) (l*l) (l*h)

-- | A raw version of testBit for 'Word#'.
hasBit# :: Word# -> Int# -> Bool
hasBit# w# i# = isTrue# (((w# `uncheckedShiftRL#` i#) `and#` 1##) `neWord#` 0##)
