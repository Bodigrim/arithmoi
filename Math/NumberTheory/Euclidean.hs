-- |
-- Module:      Math.NumberTheory.Euclidean
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
-- Description: Deprecated
--
-- This module exports a class to represent Euclidean domains.
--

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Math.NumberTheory.Euclidean {-# DEPRECATED "Use Data.Euclidean instead" #-}
  ( GcdDomain(..)
  , Euclidean(..)
  , WrappedIntegral(..)
  , extendedGCD
  , isUnit
  ) where

import Prelude hiding (divMod, div, gcd, lcm, mod, quotRem, quot, rem)
import Data.Euclidean
import Data.Maybe
import Data.Semiring (Semiring(..), isZero)

-- | Check whether an element is a unit of the ring.
isUnit :: (Eq a, GcdDomain a) => a -> Bool
isUnit x = not (isZero x) && isJust (one `divide` x)

-- | Calculate the greatest common divisor of two numbers and coefficients
--   for the linear combination.
--
--   For signed types satisfies:
--
-- > case extendedGCD a b of
-- >   (d, u, v) -> u*a + v*b == d
-- >                && d == gcd a b
--
--   For unsigned and bounded types the property above holds, but since @u@ and @v@ must also be unsigned,
--   the result may look weird. E. g., on 64-bit architecture
--
-- > extendedGCD (2 :: Word) (3 :: Word) == (1, 2^64-1, 1)
--
--   For unsigned and unbounded types (like 'Numeric.Natural.Natural') the result is undefined.
--
--   For signed types we also have
--
-- > abs u < abs b || abs b <= 1
-- >
-- > abs v < abs a || abs a <= 1
--
--   (except if one of @a@ and @b@ is 'minBound' of a signed type).
extendedGCD :: (Eq a, Num a, Euclidean a) => a -> a -> (a, a, a)
extendedGCD a b = (d, x * signum a, y * signum b)
  where
    (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
    eGCD !n1 o1 !n2 o2 r s
      | s == 0    = (r, o1, o2)
      | otherwise = case r `quotRem` s of
                      (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t
