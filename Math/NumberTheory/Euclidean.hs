-- |
-- Module:      Math.NumberTheory.Euclidean
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module exports a class to represent Euclidean domains.
--

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Math.NumberTheory.Euclidean
  ( Euclidean(..)
  , WrappedIntegral(..)
  ) where

import Prelude hiding (divMod, div, gcd, lcm, mod, quotRem, quot, rem)
import qualified Prelude as P

import GHC.Exts
import GHC.Integer.GMP.Internals
import Numeric.Natural

-- | A class to represent a Euclidean domain,
-- which is basically an 'Integral' without 'toInteger'.
class (Eq a, Num a) => Euclidean a where
  -- | When restriced to a subring of the Euclidean domain @a@ isomorphic to
  -- @Integer@, this function should match @quotRem@ for Integers.
  quotRem :: a -> a -> (a, a)
  -- | When restriced to a subring of the Euclidean domain @a@ isomorphic to
  -- @Integer@, this function should match @divMod@ for Integers.
  divMod  :: a -> a -> (a, a)

  quot :: a -> a -> a
  quot x y = fst (quotRem x y)

  rem :: a -> a -> a
  rem x y = snd (quotRem x y)

  div :: a -> a -> a
  div x y = fst (divMod x y)

  mod :: a -> a -> a
  mod x y = snd (divMod x y)

  -- | @'gcd' x y@ is the greatest number that divides both @x@ and @y@.
  gcd :: a -> a -> a
  gcd x y =  gcd' (abs x) (abs y)
    where
      gcd' :: a -> a -> a
      gcd' a 0  =  a
      gcd' a b  =  gcd' b (abs (a `mod` b))

  -- | @'lcm' x y@ is the smallest number that both @x@ and @y@ divide.
  lcm :: a -> a -> a
  lcm _ 0 =  0
  lcm 0 _ =  0
  lcm x y =  abs ((x `quot` (gcd x y)) * y)

  -- | Test whether two numbers are coprime .
  coprime :: a -> a -> Bool
  coprime x y = gcd x y == 1

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
  extendedGCD :: a -> a -> (a, a, a)
  extendedGCD a b = (d, x * signum a, y * signum b)
    where
      (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
      eGCD !n1 o1 !n2 o2 r s
        | s == 0    = (r, o1, o2)
        | otherwise = case r `quotRem` s of
                        (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t

coprimeIntegral :: Integral a => a -> a -> Bool
coprimeIntegral x y = (odd x || odd y) && P.gcd x y == 1

-- | Wrapper around 'Integral', which has an 'Eucledian' instance.
newtype WrappedIntegral a = WrappedIntegral { unWrappedIntegral :: a }
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Integral a => Euclidean (WrappedIntegral a) where
  quotRem = P.quotRem
  divMod  = P.divMod
  quot    = P.quot
  rem     = P.rem
  div     = P.div
  mod     = P.mod
  gcd     = P.gcd
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Euclidean Int where
  quotRem = P.quotRem
  divMod  = P.divMod
  quot    = P.quot
  rem     = P.rem
  div     = P.div
  mod     = P.mod
  gcd (I# x) (I# y) = I# (gcdInt x y)
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Euclidean Word where
  quotRem = P.quotRem
  divMod  = P.divMod
  quot    = P.quot
  rem     = P.rem
  div     = P.div
  mod     = P.mod
  gcd (W# x) (W# y) = W# (gcdWord x y)
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Euclidean Integer where
  quotRem = P.quotRem
  divMod  = P.divMod
  quot    = P.quot
  rem     = P.rem
  div     = P.div
  mod     = P.mod
  gcd     = gcdInteger
  lcm     = lcmInteger
  coprime = coprimeIntegral
  -- Blocked by GHC bug
  -- https://ghc.haskell.org/trac/ghc/ticket/15350
  -- extendedGCD = gcdExtInteger

instance Euclidean Natural where
  quotRem = P.quotRem
  divMod  = P.divMod
  quot    = P.quot
  rem     = P.rem
  div     = P.div
  mod     = P.mod
  gcd     = P.gcd
  lcm     = P.lcm
  coprime = coprimeIntegral
