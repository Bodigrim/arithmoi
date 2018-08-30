-- |
-- Module:      Math.NumberTheory.EuclideanDomain
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module exports a class to represent Euclidean domains.
--

module Math.NumberTheory.EuclideanDomain
  ( EuclideanDomain (..)
  , div
  , mod
  , quot
  , rem
  ) where

import Prelude hiding (divMod, div, mod, quotRem, quot, rem)

-- | A class to represent a Euclidean domain.
class EuclideanDomain a where
  -- | When restriced to a subring of the Euclidean domain @a@ isomorphic to
  -- @Integer@, this function should match @quotRem@ for Integers.
  quotRem :: a -> a -> (a, a)
  -- | When restriced to a subring of the Euclidean domain @a@ isomorphic to
  -- @Integer@, this function should match @divMod@ for Integers.
  divMod  :: a -> a -> (a, a)

quot :: EuclideanDomain a => a -> a -> a
quot x y = fst (quotRem x y)

-- | Remainder of Euclidean division, satisfying
--
-- > (x `quot` y)*y + (x `rem` y) == x
-- for @x, y@ in a Euclidean domain @a@.
rem :: EuclideanDomain a => a -> a -> a
rem x y = snd (quotRem x y)

div :: EuclideanDomain a => a -> a -> a
div x y = fst (divMod x y)

-- | Remainder of Euclidean division, satisfying
--
-- > (x `div` y) * y + (x `mod` y) == x
mod :: EuclideanDomain a => a -> a -> a
mod x y = snd (divMod x y)