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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.NumberTheory.Euclidean
  ( Euclidean (..)
  , div
  , mod
  , quot
  , rem
  ) where

import Prelude hiding (divMod, div, gcd, mod, quotRem, quot, rem)
import qualified Prelude as P

-- | A class to represent a Euclidean domain.
class (Eq a, Num a) => Euclidean a where
  -- | When restriced to a subring of the Euclidean domain @a@ isomorphic to
  -- @Integer@, this function should match @quotRem@ for Integers.
  quotRem :: a -> a -> (a, a)
  -- | When restriced to a subring of the Euclidean domain @a@ isomorphic to
  -- @Integer@, this function should match @divMod@ for Integers.
  divMod  :: a -> a -> (a, a)
  -- | Taken from @prelude.gcd@, but the helper function uses `mod` instead of
  -- `rem`. This is because the property @(x `quot` y)*y + (x `rem` y) == x@
  -- does not hold for any two @x, y@ @EisenteinInteger@s/@GaussianInteger@s.
  gcd :: a -> a -> a
  gcd x y =  gcd' (abs x) (abs y)
    where
      gcd' :: a -> a -> a
      gcd' a 0  =  a
      gcd' a b  =  gcd' b (abs (a `mod` b))

quot :: Euclidean a => a -> a -> a
quot x y = fst (quotRem x y)

-- | Remainder of Euclidean division, satisfying
--
-- > (x `quot` y)*y + (x `rem` y) == x
-- for @x, y@ in a Euclidean domain @a@.
rem :: Euclidean a => a -> a -> a
rem x y = snd (quotRem x y)

div :: Euclidean a => a -> a -> a
div x y = fst (divMod x y)

-- | Remainder of Euclidean division, satisfying
--
-- > (x `div` y) * y + (x `mod` y) == x
mod :: Euclidean a => a -> a -> a
mod x y = snd (divMod x y)

instance {-# OVERLAPPABLE #-} (Eq a, Integral a, Num a) => Euclidean a where
  quotRem = P.quotRem
  divMod  = P.divMod
  gcd     = P.gcd