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

class EuclideanDomain a where
  quotRem :: a -> a -> (a, a)
  divMod  :: a -> a -> (a, a)

quot :: EuclideanDomain a => a -> a -> a
quot x y = fst (quotRem x y)

rem :: EuclideanDomain a => a -> a -> a
rem x y = snd (quotRem x y)

div :: EuclideanDomain a => a -> a -> a
div x y = fst (divMod x y)

mod :: EuclideanDomain a => a -> a -> a
mod x y = snd (divMod x y)