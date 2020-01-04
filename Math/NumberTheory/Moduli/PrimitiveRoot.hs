-- |
-- Module:      Math.NumberTheory.Moduli.PrimitiveRoot
-- Copyright:   (c) 2017 Andrew Lelechenko, 2018 Bhavik Mehta
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Description: Deprecated
--
-- Primitive roots and cyclic groups.
--

module Math.NumberTheory.Moduli.PrimitiveRoot
  {-# DEPRECATED "Use Math.NumberTheory.Moduli.Multiplicative instead" #-}
  ( -- * Primitive roots
    PrimitiveRoot
  , unPrimitiveRoot
  , isPrimitiveRoot
  ) where

import Math.NumberTheory.Moduli.Multiplicative
