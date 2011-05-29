-- |
-- Module:      Math.NumberTheory.Powers
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Calculating integer roots, modular powers and related things.
-- This module reexports the most needed functions from the implementation
-- modules. The implementation modules provide some additional functions,
-- in particular some unsafe functions which omit some tests for performance
-- reasons.
--
module Math.NumberTheory.Powers
  ( -- *  Integer Roots
    -- ** Square roots
    integerSquareRoot
  , isSquare
  , exactSquareRoot
    -- ** Cube roots
  , integerCubeRoot
  , isCube
  , exactCubeRoot
    -- ** Fourth roots
  , integerFourthRoot
  , isFourthPower
  , exactFourthRoot
    -- ** General roots
  , integerRoot
  , isKthPower
  , exactRoot
  , isPerfectPower
  , highestPower
    -- Modular powers
  , powerMod
  ) where

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Powers.Cubes
import Math.NumberTheory.Powers.Fourth
import Math.NumberTheory.Powers.General
import Math.NumberTheory.Moduli
