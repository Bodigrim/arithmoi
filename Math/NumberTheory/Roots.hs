-- |
-- Module:      Math.NumberTheory.Roots
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Calculating integer roots and testing perfect powers.
--
module Math.NumberTheory.Roots
  ( -- * Square roots
    integerSquareRoot
  , integerSquareRootRem
  , isSquare
  , exactSquareRoot
  -- * Cube roots
  , integerCubeRoot
  , isCube
  , exactCubeRoot
  -- * Fourth power roots
  , integerFourthRoot
  , isFourthPower
  , exactFourthRoot
  -- * General
  , integerRoot
  , isKthPower
  , exactRoot
  , isPerfectPower
  , highestPower
  ) where

import Math.NumberTheory.Roots.Squares
import Math.NumberTheory.Roots.Cubes
import Math.NumberTheory.Roots.Fourth
import Math.NumberTheory.Roots.General
