-- |
-- Module:      Math.NumberTheory.Zeta
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé, Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Numeric evaluation of various zeta-functions.

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta
  ( -- * Riemann zeta-function
    zetas
  , zetasEven
    -- * Dirichlet beta-function
  , betas
  , betasOdd
    -- * Hurwitz zeta-functions
  , zetaHurwitz
  ) where

import Math.NumberTheory.Zeta.Dirichlet
import Math.NumberTheory.Zeta.Hurwitz
import Math.NumberTheory.Zeta.Riemann
