-- |
-- Module:      Math.NumberTheory.Recurrencies
-- Description: Deprecated
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--

module Math.NumberTheory.Recurrencies {-# DEPRECATED "Use `Math.NumberTheory.Recurrences` instead." #-}
    ( module Math.NumberTheory.Recurrences.Linear
    , module Math.NumberTheory.Recurrences.Bilinear
    , module Math.NumberTheory.Recurrences.Pentagonal
    ) where

import Math.NumberTheory.Recurrences.Bilinear
import Math.NumberTheory.Recurrences.Linear
import Math.NumberTheory.Recurrences.Pentagonal (partition)
