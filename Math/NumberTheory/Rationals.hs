-- |
-- Module:      Math.NumberTheory.Rationals
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module is based on the functional pearl
-- <http://www.cs.ox.ac.uk/jeremy.gibbons/publications/rationals.pdf Enumerating the Rationals>.
module Math.NumberTheory.Rationals (
  -- * Rationals
  rationals,
  positiveRationals,
  nextPositiveRational,

  -- * Coprime pairs
  coprimes,
  positiveCoprimes
) where

import Control.Arrow ((&&&))
import Data.Ratio

-- | All rational numbers (including @0@ and negative numbers).
-- The numbers appear as in breadth-first traversal
-- of Calkin-Wilf tree.
rationals :: Integral a => [Ratio a]
rationals = 0 : (positiveRationals >>= \x -> [x, -x])

-- | All positive rational numbers.
-- The numbers appear as in breadth-first traversal
-- of Calkin-Wilf tree.
positiveRationals :: Integral a => [Ratio a]
positiveRationals = iterate nextPositiveRational 1

-- | Get the next rational number in breadth-first traversal
-- of Calkin-Wilf tree.
nextPositiveRational :: Integral a => Ratio a -> Ratio a
nextPositiveRational x = recip (fromInteger n + 1 - d)
  where
    (n, d) = properFraction x

-- | All coprime pairs (including negative numbers).
coprimes :: Integral a => [(a, a)]
coprimes =
  [ (x, y)
  | (n, m) <- positiveCoprimes
  , x <- [n, -n]
  , y <- [m, -m] ]

-- | All positive coprime pairs.
positiveCoprimes :: Integral a => [(a, a)]
positiveCoprimes = map (numerator &&& denominator) positiveRationals

