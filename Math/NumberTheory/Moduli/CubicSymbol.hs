-- |
-- Module:      Math.NumberTheory.Moduli.CubicSymbol
-- Copyright:   (c) 2020 Federico Bongiorno
-- Licence:     MIT
-- Maintainer:  Federico Bongiorno <federicobongiorno97@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Cubic_reciprocity#Cubic_residue_character Cubic symbol>
-- of two Eisenstein Integers.

{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.Moduli.CubicSymbol
  ( CubicSymbol(..)
  , stimes
  , cubicSymbol
  , symbolToNum
  ) where

import Math.NumberTheory.Quadratic.EisensteinIntegers
import Math.NumberTheory.Utils.FromIntegral
import qualified Data.Euclidean as A
import Math.NumberTheory.Utils
import Data.Semigroup

-- | Represents the
-- <https://en.wikipedia.org/wiki/Cubic_reciprocity#Cubic_residue_character cubic residue character>
-- It is either @0@, @ω@, @ω²@ or @1@.
data CubicSymbol = Zero | Omega | OmegaSquare | One deriving (Eq)

-- | The set of cubic symbols form a semigroup. Note `stimes`
-- is allowed to take non-positive values. In other words, the set
-- of non-zero cubic symbols is regarded as a group.
--
-- >>> stimes (-1) Omega
-- ω²
-- >>> stimes 0 Zero
-- 1
instance Semigroup CubicSymbol where
  Zero <> _                    = Zero
  _ <> Zero                    = Zero
  One <> y                     = y
  x <> One                     = x
  Omega <> Omega               = OmegaSquare
  Omega <> OmegaSquare         = One
  OmegaSquare <> Omega         = One
  OmegaSquare <> OmegaSquare   = Omega
  stimes k n = case (k `mod` 3, n) of
    (0, _)           -> One
    (1, symbol)       -> symbol
    (2, Omega)       -> OmegaSquare
    (2, OmegaSquare) -> Omega
    (2, symbol)      -> symbol
    _                -> error "Math.NumberTheory.Moduli.CubicSymbol: exponentiation undefined."

instance Show CubicSymbol where
  show = \case
    Zero         -> "0"
    Omega        -> "ω"
    OmegaSquare  -> "ω²"
    One          -> "1"

-- | Converts a
-- <https://en.wikipedia.org/wiki/Cubic_reciprocity#Cubic_residue_character cubic symbol>
-- to an Eisenstein Integer.
symbolToNum :: CubicSymbol -> EisensteinInteger
symbolToNum = \case
  Zero        -> 0
  Omega       -> ω
  OmegaSquare -> -1 - ω
  One         -> 1

-- The algorithm `cubicSymbol` is adapted from
-- <https://cs.au.dk/~gudmund/Documents/cubicres.pdf here>.
-- It is divided in the following steps.
--
-- (1) Check whether @beta@ is coprime to 3.
-- (2) Replace @alpha@ by the remainder of @alpha@ mod @beta@
--     This does not affect the cubic symbol.
-- (3) Replace @alpha@ and @beta@ by their associated primary
--     divisors and keep track of how their cubic residue changes.
-- (4) Check if any of the two numbers is a zero or a unit. In this
--     case, return their cubic residue.
-- (5) Otherwise, invoke cubic reciprocity by swapping @alpha@ and
--     @beta@. Note both numbers have to be primary.
--     Return to Step 2.

-- | <https://en.wikipedia.org/wiki/Cubic_reciprocity#Cubic_residue_character Cubic symbol>
-- of two Eisenstein Integers.
-- The first argument is the numerator and the second argument
-- is the denominator. The latter must be coprime to @3@.
-- This condition is checked.
--
-- If the arguments have a common factor, the result
-- is 'Zero', otherwise it is either 'Omega', 'OmegaSquare' or 'One'.
--
-- >>> cubicSymbol (45 + 23*ω) (11 - 30*ω)
-- 0
-- >>> cubicSymbol (31 - ω) (1 +10*ω)
-- ω
cubicSymbol :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
cubicSymbol alpha beta = case beta `A.rem` (1 - ω) of
  -- This checks whether beta is coprime to 3, i.e. divisible by @1 - ω@
  -- In particular, it returns an error if @beta == 0@
  0 -> error "Math.NumberTheory.Moduli.CubicSymbol: denominator is not coprime to 3."
  _ -> cubicSymbolHelper alpha beta

cubicSymbolHelper :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
cubicSymbolHelper alpha beta = cubicReciprocity primaryRemainder primaryBeta <> newSymbol
  where
    (primaryRemainder, primaryBeta, newSymbol) = extractPrimaryContributions remainder beta
    remainder = A.rem alpha beta

cubicReciprocity :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
-- Note @cubicReciprocity 0 1 = One@. It is better to adopt this convention.
cubicReciprocity _ 1 = One
-- Checks if first argument is zero. Note the second argument is never zero.
cubicReciprocity 0 _ = Zero
-- This checks if the first argument is a unit. Because it's primary,
-- it is enough to pattern match with 1.
cubicReciprocity 1 _ = One
-- Otherwise, cubic reciprocity is called.
cubicReciprocity alpha beta = cubicSymbolHelper beta alpha

-- | This function takes two Eisenstein intgers @alpha@ and @beta@ and returns
-- three arguments @(gamma, delta, newSymbol)@. @gamma@ and @delta@ are the
-- associated primary numbers of alpha and beta respectively. @newSymbol@
-- is the cubic symbol measuring the discrepancy between the cubic residue
-- of @alpha@ and @beta@, and the cubic residue of @gamma@ and @delta@.
extractPrimaryContributions :: EisensteinInteger -> EisensteinInteger -> (EisensteinInteger, EisensteinInteger, CubicSymbol)
extractPrimaryContributions alpha beta = (gamma, delta, newSymbol)
  where
    newSymbol = stimes (j * m) Omega <> stimes (- m - n) i
    m :+ n = A.quot (delta - 1) 3
    (i, gamma) = getPrimaryDecomposition alphaThreeFree
    (_, delta) = getPrimaryDecomposition beta
    j = wordToInteger jIntWord
    -- This function outputs data such that
    -- @(1 - ω)^jIntWord * alphaThreeFree = alpha@.
    (jIntWord, alphaThreeFree) = splitOff (1 - ω) alpha

-- | This function takes an Eisenstein number @e@ and returns @(symbol, delta)@
-- where @delta@ is its associated primary integer and @symbol@ is the
-- cubic symbol discrepancy between @e@ and @delta@. @delta@ is defined to be
-- the unique associated Eisenstein Integer to @e@ such that
-- \( \textrm{delta} \equiv 1 (\textrm{mod} 3) \).
-- Note that @delta@ exists if and only if @e@ is coprime to 3. In this
-- case, an error message is displayed.
getPrimaryDecomposition :: EisensteinInteger -> (CubicSymbol, EisensteinInteger)
-- This is the case where a common factor between @alpha@ and @beta@ is detected.
-- In this instance @cubicReciprocity@ will return `Zero`.
-- Strictly speaking, this is not a primary decomposition.
getPrimaryDecomposition 0 = (Zero, 0)
getPrimaryDecomposition e = case e `A.rem` 3 of
  1            -> (One, e)
  1 :+ 1       -> (OmegaSquare, -ω * e)
  0 :+ 1       -> (Omega, (-1 - ω) * e)
  (-1) :+ 0    -> (One, -e)
  (-1) :+ (-1) -> (OmegaSquare, ω * e)
  0 :+ (-1)    -> (Omega, (1 + ω) * e)
  _            -> error "Math.NumberTheory.Moduli.CubicSymbol: primary decomposition failed."
