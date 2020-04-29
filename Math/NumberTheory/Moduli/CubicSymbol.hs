{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.Moduli.CubicSymbol
  ( CubicSymbol(..)
  , symbolToNum
  , cubicSymbol
  ) where

import Math.NumberTheory.Quadratic.EisensteinIntegers
import Math.NumberTheory.Utils.FromIntegral
import qualified Data.Euclidean as A
import Math.NumberTheory.Utils
import Data.Semigroup

data CubicSymbol = Zero | Omega | OmegaSquare | One deriving (Eq)

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

symbolToNum :: CubicSymbol -> EisensteinInteger
symbolToNum = \case
  Zero        -> 0
  Omega       -> ω
  OmegaSquare -> -1 - ω
  One         -> 1
-- The algorithm cubicSymbol takes two Eisentein numbers @alpha@ and @beta@ and returns
-- their cubic residue. It is divided in the following steps.

-- 1) Check whether @beta@ is coprime to 3.
-- 2) Replace @alpha@ by the remainder of @alpha@ mod @beta@
--    This does not affect the cubic symbol.
-- 3) Replace @alpha@ and @beta@ by their associated primary
--    divisors and keep track of how their cubic residue changes.
-- 4) Check if any of the two numbers is a zero or a unit. If it
--    is, return their cubic residue.
-- 5) If not, invoke cubic reciprocity by swapping @alpha@ and
--    @beta@. Note both numbers have to be primary.
--    Return to Step 2.

-- This function takes two Eisenstein integers and returns their cubic residue character.
-- Note that the second argument must be coprime to 3 else the algorithm returns an error.
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

-- This function first checks if its arguments are zeros or units. If they are not,
-- it invokes cubic reciprocity by calling cubicSymbolHelper with swapped arguments.
cubicReciprocity :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
-- Note @cubicReciprocity 0 1 = One@. It turns out it is better to adopt this convention.
cubicReciprocity _ 1 = One
-- Checks if first argument is zero. Note the second argument is never zero.
cubicReciprocity 0 _ = Zero
-- This checks if the first argument is a unit. Because it's primary,
-- it is enough to pattern match with 1.
cubicReciprocity 1 _ = One
-- Otherwise, cubic reciprocity is called.
cubicReciprocity alpha beta = cubicSymbolHelper beta alpha

-- This function takes two Eisenstein intgers @alpha@ and @beta@ and returns three
-- arguments @(gamma, delta, contribution)@. @gamma@ and @delta@ are the associated
-- primary numbers to alpha and beta respectively. @contribution@ is a an integer
-- defined mod 3 which measures the difference between the cubic residue of @alpha@
-- and @beta@ with respect to the cubic residue of @gamma@ and @delta@.
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

-- This function takes an Eisenstein number and returns its primary decomposition
-- @(symbol, delta)@. That is, given @e@ coprime with 3, it finds a unique integer
-- x (mod 6) such that (1 + ω)^x * e = 1 (mod 3).
-- It then returns @symbol = x^2@ and @delta = (1 + ω)^x * e@.
-- Note that the error message should not be displayed. This happens only if @e@ is not
-- coprime with 3. This cannot happen since @U.splitOff@ is called just before.
getPrimaryDecomposition :: EisensteinInteger -> (CubicSymbol, EisensteinInteger)
-- This is the case where a common factor between @alpha@ and @beta@ is detected.
-- In this instance @cubicReciprocity@ will return @Zero@.
-- Strictly speaking, this is not a primary decomposition.
getPrimaryDecomposition 0 = (Zero, 0)
getPrimaryDecomposition e = case e `A.rem` 3 of
  1            -> (One, e)
  1 :+ 1       -> (OmegaSquare, -ω * e)
  0 :+ 1       -> (Omega, (-1 - ω) * e)
  -1           -> (One, -e)
  (-1) :+ (-1) -> (OmegaSquare, ω * e)
  0 :+ (-1)    -> (Omega, (1 + ω) * e)
  _            -> error "Math.NumberTheory.Moduli.CubicSymbol: primary decomposition failed."
