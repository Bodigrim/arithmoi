{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Math.NumberTheory.Moduli.CubicSymbol
  ( CubicSymbol(..)
  , cubicSymbol
  ) where

import Math.NumberTheory.Quadratic.EisensteinIntegers
import Math.NumberTheory.Utils.FromIntegral
import qualified Data.Euclidean as A
import Math.NumberTheory.Utils
import Data.Semigroup
import Data.Mod.Word
import Data.Maybe
import Data.List

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

instance Show CubicSymbol where
  show = \case
    Zero         -> "0"
    Omega        -> "ω"
    OmegaSquare  -> "ω²"
    One          -> "1"

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
cubicSymbol alpha beta = case betaNorm `mod` 3 of
  -- This checks whether beta is coprime to 3, i.e. divisible by @1 - ω@
  -- In particular, it returns an error if @beta == 0@
  0 -> error "Math.NumberTheory.Moduli.CubicSymbol: denominator is not coprime to 3."
  _ -> cubicSymbolHelper alpha beta
  where
    betaNorm = norm beta

cubicSymbolHelper :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
cubicSymbolHelper alpha beta = cubicReciprocity primaryRemainder primaryBeta <> newSymbol
  where
    (primaryRemainder, primaryBeta, symbolExponent) = extractPrimaryContributions remainder beta
    remainder = A.rem alpha beta
    newSymbol = exponentiation unmodularExponent Omega
    unmodularExponent = wordToInt (unMod symbolExponent)
    exponentiation k x = if k == 0 then One else stimes k x

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
extractPrimaryContributions :: EisensteinInteger -> EisensteinInteger -> (EisensteinInteger, EisensteinInteger, Mod 3)
extractPrimaryContributions alpha beta = (gamma, delta, contribution)
  where
    contribution = j*m - i*m -i*n
    [i, j, m, n] = map fromIntegral [iInt, jInt, mInt, nInt]
    mInt :+ nInt = A.quot (delta - 1) 3
    (iInt, gamma) = getPrimaryDecomposition alphaThreeFree
    (_, delta) = getPrimaryDecomposition beta
    jInt = wordToInteger jIntWord
    -- This function outputs data such that
    -- @(1 - ω)^jIntWord * alphaThreeFree = alpha@.
    (jIntWord, alphaThreeFree) = splitOff (1 - ω) alpha

-- This function takes an Eisenstein number and returns its primary decomposition @(powerUnit, factor)@
-- That is, given @e@ coprime with 3, it returns a unique integer (mod 6) @powerUnit@ and a unique
-- Eisenstein number @factor@ such that @(1 + ω)^powerUnit * e = 1 + 3*factor@.
-- Note that L.findIndex cannot return Nothing. This happens only if @e@ is not
-- coprime with 3. This cannot happen since @U.splitOff@ is called just before.
getPrimaryDecomposition :: EisensteinInteger -> (Integer, EisensteinInteger)
-- This is the case where a common factor between @alpha@ and @beta@ is detected.
-- In this instance @cubicReciprocity@ will return @Zero@.
-- Strictly speaking, this is not a primary decomposition.
getPrimaryDecomposition 0 = (0, 0)
getPrimaryDecomposition e = (toInteger powerUnit, factor)
  where
    factor = unit * e
    unit = (1 + ω)^powerUnit
    -- The @6 - _@ ensures @(1 + ω)^powerUnit * e = 1 (mod 3)@
    powerUnit = 6 - fromMaybe
      (error "Math.NumberTheory.Moduli.CubicSymbol: primary decomposition failed.")
      findPowerUnit
    -- Note that the units in @ids@ are ordered in the following way:
    -- The i^th element of @ids@ is @(1 + ω)^i@ starting from i = 0@
    -- That is the i^th unit counting anticlockwise starting with 1.
    -- Note that this index is the inverse of what is needed.
    findPowerUnit = elemIndex remainder ids
    remainder = e `A.rem` 3
