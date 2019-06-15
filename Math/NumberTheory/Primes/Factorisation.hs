-- |
-- Module:      Math.NumberTheory.Primes.Factorisation
-- Description: Deprecated
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Various functions related to prime factorisation.
-- Many of these functions use the prime factorisation of an 'Integer'.
-- If several of them are used on the same 'Integer', it would be inefficient
-- to recalculate the factorisation, hence there are also functions working
-- on the canonical factorisation, these require that the number be positive
-- and in the case of the Carmichael function that the list of prime factors
-- with their multiplicities is ascending.

module Math.NumberTheory.Primes.Factorisation {-# DEPRECATED "Use 'Math.NumberTheory.Primes.factorise' instead" #-}
    ( -- * Factorisation functions
      -- $algorithm
      -- ** Complete factorisation
      factorise
    , defaultStdGenFactorisation
    , stepFactorisation
    , factorise'
    , defaultStdGenFactorisation'
      -- *** Trial division
    , trialDivisionTo
      -- ** Partial factorisation
    , smallFactors
    , stdGenFactorisation
    , curveFactorisation
      -- *** Single curve worker
    , montgomeryFactorisation
    ) where

import Math.NumberTheory.Primes.Factorisation.Montgomery
import Math.NumberTheory.Primes.Factorisation.TrialDivision

-- $algorithm
--
-- Factorisation of 'Integer's by the elliptic curve algorithm after Montgomery.
-- The algorithm is explained at
-- <http://programmingpraxis.com/2010/04/23/modern-elliptic-curve-factorization-part-1/>
-- and
-- <http://programmingpraxis.com/2010/04/27/modern-elliptic-curve-factorization-part-2/>
--
-- The implementation is not very optimised, so it is not suitable for factorising numbers
-- with several huge prime divisors. However, factors of 20-25 digits are normally found in
-- acceptable time. The time taken depends, however, strongly on how lucky the curve-picking
-- is. With luck, even large factors can be found in seconds; on the other hand, finding small
-- factors (about 12-15 digits) can take minutes when the curve-picking is bad.
--
-- Given enough time, the algorithm should be able to factor numbers of 100-120 digits, but it
-- is best suited for numbers of up to 50-60 digits.
