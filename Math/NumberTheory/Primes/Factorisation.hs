-- |
-- Module:      Math.NumberTheory.Primes.Factorisation
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Various functions related to prime factorisation.
-- Many of these functions use the prime factorisation of an 'Integer'.
-- If several of them are used on the same 'Integer', it would be inefficient
-- to recalculate the factorisation, hence there are also functions working
-- on the canonical factorisation, these require that the number be positive
-- and in the case of the Carmichael function that the list of prime factors
-- with their multiplicities is ascending.

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Math.NumberTheory.Primes.Factorisation
    ( -- * Factorisation functions
      -- $algorithm
      -- ** Complete factorisation
      factorise
    , defaultStdGenFactorisation
    , stepFactorisation
    , factorise'
    , defaultStdGenFactorisation'
      -- *** Factor sieves
    , FactorSieve
    , factorSieve
    , sieveFactor
      -- *** Trial division
    , trialDivisionTo
      -- ** Partial factorisation
    , smallFactors
    , stdGenFactorisation
    , curveFactorisation
      -- *** Single curve worker
    , montgomeryFactorisation
      -- * Totients
    , totient
    , φ
    , TotientSieve
    , totientSieve
    , sieveTotient
    , totientFromCanonical
      -- * Carmichael function
    , carmichael
    , λ
    , CarmichaelSieve
    , carmichaelSieve
    , sieveCarmichael
    , carmichaelFromCanonical
      -- * Moebius function
    , moebius
    , μ
    , moebiusFromCanonical
      -- * Divisors
    , divisors
    , tau
    , τ
    , divisorCount
    , divisorSum
    , sigma
    , σ
    , divisorPowerSum
    , divisorsFromCanonical
    , tauFromCanonical
    , divisorSumFromCanonical
    , sigmaFromCanonical
    ) where

import Data.Set (Set, singleton)

import Math.NumberTheory.Primes.Factorisation.Utils
import Math.NumberTheory.Primes.Factorisation.Montgomery
import Math.NumberTheory.Primes.Factorisation.TrialDivision
import Math.NumberTheory.Primes.Sieve.Misc

{-# DEPRECATED totient, φ, carmichael, λ, moebius, μ, divisors, tau, τ, divisorCount, divisorSum, sigma, σ, divisorPowerSum "Use 'Math.NumberTheory.ArithmeticFunctions'" #-}

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

-- | Calculates the totient of a positive number @n@, i.e.
--   the number of @k@ with @1 <= k <= n@ and @'gcd' n k == 1@,
--   in other words, the order of the group of units in @&#8484;/(n)@.
totient :: Integer -> Integer
totient n
    | n < 1     = error "Totient only defined for positive numbers"
    | n == 1    = 1
    | otherwise = totientFromCanonical (factorise' n)

-- | Alias of 'totient' for people who prefer Greek letters.
φ :: Integer -> Integer
φ = totient

-- | Calculates the Carmichael function for a positive integer, that is,
--   the (smallest) exponent of the group of units in @&#8484;/(n)@.
carmichael :: Integer -> Integer
carmichael n
    | n < 1     = error "Carmichael function only defined for positive numbers"
    | n == 1    = 1
    | otherwise = carmichaelFromCanonical (factorise' n)

-- | Alias of 'carmichael' for people who prefer Greek letters.
λ :: Integer -> Integer
λ = carmichael

-- | Calculates the Moebius function for a positive integer.
moebius :: Integer -> Integer
moebius n
    | n < 1     = error "Carmichael function only defined for positive numbers"
    | n == 1    = 1
    | otherwise = moebiusFromCanonical (factorise' n)

-- | Alias of 'moebius' for people who prefer Greek letters.
μ :: Integer -> Integer
μ = moebius

-- | @'divisors' n@ is the set of all (positive) divisors of @n@.
--   @'divisors' 0@ is an error because we can't create the set of all 'Integer's.
divisors :: Integer -> Set Integer
divisors n
    | n < 0     = divisors (-n)
    | n == 0    = error "Can't create set of divisors of 0"
    | n == 1    = singleton 1
    | otherwise = divisorsFromCanonical (factorise' n)

-- | @'tau' n@ is the number of (positive) divisors of @n@.
--   @'tau' 0@ is an error because @0@ has infinitely many divisors.
tau :: Integer -> Integer
tau n
    | n < 0     = tau (-n)
    | n == 0    = error "0 has infinitely many divisors"
    | n == 1    = 1
    | otherwise = tauFromCanonical (factorise' n)

-- | Alias for 'tau'.
divisorCount :: Integer -> Integer
divisorCount = tau

-- | The sum of all (positive) divisors of a positive number @n@,
--   calculated from its prime factorisation.
divisorSum :: Integer -> Integer
divisorSum n
    | n < 1     = error "divisor sum only defined for positive numbers"
    | n == 1    = 1
    | otherwise = divisorSumFromCanonical (factorise' n)

-- | Alias for 'sigma'.
divisorPowerSum :: Int -> Integer -> Integer
divisorPowerSum = sigma

-- | @'sigma' k n@ is the sum of the @k@-th powers of the
--   (positive) divisors of @n@. @k@ must be non-negative and @n@ positive.
--   For @k == 0@, it is the divisor count (@d^0 = 1@).
sigma :: Int -> Integer -> Integer
sigma 0 n = tau n
sigma 1 n = divisorSum n
sigma k n
    | k < 0     = error "sigma: exponent must be non-negative"
    | n < 1     = error "sigma: n must be positive"
    | n == 1    = 1
    | otherwise = sigmaFromCanonical k (factorise' n)

-- | Alias for 'sigma' for people preferring Greek letters.
σ :: Int -> Integer -> Integer
σ 0 = divisorCount
σ 1 = divisorSum
σ k = divisorPowerSum k

-- | Alias for 'tau' for people preferring Greek letters.
τ :: Integer -> Integer
τ = tau
