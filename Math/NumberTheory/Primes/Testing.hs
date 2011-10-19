-- |
-- Module:      Math.NumberTheory.Primes.Testing
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Primality tests.
module Math.NumberTheory.Primes.Testing
    ( -- * Standard tests
      isPrime
    , isCertifiedPrime
      -- * Partial tests
    , bailliePSW
    , millerRabinV
    , isStrongFermatPP
    , isFermatPP
      -- * Using a sieve
    , FactorSieve
    , fsIsPrime
      -- * Trial division
    , trialDivisionPrimeTo
    ) where

import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Primes.Testing.Certified
import Math.NumberTheory.Primes.Factorisation.TrialDivision
import Math.NumberTheory.Primes.Sieve.Misc

-- | Test primality using a 'FactorSieve'. If @n@ is out of bounds
--   of the sieve, fall back to 'isPrime'.
fsIsPrime :: FactorSieve -> Integer -> Bool
fsIsPrime fs n
    | n < 0     = fsIsPrime fs (-n)
    | n <= fromIntegral (fsBound fs)    = fsPrimeTest fs n
    | otherwise = isPrime n

