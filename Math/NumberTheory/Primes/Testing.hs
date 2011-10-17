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
      -- $certificates
    , bailliePSW
    , millerRabinV
    , isStrongFermatPP
    , isFermatPP
      -- * Using a sieve
    , FactorSieve
    , fsIsPrime
    ) where

import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Primes.Sieve.Misc

-- | Test primality using a 'FactorSieve'. If @n@ is out of bounds
--   of the sieve, fall back to 'isPrime'.
fsIsPrime :: FactorSieve -> Integer -> Bool
fsIsPrime fs n
    | n < 0     = fsIsPrime fs (-n)
    | n <= fromIntegral (fsBound fs)    = fsPrimeTest fs n
    | otherwise = isPrime n

-- $certificates
--
-- The tests in this module may wrongly consider some composite numbers as prime.
-- For the Baillie-PSW test, no pseudoprimes are known, and it is known that none
-- exist below @2^64@, so for most practical purposes it can be regarded as conclusive.
-- Nevertheless, it is desirable to certify numbers passing it as primes (or find that
-- they are composite). The addition of prime certificates is planned for the next release.
