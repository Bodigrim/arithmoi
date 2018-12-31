-- |
-- Module:      Math.NumberTheory.Primes.Testing
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Primality tests.

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Math.NumberTheory.Primes.Testing
    ( -- * Standard tests
      isPrime
    , isCertifiedPrime
      -- * Partial tests
    , bailliePSW
    , millerRabinV
    , isStrongFermatPP
    , isFermatPP
      -- * Trial division
    , trialDivisionPrimeTo
    ) where

import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Primes.Testing.Certified
import Math.NumberTheory.Primes.Factorisation.TrialDivision
