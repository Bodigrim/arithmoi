-- |
-- Module:      Math.NumberTheory.Primes
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
module Math.NumberTheory.Primes
    ( Prime
    , unPrime
    , UniqueFactorisation(..)
    , module Math.NumberTheory.Primes.Sieve
    , module Math.NumberTheory.Primes.Counting
    ) where

import Math.NumberTheory.Primes.Sieve
import Math.NumberTheory.Primes.Counting

import Math.NumberTheory.UniqueFactorisation
