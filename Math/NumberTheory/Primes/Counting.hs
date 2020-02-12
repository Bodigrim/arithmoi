-- |
-- Module:      Math.NumberTheory.Primes.Counting
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Number of primes not exceeding @n@, @&#960;(n)@, and @n@-th prime; also fast, but
-- reasonably accurate approximations to these.
module Math.NumberTheory.Primes.Counting
    ( -- * Exact functions
      primeCount
    , primeCountMaxArg
    , nthPrime
      -- * Approximations
    , approxPrimeCount
    , approxPrimeCountOverestimateLimit
    , nthPrimeApprox
    , nthPrimeApproxUnderestimateLimit
    ) where

import Math.NumberTheory.Primes.Counting.Impl
import Math.NumberTheory.Primes.Counting.Approximate
