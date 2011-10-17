-- |
-- Module:      Math.NumberTheory.Primes.Sieve
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Prime generation using a sieve.
-- Currently, an enhanced sieve of Eratosthenes is used, switching to an
-- Atkin sieve is planned (if I get around to implementing it and it's not slower).
--
-- The sieve used is segmented, with a chunk size chosen to give good (enough)
-- cache locality while still getting something substantial done per chunk.
-- However, that means we must store data for primes up to the square root of
-- where sieving is done, thus sieving primes up to @n@ requires
-- @/O/(sqrt n/log n)@ space.
module Math.NumberTheory.Primes.Sieve
    ( primes
    , sieveFrom
    , PrimeSieve
    , primeSieve
    , psieveList
    , psieveFrom
    , primeList
    ) where

import Math.NumberTheory.Primes.Sieve.Eratosthenes
