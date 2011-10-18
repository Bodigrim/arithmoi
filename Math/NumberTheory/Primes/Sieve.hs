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
    ( -- * Limitations
      -- $limits

      -- * Sieves and lists
      primes
    , sieveFrom
    , PrimeSieve
    , primeSieve
    , psieveList
    , psieveFrom
    , primeList
    ) where

import Math.NumberTheory.Primes.Sieve.Eratosthenes

-- $limits
--
-- There are three factors limiting the range of these sieves.
--
-- (1) Memory
--
-- (2) Overflow
--
-- (3) The internal representation of the state
--
-- An Eratosthenes type sieve needs to store the primes up to the square root of
-- the currently sieved region, thus requires @/O/(sqrt n\/log n)@ space.We store @16@ bytes
-- of information per prime, thus a Gigabyte of memory takes you to about @1.6*10^18@.
-- The @log@ doesn't change much in that range, so as a first approximation, doubling
-- the storage increases the sieve range by a factor of four.
--
-- On a 64-bit system, this is (currently) the only limitation to be concerned with, but
-- with more than four Terabyte of memory, the fact that the internal representation
-- currently limits the sieve range to about @6.8*10^25@ could become relevant.
-- Overflow in array indexing doesn't become a concern before memory and internal
-- representation would allow to sieve past @10^37@.
--
-- On a 32-bit system, the internal representation imposes no additional limits,
-- but overflow has to be reckoned with. On the one hand, the fact that arrays are
-- 'Int'-indexed restricts the size of the prime store, on the other hand, overflow
-- in calculating the indices to cross off multiples is possible before running out
-- of memory. The former limits the upper bound of the monolithic 'primeSieve' to
-- shortly above @8*10^9@, the latter limits the range of the segmented sieves to
-- about @1.7*10^18@.
