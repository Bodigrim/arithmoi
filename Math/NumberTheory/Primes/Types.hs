-- |
-- Module:      Math.NumberTheory.Primes.Types
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This is an internal module, defining types for primes.
-- Should not be exposed to users.
--

{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DeriveGeneric #-}

module Math.NumberTheory.Primes.Types
  ( Prime(..)
  ) where

import GHC.Generics
import Control.DeepSeq

-- | Wrapper for prime elements of @a@. It is supposed to be constructed
-- by 'Math.NumberTheory.Primes.nextPrime' / 'Math.NumberTheory.Primes.precPrime'.
-- and eliminated by 'unPrime'.
--
-- One can leverage 'Enum' instance to generate lists of primes.
-- Here are some examples.
--
-- *  Generate primes from the given interval:
--
--    >>> [nextPrime 101 .. precPrime 130]
--    [Prime 101,Prime 103,Prime 107,Prime 109,Prime 113,Prime 127]
--
-- *  Generate an infinite list of primes:
--
--    >>> [nextPrime 101 ..]
--    [Prime 101,Prime 103,Prime 107,Prime 109,Prime 113,Prime 127...
--
-- *  Generate primes from the given interval of form p = 6k+5:
--
--    >>> [nextPrime 101, nextPrime 107 .. precPrime 150]
--    [Prime 101,Prime 107,Prime 113,Prime 131,Prime 137,Prime 149]
--
-- *  Get next prime:
--
--    >>> succ (nextPrime 101)
--    Prime 103
--
-- *  Get previous prime:
--
--    >>> prec (nextPrime 101)
--    Prime 97
--
-- *  Count primes less than a given number (cf. 'Math.NumberTheory.Primes.Counting.approxPrimeCount'):
--
--    >>> fromEnum (precPrime 100)
--    25
--
-- *  Get 25-th prime number (cf. 'Math.NumberTheory.Primes.Counting.nthPrimeApprox'):
--
--    >>> toEnum 25 :: Prime Int
--    Prime 97
--
newtype Prime a = Prime
  { unPrime :: a -- ^ Unwrap prime element.
  }
  deriving (Eq, Ord, Generic)

instance NFData a => NFData (Prime a)

instance Show a => Show (Prime a) where
  showsPrec d (Prime p) r = (if d > 10 then "(" ++ s ++ ")" else s) ++ r
    where
      s = "Prime " ++ show p
