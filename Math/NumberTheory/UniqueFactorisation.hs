-- |
-- Module:      Math.NumberTheory.UniqueFactorisation
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- An abstract type class for unique factorisation domains.
--

{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DefaultSignatures #-}

module Math.NumberTheory.UniqueFactorisation
  ( Prime
  , unPrime
  , UniqueFactorisation(..)
  ) where

import Control.Arrow
import Data.Coerce

import qualified Math.NumberTheory.Primes.Factorisation as F (factorise)
import Math.NumberTheory.Primes.Testing.Probabilistic as T (isPrime)
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Utils.FromIntegral

import Numeric.Natural

-- | A class for unique factorisation domains.
class UniqueFactorisation a where
  -- | Factorise a number into a product of prime powers.
  -- Factorisation of 0 is an undefined behaviour. Otherwise
  -- following invariants hold:
  --
  -- > abs n == abs (product (map (\(p, k) -> unPrime p ^ k) (factorise n)))
  -- > all ((> 0) . snd) (factorise n)
  --
  -- >>> factorise (1 :: Integer)
  -- []
  -- >>> factorise (-1 :: Integer)
  -- []
  -- >>> factorise (6 :: Integer)
  -- [(Prime 2,1),(Prime 3,1)]
  -- >>> factorise (-108 :: Integer)
  -- [(Prime 2,2),(Prime 3,3)]
  --
  -- This function is a replacement
  -- for 'Math.NumberTheory.Primes.Factorisation.factorise'.
  -- If you were looking for the latter, please import
  -- "Math.NumberTheory.Primes.Factorisation" instead of this module.
  factorise :: a -> [(Prime a, Word)]
  -- | Check whether an argument is prime.
  -- If it is then return an associated prime.
  --
  -- >>> isPrime (3 :: Integer)
  -- Just (Prime 3)
  -- >>> isPrime (4 :: Integer)
  -- Nothing
  -- >>> isPrime (-5 :: Integer)
  -- Just (Prime 5)
  --
  -- This function is a replacement
  -- for 'Math.NumberTheory.Primes.Testing.isPrime'.
  -- If you were looking for the latter, please import
  -- "Math.NumberTheory.Primes.Testing" instead of this module.
  isPrime   :: a -> Maybe (Prime a)

instance UniqueFactorisation Int where
  factorise = map (Prime . integerToInt *** id) . F.factorise . intToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (Prime $ abs n) else Nothing

instance UniqueFactorisation Word where
  factorise = map (coerce integerToWord *** id) . F.factorise . wordToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (Prime n) else Nothing

instance UniqueFactorisation Integer where
  factorise = coerce . F.factorise
  isPrime n = if T.isPrime n then Just (Prime $ abs n) else Nothing

instance UniqueFactorisation Natural where
  factorise = map (coerce integerToNatural *** id) . F.factorise . naturalToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (Prime n) else Nothing
