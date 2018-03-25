-- |
-- Module:      Math.NumberTheory.UniqueFactorisation
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- An abstract type class for unique factorisation domains.
--

{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DefaultSignatures #-}

module Math.NumberTheory.UniqueFactorisation
  ( UniqueFactorisation(..)
  ) where

import Control.Arrow
import Data.Coerce

import qualified Math.NumberTheory.Primes.Factorisation as F (factorise)
import Math.NumberTheory.Primes.Testing.Probabilistic as T (isPrime)
import Math.NumberTheory.Primes.Types (Prm(..), PrimeNat(..))
import Math.NumberTheory.Utils.FromIntegral

import Numeric.Natural

-- | The following invariant must hold for @n /= 0@:
--
-- > abs n == abs (product (map (\(p, k) -> unPrime p ^ k) (factorise n)))
--
-- The result of 'factorise' should not contain zero powers and should not change after multiplication of the argument by domain's unit.
class UniqueFactorisation a where
  type Prime a
  unPrime   :: Prime a -> a
  factorise :: a -> [(Prime a, Word)]
  isPrime   :: a -> Maybe (Prime a)

  default isPrime :: (Eq a, Num a) => a -> Maybe (Prime a)
  isPrime 0 = Nothing
  isPrime n = case factorise n of
    [(p, 1)] -> Just p
    _        -> Nothing

  {-# MINIMAL unPrime, factorise #-}

instance UniqueFactorisation Int where
  type Prime Int = Prm
  unPrime   = coerce wordToInt
  factorise = map (coerce integerToWord *** id) . F.factorise . intToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (coerce $ intToWord $ abs n) else Nothing

instance UniqueFactorisation Word where
  type Prime Word = Prm
  unPrime   = coerce
  factorise = map (coerce integerToWord *** id) . F.factorise . wordToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (coerce n) else Nothing

instance UniqueFactorisation Integer where
  type Prime Integer = PrimeNat
  unPrime   = coerce naturalToInteger
  factorise = map (coerce integerToNatural *** id) . F.factorise
  isPrime n = if T.isPrime n then Just (coerce $ integerToNatural $ abs n) else Nothing

instance UniqueFactorisation Natural where
  type Prime Natural = PrimeNat
  unPrime   = coerce
  factorise = map (coerce integerToNatural *** id) . F.factorise . naturalToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (coerce n) else Nothing
