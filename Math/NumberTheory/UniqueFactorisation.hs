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
  ( Prime
  , UniqueFactorisation(..)
  ) where

import Control.Arrow
import Data.Coerce

import qualified Math.NumberTheory.Primes.Factorisation as F (factorise)
import Math.NumberTheory.Primes.Testing.Probabilistic as T (isPrime)
import Math.NumberTheory.Primes.Types (Prime, Prm(..), PrimeNat(..))
import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E
import qualified Math.NumberTheory.Quadratic.GaussianIntegers as G
import Math.NumberTheory.Utils.FromIntegral

import Numeric.Natural

type instance Prime G.GaussianInteger = GaussianPrime

-- | The following invariant must hold for @n /= 0@:
--
-- > abs n == abs (product (map (\(p, k) -> unPrime p ^ k) (factorise n)))
--
-- The result of 'factorise' should not contain zero powers and should not change after multiplication of the argument by domain's unit.
class UniqueFactorisation a where
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
  unPrime   = coerce wordToInt
  factorise = map (coerce integerToWord *** intToWord) . F.factorise . intToInteger

instance UniqueFactorisation Word where
  unPrime   = coerce
  factorise = map (coerce integerToWord *** intToWord) . F.factorise . wordToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (coerce n) else Nothing

instance UniqueFactorisation Integer where
  unPrime   = coerce naturalToInteger
  factorise = map (coerce integerToNatural *** intToWord) . F.factorise
  isPrime n = if T.isPrime n then Just (coerce $ integerToNatural $ abs n) else Nothing

instance UniqueFactorisation Natural where
  unPrime   = coerce
  factorise = map (coerce integerToNatural *** intToWord) . F.factorise . naturalToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (coerce n) else Nothing

newtype GaussianPrime = GaussianPrime { _unGaussianPrime :: G.GaussianInteger }
  deriving (Eq, Show)

instance UniqueFactorisation G.GaussianInteger where
  unPrime = coerce

  factorise 0 = []
  factorise g = map (coerce *** intToWord) $ G.factorise g

newtype EisensteinPrime = EisensteinPrime { _unEisensteinPrime :: E.EisensteinInteger }
  deriving (Eq, Show)

type instance Prime E.EisensteinInteger = EisensteinPrime

instance UniqueFactorisation E.EisensteinInteger where
  unPrime = coerce

  factorise 0 = []
  factorise e = map (coerce *** intToWord) $ E.factorise e
