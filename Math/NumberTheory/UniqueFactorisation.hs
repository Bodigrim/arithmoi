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

{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Math.NumberTheory.UniqueFactorisation
  ( Prime
  , UniqueFactorisation(..)
  ) where

import Control.Arrow
import Data.Coerce

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Primes.Factorisation as F (factorise')
import Math.NumberTheory.Primes.Types (Prime, Prm(..), PrimeNat(..))
import Math.NumberTheory.GaussianIntegers as G
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

instance UniqueFactorisation Int where
  unPrime   = coerce wordToInt
  factorise m' = if m <= 1
                then []
                else map (coerce integerToWord *** intToWord) . F.factorise' . intToInteger $ m
                  where
                    m = abs m'

instance UniqueFactorisation Word where
  unPrime     = coerce
  factorise m = if m <= 1
                  then []
                  else map (coerce integerToWord *** intToWord) . F.factorise' . wordToInteger $ m


instance UniqueFactorisation Integer where
  unPrime      = coerce naturalToInteger
  factorise m' = if m <= 1
                then []
                else map (coerce integerToNatural *** intToWord) . F.factorise' $ m
                  where
                    m = abs m'

instance UniqueFactorisation Natural where
  unPrime   = coerce
  factorise m = if m <= 1
                  then []
                  else map (coerce integerToNatural *** intToWord) . F.factorise' . naturalToInteger $ m

newtype GaussianPrime = GaussianPrime { _unGaussianPrime :: G.GaussianInteger }
  deriving (Eq, Show)

instance UniqueFactorisation G.GaussianInteger where
  unPrime = coerce

  factorise 0 = []
  factorise g = map (coerce *** intToWord) $ filter (\(h, _) -> abs h /= 1) $ G.factorise g
