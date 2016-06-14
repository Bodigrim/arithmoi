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

{- Coercions below relies on the fact that runtime representations
   of small non-negative Int, Word, Integer and Natural coincides.
-}
import Unsafe.Coerce

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Primes.Factorisation as F (factorise')
import Math.NumberTheory.GaussianIntegers as G

import Numeric.Natural

#if MIN_VERSION_base(4,7,0)
import Data.Coerce
#else
coerce :: a -> b
coerce = unsafeCoerce
#endif

newtype SmallPrime = SmallPrime { _unSmallPrime :: Word }
  deriving (Eq, Ord, Show)

newtype BigPrime = BigPrime { _unBigPrime :: Natural }
  deriving (Eq, Ord, Show)

-- | Type of primes of a given unique factorisation domain.
-- When the domain has exactly one unit, @Prime t = t@,
-- but when units are multiple more restricted types
-- (or at least newtypes) should be specified.
--
-- @abs n == n@ must hold for all values of @Prime t@
type family Prime (f :: *) :: *

type instance Prime Int     = SmallPrime
type instance Prime Word    = SmallPrime
type instance Prime Integer = BigPrime
type instance Prime Natural = BigPrime

type instance Prime G.GaussianInteger = G.GaussianInteger

-- | The following invariant must hold for @n /= 0@:
--
-- > n == signum n * product (map (\(p, k) -> unPrime p ^ k) (factorise n))
--
-- The result of 'factorise' should not contain zero powers and should not change after multiplication of the argument by domain's unit.
class UniqueFactorisation a where
  unPrime   :: Prime a -> a
  factorise :: a -> [(Prime a, Word)]

instance UniqueFactorisation Int where
  unPrime = unsafeCoerce
  factorise = factoriseGeneric

instance UniqueFactorisation Word where
  unPrime   = coerce
  factorise = factoriseGeneric

instance UniqueFactorisation Integer where
  unPrime   = unsafeCoerce
  factorise = factoriseGeneric

instance UniqueFactorisation Natural where
  unPrime   = coerce
  factorise = factoriseGeneric

factoriseGeneric :: Integral a => a -> [(Prime a, Word)]
factoriseGeneric
  = (\m -> if m <= 1
            then []
            else unsafeCoerce . F.factorise' . unsafeCoerce $ m
    )
  . abs

instance UniqueFactorisation G.GaussianInteger where
  unPrime   = id
  factorise = unsafeCoerce . G.factorise
