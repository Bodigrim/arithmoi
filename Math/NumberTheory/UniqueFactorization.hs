-- |
-- Module:      Math.NumberTheory.UniqueFactorization
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- An abstract type class for unique factorization domains.
--

{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Math.NumberTheory.UniqueFactorization
  ( Prime
  , UniqueFactorization(..)
  ) where

import Data.Coerce

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

newtype SmallPrime = SmallPrime { _unSmallPrime :: Word }
  deriving (Eq, Ord, Show)

newtype BigPrime = BigPrime { _unBigPrime :: Natural }
  deriving (Eq, Ord, Show)

type family Prime (f :: *) :: *

type instance Prime Int     = SmallPrime
type instance Prime Word    = SmallPrime
type instance Prime Integer = BigPrime
type instance Prime Natural = BigPrime

type instance Prime G.GaussianInteger = G.GaussianInteger

class UniqueFactorization a where
  unPrime   :: Prime a -> a
  factorise :: a -> [(Prime a, Word)]

instance UniqueFactorization Int where
  unPrime = unsafeCoerce
  factorise = factoriseGeneric

instance UniqueFactorization Word where
  unPrime   = coerce
  factorise = factoriseGeneric

instance UniqueFactorization Integer where
  unPrime   = unsafeCoerce
  factorise = factoriseGeneric

#if MIN_VERSION_base(4,8,0)
instance UniqueFactorization Natural where
  unPrime   = coerce
  factorise = factoriseGeneric
#endif

factoriseGeneric :: Integral a => a -> [(Prime a, Word)]
factoriseGeneric
  = (\m -> if m <= 1
            then []
            else unsafeCoerce . F.factorise' . unsafeCoerce $ m
    )
  . abs

instance UniqueFactorization G.GaussianInteger where
  unPrime   = id
  factorise = unsafeCoerce . G.factorise
