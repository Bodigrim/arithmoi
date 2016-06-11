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

import Control.Arrow

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Primes.Factorisation as F (factorise')
import Math.NumberTheory.GaussianIntegers as G

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#else
type Natural = Integer
#endif

newtype SmallPrime = SmallPrime { unSmallPrime :: Word }
  deriving (Eq, Ord, Show)

newtype BigPrime = BigPrime { unBigPrime :: Natural }
  deriving (Eq, Ord, Show)

type family Prime (f :: *) :: *

type instance Prime Int     = SmallPrime
type instance Prime Word    = SmallPrime
type instance Prime Integer = BigPrime
#if MIN_VERSION_base(4,8,0)
type instance Prime Natural = BigPrime
#endif
type instance Prime G.GaussianInteger = G.GaussianInteger

class UniqueFactorization a where
  unPrime   :: Prime a -> a
  factorise :: a -> [(Prime a, Word)]

instance UniqueFactorization Int where
  unPrime = fromIntegral . unSmallPrime
  factorise = factoriseGeneric (SmallPrime . fromIntegral)

instance UniqueFactorization Word where
  unPrime   = unSmallPrime
  factorise = factoriseGeneric (SmallPrime . fromIntegral)

instance UniqueFactorization Integer where
  unPrime   = toInteger . unBigPrime
  factorise = factoriseGeneric (BigPrime . fromInteger)

#if MIN_VERSION_base(4,8,0)
instance UniqueFactorization Natural where
  unPrime   = unBigPrime
  factorise = factoriseGeneric (BigPrime . fromInteger)
#endif

factoriseGeneric :: Integral a => (Integer -> Prime a) -> a -> [(Prime a, Word)]
factoriseGeneric _ 0 = []
factoriseGeneric f n = map (f *** fromIntegral) . filter (\(x, _) -> x /= 0 && x /= 1 && x /= -1) . F.factorise' . toInteger $ n

instance UniqueFactorization G.GaussianInteger where
  unPrime = id
  factorise = map (id *** fromIntegral) . G.factorise
