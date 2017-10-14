-- |
-- Module:      Math.NumberTheory.Primes.Types
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This is an internal module, defining types for primes.
-- Should not be exposed to users.
--

{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Math.NumberTheory.Primes.Types
  ( Prime
  , Prm(..)
  , PrimeNat(..)
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Numeric.Natural

newtype Prm = Prm { unPrm :: Word }
  deriving (Eq, Ord)

instance Show Prm where
  show (Prm p) = "Prm " ++ show p

newtype PrimeNat = PrimeNat { unPrimeNat :: Natural }
  deriving (Eq, Ord)

instance Show PrimeNat where
  show (PrimeNat p) = "PrimeNat " ++ show p

-- | Type of primes of a given unique factorisation domain.
--
-- @abs (unPrime n) == unPrime n@ must hold for all @n@ of type @Prime t@
type family Prime (f :: *) :: *

type instance Prime Int     = Prm
type instance Prime Word    = Prm
type instance Prime Integer = PrimeNat
type instance Prime Natural = PrimeNat
