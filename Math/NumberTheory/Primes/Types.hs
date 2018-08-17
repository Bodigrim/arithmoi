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

{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DeriveGeneric #-}

module Math.NumberTheory.Primes.Types
  ( Prime
  , Prm(..)
  , PrimeNat(..)
  ) where

import Numeric.Natural
import GHC.Generics

newtype Prm = Prm { unPrm :: Word }
  deriving (Eq, Ord, Generic)

instance Show Prm where
  showsPrec d (Prm p) r = (if d > 10 then "(" ++ s ++ ")" else s) ++ r
    where
      s = "Prm " ++ show p

newtype PrimeNat = PrimeNat { unPrimeNat :: Natural }
  deriving (Eq, Ord, Generic)

instance Show PrimeNat where
  showsPrec d (PrimeNat p) r = (if d > 10 then "(" ++ s ++ ")" else s) ++ r
    where
      s = "PrimeNat " ++ show p

-- | Type of primes of a given unique factorisation domain.
--
-- @abs (unPrime n) == unPrime n@ must hold for all @n@ of type @Prime t@
type family Prime (f :: *) :: *

type instance Prime Int     = Prm
type instance Prime Word    = Prm
type instance Prime Integer = PrimeNat
type instance Prime Natural = PrimeNat
