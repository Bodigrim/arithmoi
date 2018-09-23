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
  ( Prime(..)
  ) where

import GHC.Generics
import Control.DeepSeq

-- | Wrapper for prime elements of @a@.
newtype Prime a = Prime
  { unPrime :: a -- ^ Unwrap prime element.
  }
  deriving (Eq, Ord, Generic)

instance NFData a => NFData (Prime a)

instance Show a => Show (Prime a) where
  showsPrec d (Prime p) r = (if d > 10 then "(" ++ s ++ ")" else s) ++ r
    where
      s = "Prime " ++ show p
