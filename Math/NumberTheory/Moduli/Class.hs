-- |
-- Module:      Math.NumberTheory.Moduli.Class
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Safe modular arithmetic with modulo on type level.
--

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UnboxedTuples              #-}

module Math.NumberTheory.Moduli.Class
  ( -- * Known modulo
    Mod
  , getVal
  , getNatVal
  , getMod
  , getNatMod
  , invertMod
  , powMod
  , (^%)
  -- * Multiplicative group
  , MultMod
  , multElement
  , isMultElement
  , invertGroup
  -- * Unknown modulo
  , SomeMod(..)
  , modulo
  , invertSomeMod
  , powSomeMod
  -- * Re-exported from GHC.TypeNats.Compat
  , KnownNat
  ) where

import Data.Mod
import GHC.Natural
import GHC.TypeNats (KnownNat, natVal)

import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.SomeMod

-- | Linking type and value levels: extract modulo @m@ as a value.
getMod :: KnownNat m => Mod m -> Integer
getMod = toInteger . natVal
{-# INLINE getMod #-}

-- | Linking type and value levels: extract modulo @m@ as a value.
getNatMod :: KnownNat m => Mod m -> Natural
getNatMod = natVal
{-# INLINE getNatMod #-}

-- | The canonical representative of the residue class, always between 0 and m-1 inclusively.
getVal :: Mod m -> Integer
getVal = toInteger . unMod
{-# INLINE getVal #-}

-- | The canonical representative of the residue class, always between 0 and m-1 inclusively.
getNatVal :: Mod m -> Natural
getNatVal = unMod
{-# INLINE getNatVal #-}

-- | Synonym of '(^%)'.
powMod :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
powMod = (^%)
{-# INLINE powMod #-}
