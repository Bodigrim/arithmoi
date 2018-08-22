-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--

{-# LANGUAGE CPP #-}

module Math.NumberTheory.Moduli.DiscreteLogarithm where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import Data.Maybe
import Data.List
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Prefactored

-- | Computes the discrete logarithm. Currently uses a naive search.
discreteLogarithm
  :: KnownNat m
  => PrimitiveRoot m
  -> MultMod m
  -> Natural
discreteLogarithm a b = let n = prefValue . totient . cyclicGroupToModulo . getGroup $ a
                            a' = unPrimitiveRoot a
                            vals = genericTake n $ iterate (<> a') mempty
                         in fromIntegral $ fromJust $ elemIndex b vals
