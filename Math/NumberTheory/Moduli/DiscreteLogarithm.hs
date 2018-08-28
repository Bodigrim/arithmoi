-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Math.NumberTheory.Moduli.DiscreteLogarithm
  ( discreteLogarithm
  ) where

-- import Data.Semigroup
import Data.Maybe
import qualified Data.IntMap.Strict as M
import Numeric.Natural

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Prefactored
import Math.NumberTheory.Powers.Squares

-- | Computes the discrete logarithm. Currently uses the baby-step giant-step method.
discreteLogarithm
  :: forall m. KnownNat m
  => PrimitiveRoot m
  -> MultMod m
  -> Natural
discreteLogarithm a b = discreteLogarithm' (getGroup a) a' aInv (multElement b)
  where
    aElem = unPrimitiveRoot a
    a'    = multElement aElem
    aInv  = multElement $ invertGroup aElem

discreteLogarithm'
  :: forall m. KnownNat m
  => CyclicGroup Natural -- ^ group structure
  -> Mod m               -- ^ a
  -> Mod m               -- ^ a inverse
  -> Mod m               -- ^ b
  -> Natural             -- ^ result
discreteLogarithm' cg a a' b = head [i*m + j | (v,i) <- zip giants [0..m-1], j <- maybeToList (M.lookup v table)]
  where
    n = prefValue (groupSize cg)
    m = integerSquareRoot (n - 1) + 1 -- simple way of ceiling . sqrt
    babies = fromIntegral . getNatVal <$> iterate (* a) 1
    table = M.fromList (zip babies [0..m-1])
    bigGiant = powMod a' m
    giants = fromIntegral . getNatVal <$> iterate (* bigGiant) b
