-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--

module Math.NumberTheory.Moduli.DiscreteLogarithm where

import Data.Semigroup
import Data.Maybe
-- import Data.List
import qualified Data.IntMap.Strict as M
import Numeric.Natural

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Prefactored
import Math.NumberTheory.Powers.Squares

-- | Computes the discrete logarithm. Currently uses a naive search.
discreteLogarithm
  :: KnownNat m
  => PrimitiveRoot m
  -> MultMod m
  -> Natural
-- discreteLogarithm a b = let n = prefValue . groupSize . getGroup $ a
--                             a' = unPrimitiveRoot a
--                             vals = genericTake n $ iterate (<> a') mempty
--                          in fromIntegral $ fromJust $ elemIndex b vals

discreteLogarithm a b = let n = prefValue . groupSize . getGroup $ a
                            a' = unPrimitiveRoot a
                            m = integerSquareRoot (n - 1) + 1 -- simple way of ceiling . sqrt
                            babies = fromInteger . getVal . multElement <$> iterate (<> a') mempty
                            table = M.fromList $ zip babies [0..(m-1)]
                            bigGiant = stimes (- toInteger m) a'
                            giants = fromInteger . getVal . multElement <$> iterate (<> bigGiant) b
                         in head [i*m + j | (v,i) <- zip giants [0..(m-1)], j <- maybeToList $ M.lookup v table]
