-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches -fno-warn-unused-local-binds #-} -- devel only; to remove
{-# LANGUAGE PartialTypeSignatures #-}

module Math.NumberTheory.Moduli.DiscreteLogarithm where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import Data.Maybe
import Data.List
import Data.IntMap.Strict (IntMap)
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
                            bigGiant = powMultMod a' (- fromIntegral m)
                            giants = fromInteger . getVal . multElement <$> iterate (<> bigGiant) b
                         in head [i*m + j | (v,i) <- zip giants [0..(m-1)], j <- maybeToList $ M.lookup v table]
