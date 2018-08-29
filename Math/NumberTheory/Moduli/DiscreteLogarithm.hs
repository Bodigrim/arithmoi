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
{-# LANGUAGE ViewPatterns #-}
-- {-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Math.NumberTheory.Moduli.DiscreteLogarithm
  ( discreteLogarithm
  ) where

-- import Data.Semigroup
import Data.Maybe
import qualified Data.IntMap.Strict as M
import Numeric.Natural
import GHC.Natural

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.UniqueFactorisation

-- | Computes the discrete logarithm. Currently uses the baby-step giant-step method.
discreteLogarithm
  :: forall m. KnownNat m
  => PrimitiveRoot m
  -> MultMod m
  -> Natural
discreteLogarithm a b = discreteLogarithm' (getGroup a) (multElement $ unPrimitiveRoot a) (multElement b)

discreteLogarithm'
  :: forall m. KnownNat m
  => CyclicGroup Natural -- ^ group structure
  -> Mod m               -- ^ a
  -> Mod m               -- ^ b
  -> Natural             -- ^ result
-- discreteLogarithm' cg = discreteLogarithm'' n
--   where
--     n = prefValue (groupSize cg)
discreteLogarithm' cg a b =
  case cg of
    CG2                                    -> 0
       -- the only valid input was a=1, b=1
    CG4                                    -> if b == 1 then 0 else 1
       -- the only possible input here is a=3 with b = 1 or 3
    CGOddPrimePower       (unPrime -> p) k -> discreteLogarithm'' ((p-1)*p^(k-1)) (p^k)   (getNatVal a) (getNatVal $ recip a) (getNatVal b)
    CGDoubleOddPrimePower (unPrime -> p) k -> discreteLogarithm'' ((p-1)*p^(k-1)) (2*p^k) (getNatVal a) (getNatVal $ recip a) (getNatVal b)

discreteLogarithm''
  :: Natural -- ^ group size
  -> Natural -- ^ modulus
  -> Natural -- ^ a
  -> Natural -- ^ a inv
  -> Natural -- ^ b
  -> Natural -- ^ result
discreteLogarithm'' n k a aInv b = head [i*m + j | (v,i) <- zip giants [0..m-1], j <- maybeToList (M.lookup v table)]
  where
    m        = integerSquareRoot (n - 1) + 1 -- simple way of ceiling . sqrt
    babies   = fromIntegral <$> iterate (.* a) 1
    table    = M.fromList (zip babies [0..m-1])
    bigGiant = powModNatural aInv m k
    giants   = fromIntegral <$> iterate (.* bigGiant) b
    x .* y   = x * y `rem` k
