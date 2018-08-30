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

module Math.NumberTheory.Moduli.DiscreteLogarithm where

-- import Data.Semigroup
import Data.Maybe
import qualified Data.IntMap.Strict as M
import Numeric.Natural
import GHC.Integer.GMP.Internals

import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.UniqueFactorisation

-- | Computes the discrete logarithm. Currently uses the baby-step giant-step method with Bach reduction.
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
discreteLogarithm' cg a b =
  case cg of
    CG2                                    -> 0
       -- the only valid input was a=1, b=1
    CG4                                    -> if b == 1 then 0 else 1
       -- the only possible input here is a=3 with b = 1 or 3
    CGOddPrimePower       (unPrime -> p) k -> discreteLogarithmPP p k (getVal a) (getVal b)
    CGDoubleOddPrimePower (unPrime -> p) k -> discreteLogarithmPP p k (getVal a) (getVal b)

discreteLogarithmPP
  :: Integer -- ^ prime
  -> Word    -- ^ power
  -> Integer -- ^ a
  -> Integer -- ^ b
  -> Natural -- ^ result
discreteLogarithmPP p 1 a b = discreteLogarithmPrime p (a `rem` p) (b `rem` p)
discreteLogarithmPP p k a b = fromInteger result
  where
    baseCase = toInteger $ discreteLogarithmPrime p (a `rem` p) (b `rem` p)
    thetaA = theta p k (a `rem` p^k)
    thetaB = theta p k (b `rem` p^k)
    c = (recipModInteger thetaA (p^(k-1)) * thetaB) `rem` p^(k-1)
    result = chineseRemainder2 (baseCase, p-1) (c, p^(k-1))

discreteLogarithmPrime
  :: Integer -- ^ prime
  -> Integer -- ^ a
  -> Integer -- ^ b
  -> Natural -- ^ result
discreteLogarithmPrime p a b = fromInteger $ head [i*m + j | (v,i) <- zip giants [0..m-1], j <- maybeToList (M.lookup v table)]
  where
    m        = integerSquareRoot (p - 2) + 1 -- simple way of ceiling (sqrt (p-1))
    babies   = fromIntegral <$> iterate (.* a) 1
    table    = M.fromList (zip babies [0..m-1])
    aInv     = recipModInteger a p
    bigGiant = powModInteger aInv m p
    giants   = fromIntegral <$> iterate (.* bigGiant) b
    x .* y   = x * y `rem` p

-- compute the homomorphism theta given in https://math.stackexchange.com/a/1864495/418148
theta
  :: Integer -- ^ prime
  -> Word    -- ^ power
  -> Integer -- ^ k
  -> Integer -- ^ result
theta p s k = (numerator `quot` (p^s)) `rem` (p^(s-1))
  where
    numeratorMod = p^(2*s - 1)
    numerator = (powModInteger k ((p-1)*p^(s-1)) numeratorMod - 1) `rem` numeratorMod
