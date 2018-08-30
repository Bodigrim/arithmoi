-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--

{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Moduli.DiscreteLogarithm
  ( discreteLogarithm
  ) where

import qualified Data.IntMap.Strict as M
import Data.Maybe                             (maybeToList)
import Numeric.Natural                        (Natural)
import GHC.Integer.GMP.Internals              (recipModInteger, powModInteger)

import Math.NumberTheory.Moduli.Chinese       (chineseRemainder2)
import Math.NumberTheory.Moduli.Class         (KnownNat, MultMod(..), Mod, getVal)
import Math.NumberTheory.Moduli.PrimitiveRoot (PrimitiveRoot(..), CyclicGroup(..))
import Math.NumberTheory.Powers.Squares       (integerSquareRoot)
import Math.NumberTheory.UniqueFactorisation  (unPrime)

-- | Computes the discrete logarithm. Currently uses the baby-step giant-step method with Bach reduction.
discreteLogarithm
  :: KnownNat m
  => PrimitiveRoot m
  -> MultMod m
  -> Natural
discreteLogarithm a b = discreteLogarithm' (getGroup a) (multElement $ unPrimitiveRoot a) (multElement b)

discreteLogarithm'
  :: KnownNat m
  => CyclicGroup Natural  -- ^ group structure (must be the multiplicative group mod m)
  -> Mod m                -- ^ a
  -> Mod m                -- ^ b
  -> Natural              -- ^ result
discreteLogarithm' cg a b =
  case cg of
    CG2                                    -> 0
       -- the only valid input was a=1, b=1
    CG4                                    -> if b == 1 then 0 else 1
       -- the only possible input here is a=3 with b = 1 or 3
    CGOddPrimePower       (unPrime -> p) k -> discreteLogarithmPP p k (getVal a) (getVal b)
    CGDoubleOddPrimePower (unPrime -> p) k -> discreteLogarithmPP p k (getVal a `rem` p^k) (getVal b `rem` p^k)
       -- we have the isomorphism t -> t `rem` p^k from (Z/2p^kZ)* -> (Z/p^kZ)*

discreteLogarithmPP
  :: Integer -- ^ prime
  -> Word    -- ^ power
  -> Integer -- ^ a
  -> Integer -- ^ b
  -> Natural -- ^ result
discreteLogarithmPP p 1 a b = discreteLogarithmPrime p a b
discreteLogarithmPP p k a b = fromInteger result
  where
    baseSol = toInteger $ discreteLogarithmPrime p (a `rem` p) (b `rem` p)
    thetaA  = theta p k a
    thetaB  = theta p k b
    c       = (recipModInteger thetaA (p^(k-1)) * thetaB) `rem` p^(k-1)
    result  = chineseRemainder2 (baseSol, p-1) (c, p^(k-1))

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
{-# INLINE theta #-}
theta
  :: Integer -- ^ prime
  -> Word    -- ^ power
  -> Integer -- ^ a
  -> Integer -- ^ result
theta p k a = (numerator `quot` p^k) `rem` p^(k-1)
  where
    numeratorMod = p^(2*k - 1)
    numerator    = (powModInteger a ((p-1)*p^(k-1)) numeratorMod - 1) `rem` numeratorMod
