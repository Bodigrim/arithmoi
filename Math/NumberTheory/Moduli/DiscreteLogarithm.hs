-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
--

{-# LANGUAGE ViewPatterns        #-}

module Math.NumberTheory.Moduli.DiscreteLogarithm
  ( discreteLogarithm
  ) where

import Numeric.Natural                                     (Natural)

import Math.NumberTheory.Moduli.Class                      (KnownNat, MultMod(..), Mod, getVal)
import Math.NumberTheory.Moduli.DiscreteLogarithm.Internal (discreteLogarithmPP)
import Math.NumberTheory.Moduli.PrimitiveRoot              (PrimitiveRoot(..), CyclicGroup(..))
import Math.NumberTheory.UniqueFactorisation               (unPrime)

-- | Computes the discrete logarithm. Currently uses a combination of the baby-step
-- giant-step method and Pollard's rho algorithm, with Bach reduction.
discreteLogarithm :: KnownNat m => PrimitiveRoot m -> MultMod m -> Natural
discreteLogarithm a b = discreteLogarithm' (getGroup a) (multElement $ unPrimitiveRoot a) (multElement b)

discreteLogarithm'
  :: KnownNat m
  => CyclicGroup Natural  -- ^ group structure (must be the multiplicative group mod m)
  -> Mod m                -- ^ a
  -> Mod m                -- ^ b
  -> Natural              -- ^ result
discreteLogarithm' cg a b =
  case cg of
    CG2
      -> 0
      -- the only valid input was a=1, b=1
    CG4
      -> if b == 1 then 0 else 1
      -- the only possible input here is a=3 with b = 1 or 3
    CGOddPrimePower       (toInteger . unPrime -> p) k
      -> discreteLogarithmPP p k (getVal a) (getVal b)
    CGDoubleOddPrimePower (toInteger . unPrime -> p) k
      -> discreteLogarithmPP p k (getVal a `rem` p^k) (getVal b `rem` p^k)
      -- we have the isomorphism t -> t `rem` p^k from (Z/2p^kZ)* -> (Z/p^kZ)*
