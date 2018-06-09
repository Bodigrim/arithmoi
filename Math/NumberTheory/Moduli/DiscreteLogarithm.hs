-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Nathan van Doorn
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--

module Math.NumberTheory.Moduli.DiscreteLogarithm where

import Math.NumberTheory.Moduli.Class

-- | Computes the discrete logarithm. @'discreteLogarithm' a b@ finds a value x,
-- such that @a '^%' x == b@, if such a value exists.
--
-- >>> discreteLogarithm (3 :: Mod 17) 14
-- Just 9
--
-- >>> discreteLogarithm (2 :: Mod 17) 14
-- Nothing
discreteLogarithm :: (KnownNat m, Integral b) => Mod m -> Mod m -> Maybe b
discreteLogarithm b g = go 0 1
  where
    m = fromInteger $ getMod b
    go k b' | b' == g = Just k
            | k  >= m = Nothing
            | otherwise =
              let
                k' = k + 1
                b'' = b * b'
              in k' `seq` b'' `seq` go k' b''
