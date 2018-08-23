-- |
-- Module:      Math.NumberTheory.Moduli.Equations
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Modular equations
--

module Math.NumberTheory.Moduli.Equations
  ( solveLinear
  ) where

import GHC.Integer.GMP.Internals

import Math.NumberTheory.Moduli.Class

-- | Find all solutions of ax + b == 0 (mod m).
solveLinear :: KnownNat m => Mod m -> Mod m -> [Mod m]
solveLinear a b = map fromInteger $ solveLinear' (getMod a) (getVal a) (getVal b)

solveLinear' :: Integer -> Integer -> Integer -> [Integer]
solveLinear' m a b = case solveLinearCoprime m' (a `quot` d) (b `quot` d) of
  Nothing -> []
  Just x  -> map (\i -> x + m' * i) [0 .. d - 1]
  where
    d = m `gcd` a `gcd` b
    m' = m `quot` d

solveLinearCoprime :: Integer -> Integer -> Integer -> Maybe Integer
solveLinearCoprime m a b
  | m `gcd` a /= 1 = Nothing
  | otherwise = Just $ negate b * recipModInteger a m `mod` m
