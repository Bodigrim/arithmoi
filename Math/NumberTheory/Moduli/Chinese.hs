-- |
-- Module:      Math.NumberTheory.Moduli.Chinese
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Chinese remainder theorem
--

{-# LANGUAGE BangPatterns #-}

module Math.NumberTheory.Moduli.Chinese
  ( chineseRemainder
  , chineseRemainder2
  ) where

import Control.Monad (foldM)

import Math.NumberTheory.GCD (extendedGCD)
import Math.NumberTheory.Utils (recipMod)

-- | Given a list @[(r_1,m_1), ..., (r_n,m_n)]@ of @(residue,modulus)@
--   pairs, @chineseRemainder@ calculates the solution to the simultaneous
--   congruences
--
-- >
-- > r ≡ r_k (mod m_k)
-- >
--
--   if all moduli are positive and pairwise coprime. Otherwise
--   the result is @Nothing@ regardless of whether
--   a solution exists.
chineseRemainder :: [(Integer,Integer)] -> Maybe Integer
chineseRemainder remainders = foldM addRem 0 remainders
  where
    !modulus = product (map snd remainders)
    addRem acc (_,1) = Just acc
    addRem acc (r,m) = do
        let cf = modulus `quot` m
        inv <- recipMod cf m
        Just $! (acc + inv*cf*r) `mod` modulus

-- | @chineseRemainder2 (r_1,m_1) (r_2,m_2)@ calculates the solution of
--
-- >
-- > r ≡ r_k (mod m_k)
--
--   if @m_1@ and @m_2@ are coprime.
chineseRemainder2 :: (Integer,Integer) -> (Integer,Integer) -> Integer
chineseRemainder2 (r1, md1) (r2,md2)
    = case extendedGCD md1 md2 of
        (_,u,v) -> ((1 - u*md1)*r1 + (1 - v*md2)*r2) `mod` (md1*md2)
