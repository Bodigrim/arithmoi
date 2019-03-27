-- |
-- Module:      Math.NumberTheory.Moduli.Chinese
-- Copyright:   (c) 2011 Daniel Fischer, 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Chinese remainder theorem
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

#if __GLASGOW_HASKELL__ > 805
{-# LANGUAGE NoStarIsType #-}
#endif

module Math.NumberTheory.Moduli.Chinese
  ( -- * Safe interface
    chinese
  , chineseCoprime
  , chineseSomeMod
  , chineseCoprimeSomeMod

  , -- * Unsafe interface
    chineseRemainder
  , chineseRemainder2
  ) where

import Prelude hiding (mod, quot, gcd, lcm)

import Control.Monad (foldM)
import Data.Foldable
import Data.Ratio
import GHC.TypeNats.Compat
import Numeric.Natural

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Euclidean
import Math.NumberTheory.Euclidean.Coprimes
import Math.NumberTheory.Utils (recipMod, splitOff)

-- | 'chineseCoprime' @(n1, m1)@ @(n2, m2)@ returns @n@ such that
-- @n \`mod\` m1 == n1@ and @n \`mod\` m2 == n2@.
-- Moduli @m1@ and @m2@ must be coprime, otherwise 'Nothing' is returned.
--
-- This function is slightly faster than 'chinese', but more restricted.
--
-- >>> chineseCoprime (1, 2) (2, 3)
-- Just 5
-- >>> chineseCoprime (3, 4) (5, 6)
-- Nothing -- moduli must be coprime
chineseCoprime :: Euclidean a => (a, a) -> (a, a) -> Maybe a
chineseCoprime (n1, m1) (n2, m2) = case d of
  1 -> Just $ ((1 - u * m1) * n1 + (1 - v * m2) * n2) `mod` (m1 * m2)
  _ -> Nothing
  where
    (d, u, v) = extendedGCD m1 m2

{-# SPECIALISE chineseCoprime :: (Int, Int) -> (Int, Int) -> Maybe Int #-}
{-# SPECIALISE chineseCoprime :: (Word, Word) -> (Word, Word) -> Maybe Word #-}
{-# SPECIALISE chineseCoprime :: (Integer, Integer) -> (Integer, Integer) -> Maybe Integer #-}
{-# SPECIALISE chineseCoprime :: (Natural, Natural) -> (Natural, Natural) -> Maybe Natural #-}

-- | 'chinese' @(n1, m1)@ @(n2, m2)@ returns @n@ such that
-- @n \`mod\` m1 == n1@ and @n \`mod\` m2 == n2@, if exists.
-- Moduli @m1@ and @m2@ are allowed to have common factors.
--
-- >>> chinese (1, 2) (2, 3)
-- Just 5
-- >>> chinese (3, 4) (5, 6)
-- Just 11
-- >>> chinese (3, 4) (2, 6)
-- Nothing
chinese :: forall a. Euclidean a => (a, a) -> (a, a) -> Maybe a
chinese (n1, m1) (n2, m2)
  | n1 `mod` g == n2 `mod` g
  = chineseCoprime (n1 `mod` m1', m1') (n2 `mod` m2', m2')
  | otherwise
  = Nothing
  where
    g :: a
    g = gcd m1 m2

    ms :: [(a, Word)]
    ms = unCoprimes $ splitIntoCoprimes [(m1, 1), (m2 `quot` g, 1)]

    m1', m2' :: a
    (m1', m2') = foldl' go (1, 1) $ map fst ms

    go :: (a, a) -> a -> (a, a)
    go (t1, t2) p
      | k1 <= k2
      = (t1, t2 * p ^ k2)
      | otherwise
      = (t1 * p ^ k1, t2)
      where
        (k1, _) = splitOff p m1
        (k2, _) = splitOff p m2

{-# SPECIALISE chinese :: (Int, Int) -> (Int, Int) -> Maybe Int #-}
{-# SPECIALISE chinese :: (Word, Word) -> (Word, Word) -> Maybe Word #-}
{-# SPECIALISE chinese :: (Integer, Integer) -> (Integer, Integer) -> Maybe Integer #-}
{-# SPECIALISE chinese :: (Natural, Natural) -> (Natural, Natural) -> Maybe Natural #-}

isCompatible :: KnownNat m => Mod m -> Rational -> Bool
isCompatible n r = case invertMod (fromInteger (denominator r)) of
  Nothing -> False
  Just r' -> r' * fromInteger (numerator r) == n

chineseWrap
  :: (Integer -> Integer -> Integer)
  -> ((Integer, Integer) -> (Integer, Integer) -> Maybe Integer)
  -> SomeMod
  -> SomeMod
  -> Maybe SomeMod
chineseWrap f g (SomeMod n1) (SomeMod n2)
  = fmap (`modulo` fromInteger (f m1 m2)) (g (getVal n1, m1) (getVal n2, m2))
  where
    m1 = getMod n1
    m2 = getMod n2
chineseWrap _ _ (SomeMod n) (InfMod r)
  | isCompatible n r = Just $ InfMod r
  | otherwise        = Nothing
chineseWrap _ _ (InfMod r) (SomeMod n)
  | isCompatible n r = Just $ InfMod r
  | otherwise        = Nothing
chineseWrap _ _ (InfMod r1) (InfMod r2)
  | r1 == r2  = Just $ InfMod r1
  | otherwise = Nothing

-- | Same as 'chineseCoprime', but operates on residues.
--
-- >>> :set -XDataKinds
-- >>> import Math.NumberTheory.Moduli.Class
-- >>> (1 `modulo` 2) `chineseCoprimeSomeMod` (2 `modulo` 3)
-- Just (5 `modulo` 6)
-- >>> (3 `modulo` 4) `chineseCoprimeSomeMod` (5 `modulo` 6)
-- Nothing
chineseCoprimeSomeMod :: SomeMod -> SomeMod -> Maybe SomeMod
chineseCoprimeSomeMod = chineseWrap (*) chineseCoprime

-- | Same as 'chinese', but operates on residues.
--
-- >>> :set -XDataKinds
-- >>> import Math.NumberTheory.Moduli.Class
-- >>> (1 `modulo` 2) `chineseSomeMod` (2 `modulo` 3)
-- Just (5 `modulo` 6)
-- >>> (3 `modulo` 4) `chineseSomeMod` (5 `modulo` 6)
-- Just (11 `modulo` 12)
-- >>> (3 `modulo` 4) `chineseSomeMod` (2 `modulo` 6)
-- Nothing
chineseSomeMod :: SomeMod -> SomeMod -> Maybe SomeMod
chineseSomeMod = chineseWrap lcm chinese

-------------------------------------------------------------------------------
-- Unsafe interface

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
chineseRemainder :: [(Integer, Integer)] -> Maybe Integer
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
chineseRemainder2 :: (Integer, Integer) -> (Integer, Integer) -> Integer
chineseRemainder2 (n1, m1) (n2, m2) = ((1 - u * m1) * n1 + (1 - v * m2) * n2) `mod` (m1 * m2)
  where
    (_, u, v) = extendedGCD m1 m2
