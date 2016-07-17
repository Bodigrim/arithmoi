-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.Utils
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Some utilities related to factorisation, defined here to avoid import cycles.
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Factorisation.Utils
    ( ppTotient
    , totientFromCanonical
    , carmichaelFromCanonical
    , moebiusFromCanonical
    , divisorsFromCanonical
    , tauFromCanonical
    , divisorSumFromCanonical
    , sigmaFromCanonical
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits
import Data.List

import Math.NumberTheory.Powers.Integer

{-# DEPRECATED totientFromCanonical, carmichaelFromCanonical, moebiusFromCanonical, divisorsFromCanonical, tauFromCanonical, divisorSumFromCanonical, sigmaFromCanonical "Use 'Math.NumberTheory.ArithmeticFunctions'" #-}

-- | Totient of a prime power.
ppTotient :: (Integer,Int) -> Integer
ppTotient (p,1) = p-1
ppTotient (p,k) = (p-1)*(integerPower p (k-1))  -- slightly faster than (^) usually

-- | Calculate the totient from the canonical factorisation.
totientFromCanonical :: [(Integer,Int)] -> Integer
totientFromCanonical = product . map ppTotient

-- | Calculate the Carmichael function from the factorisation.
--   Requires that the list of prime factors is strictly ascending.
carmichaelFromCanonical :: [(Integer,Int)] -> Integer
carmichaelFromCanonical = go2
  where
    go2 ((2,k):ps) = let acc = case k of
                                 1 -> 1
                                 2 -> 2
                                 _ -> 1 `shiftL` (k-2)
                     in go acc ps
    go2 ps = go 1 ps
    go !acc ((p,1):pps) = go (lcm acc (p-1)) pps
    go acc ((p,k):pps)  = go ((lcm acc (p-1))*integerPower p (k-1)) pps
    go acc []           = acc

-- | Calculate the Moebius function from the canonical factorisation.
moebiusFromCanonical :: [(a, Int)] -> Integer
moebiusFromCanonical = go 1
  where
  go acc []            = acc
  go acc ((_, 1) : xs) = go (negate acc) xs
  go acc ((_, 0) : xs) = go acc xs          -- Should not really happen
  go _   _             = 0                  -- Short circuit for powers > 1

-- | The set of divisors, efficiently calculated from the canonical factorisation.
divisorsFromCanonical :: [(Integer,Int)] -> Set Integer
divisorsFromCanonical = foldl' step (Set.singleton 1)
  where
    step st (p,k) = Set.unions (st:[Set.mapMonotonic (*pp) st | pp <- take k (iterate (*p) p) ])

-- | The number of divisors, efficiently calculated from the canonical factorisation.
tauFromCanonical :: [(a,Int)] -> Integer
tauFromCanonical pps = product [fromIntegral k + 1 | (_,k) <- pps]

-- | The sum of all divisors, efficiently calculated from the canonical factorisation.
divisorSumFromCanonical :: [(Integer,Int)] -> Integer
divisorSumFromCanonical = product . map ppDivSum

ppDivSum :: (Integer,Int) -> Integer
ppDivSum (p,1) = p+1
ppDivSum (p,k) = (p^(k+1)-1) `quot` (p-1)

-- | The sum of the powers (with fixed exponent) of all divisors,
--   efficiently calculated from the canonical factorisation.
sigmaFromCanonical :: Int -> [(Integer,Int)] -> Integer
sigmaFromCanonical k = product . map (ppDivPowerSum k)

ppDivPowerSum :: Int -> (Integer,Int) -> Integer
ppDivPowerSum k (p,m) = (p^(k*(m+1)) - 1) `quot` (p^k - 1)
