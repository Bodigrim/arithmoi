-- |
-- Module:      Math.NumberTheory.Zeta.Riemann
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Riemann zeta-function.

{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta.Riemann
  ( zetas
  , zetasEven
  , zetasOdd
  ) where

import Data.ExactPi
import Data.List.Infinite (Infinite(..), (...), (....))
import qualified Data.List.Infinite as Inf
import Data.Ratio                     ((%))

import Math.NumberTheory.Recurrences  (bernoulli)
import Math.NumberTheory.Zeta.Hurwitz (zetaHurwitz)
import Math.NumberTheory.Zeta.Utils   (skipEvens, skipOdds)

-- | Infinite sequence of exact values of Riemann zeta-function at even arguments, starting with @ζ(0)@.
-- Note that due to numerical errors conversion to 'Double' may return values below 1:
--
-- >>> approximateValue (zetasEven !! 25) :: Double
-- 0.9999999999999996
--
-- Use your favorite type for long-precision arithmetic. For instance, 'Data.Number.Fixed.Fixed' works fine:
--
-- >>> import Data.Number.Fixed
-- >>> approximateValue (zetasEven !! 25) :: Fixed Prec50
-- 1.00000000000000088817842111574532859293035196051773
--
zetasEven :: Infinite ExactPi
zetasEven = Inf.zipWith Exact ((0, 2)....) $ Inf.zipWith (*) (skipOdds bernoulli) cs
  where
    cs :: Infinite Rational
    cs = (- 1 % 2) :< Inf.zipWith (\i f -> i * (-4) / fromInteger (2 * f * (2 * f - 1))) cs (1...)

-- | Infinite sequence of approximate values of Riemann zeta-function
-- at odd arguments, starting with @ζ(1)@.
zetasOdd :: forall a. (Floating a, Ord a) => a -> Infinite a
zetasOdd eps = (1 / 0) :< Inf.tail (skipEvens $ zetaHurwitz eps 1)

-- | Infinite sequence of approximate (up to given precision)
-- values of Riemann zeta-function at integer arguments, starting with @ζ(0)@.
--
-- >>> take 5 (zetas 1e-14) :: [Double]
-- [-0.5,Infinity,1.6449340668482264,1.2020569031595942,1.0823232337111381]
--
-- Beware to force evaluation of @zetas !! 1@ if the type @a@ does not support infinite values
-- (for instance, 'Data.Number.Fixed.Fixed').
--
zetas :: (Floating a, Ord a) => a -> Infinite a
zetas eps = e :< o :< Inf.scanl1 f (Inf.interleave es os)
  where
    e :< es = Inf.map (getRationalLimit (\a b -> abs (a - b) < eps) . rationalApproximations) zetasEven
    o :< os = zetasOdd eps

    -- Cap-and-floor to improve numerical stability:
    -- 0 < zeta(n + 1) - 1 < (zeta(n) - 1) / 2
    f x y = 1 `max` (y `min` (1 + (x - 1) / 2))
