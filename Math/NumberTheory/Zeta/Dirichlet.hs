-- |
-- Module:      Math.NumberTheory.Zeta.Dirichlet
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Dirichlet beta-function.

{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta.Dirichlet
  ( betas
  , betasEven
  , betasOdd
  ) where

import Data.ExactPi
import Data.List.Infinite (Infinite(..), (....))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.Infinite as Inf
import Data.Ratio                     ((%))

import Math.NumberTheory.Recurrences  (euler, factorial)
import Math.NumberTheory.Zeta.Hurwitz (zetaHurwitz)
import Math.NumberTheory.Zeta.Utils   (skipOdds)

-- | Infinite sequence of exact values of Dirichlet beta-function at odd arguments, starting with @β(1)@.
--
-- >>> import Data.ExactPi
-- >>> approximateValue (betasOdd !! 25) :: Double
-- 0.9999999999999987
-- >>> import Data.Number.Fixed
-- >>> approximateValue (betasOdd !! 25) :: Fixed Prec50
-- 0.99999999999999999999999960726927497384196726751694
betasOdd :: Infinite ExactPi
betasOdd = Inf.zipWith Exact ((1, 3)....) $ Inf.zipWith4
                                     (\sgn denom eul twos -> sgn * (eul % (twos * denom)))
                                     (Inf.cycle (1 :| [-1]))
                                     (skipOdds factorial)
                                     (skipOdds euler)
                                     (Inf.iterate (4 *) 4)

-- | Infinite sequence of approximate values of the Dirichlet @β@ function at
-- positive even integer arguments, starting with @β(0)@.
betasEven :: forall a. (Floating a, Ord a) => a -> Infinite a
betasEven eps = (1 / 2) :< hurwitz
  where
    hurwitz :: Infinite a
    hurwitz =
        Inf.zipWith3 (\quarter threeQuarters four ->
            (quarter - threeQuarters) / four)
        (Inf.tail . skipOdds $ zetaHurwitz eps 0.25)
        (Inf.tail . skipOdds $ zetaHurwitz eps 0.75)
        (Inf.iterate (16 *) 16)

-- | Infinite sequence of approximate (up to given precision)
-- values of Dirichlet beta-function at integer arguments, starting with @β(0)@.
--
-- >>> take 5 (betas 1e-14) :: [Double]
-- [0.5,0.7853981633974483,0.9159655941772189,0.9689461462593694,0.9889445517411051]
betas :: (Floating a, Ord a) => a -> Infinite a
betas eps = e :< o :< Inf.scanl1 f (Inf.interleave es os)
  where
    e :< es = betasEven eps
    o :< os = Inf.map (getRationalLimit (\a b -> abs (a - b) < eps) . rationalApproximations) betasOdd

    -- Cap-and-floor to improve numerical stability:
    -- 1 > beta(n + 1) - 1 > (beta(n) - 1) / 2
    -- A similar method is used in @Math.NumberTheory.Zeta.Riemann.zetas@.
    f x y = 1 `min` (y `max` (1 + (x - 1) / 2))
