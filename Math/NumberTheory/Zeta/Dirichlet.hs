-- |
-- Module:      Math.NumberTheory.Zeta.Dirichlet
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Dirichlet beta-function.

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta.Dirichlet
  ( betas
  , betasEven
  , betasOdd
  ) where

import Data.ExactPi                   (ExactPi (..), approximateValue)
import Data.List                      (zipWith4)
import Data.Ratio                     ((%))

import Math.NumberTheory.Recurrences  (euler, factorial)
import Math.NumberTheory.Zeta.Hurwitz (zetaHurwitz)
import Math.NumberTheory.Zeta.Utils   (intertwine, skipOdds)

-- | Infinite sequence of exact values of Dirichlet beta-function at odd arguments, starting with @β(1)@.
--
-- >>> approximateValue (betasOdd !! 25) :: Double
-- 0.9999999999999987
--
-- Using 'Data.Number.Fixed.Fixed':
--
-- >>> approximateValue (betasOdd !! 25) :: Fixed Prec50
-- 0.99999999999999999999999960726927497384196726751694
betasOdd :: [ExactPi]
betasOdd = zipWith Exact [1, 3 ..] $ zipWith4
                                     (\sgn denom eul twos -> sgn * (eul % (twos * denom)))
                                     (cycle [1, -1])
                                     (skipOdds factorial)
                                     (skipOdds euler)
                                     (iterate (4 *) 4)

-- | @betasOdd@, but with @forall a . Floating a => a@ instead of @ExactPi@s.
-- Used in @betas@.
betasOdd' :: Floating a => [a]
betasOdd' = map approximateValue betasOdd

-- | Infinite sequence of approximate values of the Dirichlet @β@ function at
-- positive even integer arguments, starting with @β(0)@.
betasEven :: forall a. (Floating a, Ord a) => a -> [a]
betasEven eps = (1 / 2) : hurwitz
  where
    hurwitz :: [a]
    hurwitz =
        zipWith3 (\quarter threeQuarters four -> 
            (quarter - threeQuarters) / four)
        (tail . skipOdds $ zetaHurwitz eps 0.25)
        (tail . skipOdds $ zetaHurwitz eps 0.75)
        (iterate (16 *) 16)

-- | Infinite sequence of approximate (up to given precision)
-- values of Dirichlet beta-function at integer arguments, starting with @β(0)@.
-- The algorithm previously used to compute @β@ for even arguments was derived
-- from <https://arxiv.org/pdf/0910.5004.pdf An Euler-type formula for β(2n) and closed-form expressions for a class of zeta series>
-- by F. M. S. Lima, formula (12), but is now based on the Hurwitz zeta function.
--
-- >>> take 5 (betas 1e-14) :: [Double]
-- [0.5,0.7853981633974483,0.9159655941772191,0.9689461462593693,0.988944551741105]
betas :: (Floating a, Ord a) => a -> [a]
betas eps = e : o : scanl1 f (intertwine es os)
  where
    e : es = betasEven eps
    o : os = betasOdd'

    -- Cap-and-floor to improve numerical stability:
    -- 1 > beta(n + 1) - 1 > (beta(n) - 1) / 2
    -- A similar method is used in @Math.NumberTheory.Zeta.Riemann.zetas@.
    f x y = 1 `min` (y `max` (1 + (x - 1) / 2))
