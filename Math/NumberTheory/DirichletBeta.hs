{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      Math.NumberTheory.DirichletBeta
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Dirichlet beta-function.

module Math.NumberTheory.DirichletBeta
  ( dirichletBetasEven
  , dirichletBetasOdd
  , euler
  ) where

import Data.ExactPi                   (ExactPi (..))
import Data.List                      (zipWith4)
import Data.Ratio                     (Ratio, (%))

import Math.NumberTheory.Recurrencies (factorial, stirling2)

dirichletBetasEven :: a
dirichletBetasEven = undefined

skipOdds :: [a] -> [a]
skipOdds (x : _ : xs) = x : skipOdds xs
skipOdds xs = xs

-- | Infinite sequence of exact values of Dirichlet beta-function at odd arguments, starting with @β(0)@.
--
-- > > approximateValue (dirichletBetasOdd !! 25) :: Double
-- > 0.9999999999999987
--
-- Using 'Data.Number.Fixed.Fixed':
--
-- > > approximateValue (dirichletBetasOdd !! 25) :: Fixed Prec50
-- > 0.99999999999999999999999960726927497384196726751694z
--
dirichletBetasOdd :: [ExactPi]
dirichletBetasOdd = zipWith Exact [1, 3 ..] $ zipWith4 (\sgn denom eul twos -> sgn * (eul / (twos * denom)))
                                                       (cycle [1, -1])
                                                       (skipOdds factorial)
                                                       (skipOdds euler)
                                                       (iterate (*4) 4)

-- | Infinite zero-based list of <https://en.wikipedia.org/wiki/Euler_number Euler numbers>.

-- >>> take 10 euler :: [Integer]
-- [1 % 1,0 % 1,(-1) % 1,0 % 1,5 % 1,0 % 1,(-61) % 1,0 % 1,1385 % 1,0 % 1]
euler :: forall a . Integral a => [Ratio a]
euler = map (f . tail) (tail stirling2)
  where
    f = sum . zipWith4 (\sgn fact a stir -> sgn * fact * stir * a) (cycle [1, -1])
                                                                   factorial
                                                                   as

    as :: Integral a => [Ratio a]
    as = zipWith3 (\sgn frac divByFour -> sgn * divByFour * frac) (cycle [1, 1, 1, 1, -1, -1, -1, -1])
                                                                  (dups (1 : iterate (/ 2) (1 % 2)))
                                                                  (cycle [1, 1, 1, 0])

    dups :: [a] -> [a]
    dups (n : ns) = n : n : dups ns
    dups l = l