-- |
-- Module:      Math.NumberTheory.Zeta.Dirichlet
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Dirichlet beta-function.

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta.Dirichlet
  ( betas
  , betasEven
  , betasOdd
  ) where

import Data.ExactPi                    (ExactPi (..), approximateValue)
import Data.List                       (zipWith4)
import Data.Ratio                      ((%))

import Math.NumberTheory.Recurrences   (euler, eulerPolyAt1, factorial)
import Math.NumberTheory.Zeta.Riemann  (zetasOdd)
import Math.NumberTheory.Zeta.Utils    (intertwine, skipEvens, skipOdds,
                                        suminf)

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
-- Used in @betasEven@.
betasOdd' :: Floating a => [a]
betasOdd' = map approximateValue betasOdd

-- | Infinite sequence of approximate values of the Dirichlet @β@ function at
-- positive even integer arguments, starting with @β(0)@.
betasEven :: forall a. (Floating a, Ord a) => a -> [a]
betasEven eps = (1 / 2) : bets
  where
    bets :: [a]
    bets = zipWith3 (\r1 r2 r3 -> (r1 + (negate r2) + r3)) rhs1 rhs2 rhs3

    -- [1!, 3!, 5!..]
    factorial1AsInteger :: [Integer]
    factorial1AsInteger = skipEvens factorial

    -- [1!, 3!, 5!..]
    factorial1 :: [a]
    factorial1 = map fromInteger factorial1AsInteger

    -- [2^1 * 1!, 2^3 * 3!, 2^5 * 5!, 2^7 * 7! ..]
    denoms :: [a]
    denoms = zipWith
             (\pow fac -> fromInteger $ pow * fac)
             factorial1AsInteger
             (iterate (4 *) 2)

    -- First term of the right hand side of (12).
    rhs1 = zipWith
           (\sgn piFrac -> sgn * piFrac * log 2)
           (cycle [1, -1])
           (zipWith (\p f -> p / f) (iterate ((pi * pi) *) pi) denoms)

    -- [1 - (1 / (2^2)), 1 - (1 / (2^4)), 1 - (1 / (2^6)), ..]
    second :: [a]
    second = map (1 -) $ (iterate (/ 4) (1/4))

    -- [- (1 - (1 / (2^2))) * zeta(3), (1 - (1 / (2^4))) * zeta(5), - (1 - (1 / (2^6))) * zeta(7), ..]
    zets :: [a]
    zets = zipWith3
           (\sgn twosFrac z -> sgn * twosFrac * z)
           (cycle [-1, 1])
           second
           (tail $ zetasOdd eps)

    -- [pi / (2^1 * 1!), pi^3 / (2^3 * 3!), pi^5 / (2^5 * 5!), ..]
    pisAndFacs :: [a]
    pisAndFacs = zipWith3
                 (\p pow fac -> p / (pow * fac))
                 (iterate ((pi * pi) *) pi)
                 (iterate (4 *) 2)
                 factorial1

    -- [[], [pisAndFacs !! 0], [pisAndFacs !! 1, pisAndFacs !! 0], [pisAndFacs !! 2, pisAndFacs !! 1, pisAndFacs !! 0]...]
    pisAndFacs' :: [[a]]
    pisAndFacs' = scanl (flip (:)) [] pisAndFacs

    -- Second summand of RHS in (12) for k = [1 ..]
    rhs2 :: [a]
    rhs2 = zipWith (*) (cycle [-1, 1]) $ map (sum . zipWith (*) zets) pisAndFacs'

    -- [pi^3 / (2^4), pi^5 / (2^6), pi^7 / (2^8) ..]
    -- Second factor of third addend in RHS of (12).
    pis :: [a]
    pis = zipWith
          (\p f -> p / f)
          (iterate ((pi * pi) *) (pi ^^ (3 :: Integer)))
          (iterate (4 *) 16)

    -- [[3!, 5!, 7! ..], [5!, 7! ..] ..]
    oddFacs :: [[a]]
    oddFacs = iterate tail (tail factorial1)

    -- [1, 4, 16 ..]
    fours :: [a]
    fours = iterate (4 *) 1

    -- [[3! * 2^0, 5! * 2^2, 7! * 2^4 ..], [5! * 2^0, 7! * 2^2, 9! * 2^4 ..] ..]
    infSumDenoms :: [[a]]
    infSumDenoms = map (zipWith (*) fours) oddFacs

    -- [pi^0, pi^2, pi^4, pi^6 ..]
    pis2 :: [a]
    pis2 = iterate ((pi * pi) *) 1

    -- [pi^0 * E_1(1), - pi^2 * E_3(1), pi^4 * E_5(1) ..]
    infSumNum :: [a]
    infSumNum = zipWith3
                (\sgn p eulerP -> sgn * p * eulerP)
                (cycle [1, -1])
                pis2
                (map fromRational . skipEvens $ eulerPolyAt1)

    -- [     [ pi^0 * E_1(1)  (-1) * pi^2 * E_3(1)   ]      [ (-1) * pi^2 * E_3(1)  pi^4 * E_5(1)    ]      [ pi^4 * E_5(1)  (-1) * pi^6 * E_7(1)    ]  ]
    -- | sum | -------------, -------------------- ..|, sum | --------------------, ------------- .. |, sum | -------------, -------------------- .. |..|
    -- [     [       3!                 5!           ]      [          5!                 7!         ]      [       7!                9!             ]  ]
    infSum :: [a]
    infSum = map (suminf eps . zipWith (/) infSumNum) infSumDenoms

    -- Third summand of the right hand side of (12).
    rhs3 :: [a]
    rhs3 = zipWith3
           (\sgn p inf -> sgn * p * inf)
           (cycle [-1, 1])
           pis
           infSum

-- | Infinite sequence of approximate (up to given precision)
-- values of Dirichlet beta-function at integer arguments, starting with @β(0)@.
-- The algorithm used to compute @β@ for even arguments was derived from
-- <https://arxiv.org/pdf/0910.5004.pdf An Euler-type formula for β(2n) and closed-form expressions for a class of zeta series>
-- by F. M. S. Lima, formula (12).
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
