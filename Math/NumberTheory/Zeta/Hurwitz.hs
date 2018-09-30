-- |
-- Module:      Math.NumberTheory.Zeta.Hurwitz
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Hurwitz zeta function.

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta.Hurwitz
  ( zetaHurwitz
  ) where

import Data.List                      (zipWith4)

import Math.NumberTheory.Recurrencies (bernoulli, factorial)
import Math.NumberTheory.Zeta.Utils   (skipOdds)

-- | Value of Hurwitz zeta function evaluated at @ζ(s, a)@ with
-- @forall t1 t2 . (Floating t1, Ord t1, Integral t2) => s ∈ t2, a ∈ t1@.
-- The algorithm used was based on the Euler-Maclaurin formula and was derived
-- from <http://fredrikj.net/thesis/thesis.pdf Fast and Rigorous Computation of Special Functions to High Precision>
-- by F. Johansson, chapter 4.8, formula 4.8.5.
-- The error for this formula is given in formula 4.8.9 as an indefinite
-- integral, and in formula 4.8.12 as a closed form formula.
-- It is the __user's responsibility__ to provide an appropriate precision for
-- the type chosen. For instance, when using @Double@s, it does not make sense
-- to provide a number @ε >= 1e-53@ as the desired precision. For @Float@s,
-- providing an @ε >= 1e-24@ also does not make sense.
zetaHurwitz :: forall a b . (Floating a, Ord a, Integral b) => a -> b -> a -> a
zetaHurwitz eps s a = s' + i + t
  where
    -- When given @1e-14@ as the @eps@ argument, this'll be
    -- @div (33 * (length . takeWhile (>= 1) . iterate (/ 10) . recip) 1e-14) 10 == div (33 * 14) 10@
    -- @div (33 * 14) 10 == 46.
    -- meaning @N,M@ in formula 4.8.5 will be @46@.
    -- Multiplying by 33 and dividing by 10 is because asking for @14@ digits
    -- of decimal precision equals asking for @(log 10 / log 2) * 14 ~ 3.3 * 14 ~ 46@
    -- bits of precision.
    digitsOfPrecision :: Int
    digitsOfPrecision =
       let magnitude = length . takeWhile (>= 1) . iterate (/ 10) . recip $ eps
       in  div (magnitude * 33) 10

    -- @a + n@
    aPlusN :: a
    aPlusN = a + fromIntegral digitsOfPrecision

    -- @(a + n)^s@
    powOfAPlusN :: a
    powOfAPlusN = aPlusN ^^ s

    --                   [      1      ]
    -- \sum_{k=0}^\(n-1) | ----------- |
    --                   [ (a + k) ^ s ]
    -- @S@ value in 4.8.5 formula.
    s' :: a
    s' = sum .
         take digitsOfPrecision .
         map (recip . (^^ s) . (a +) . fromInteger) $ [0..]

    -- (a + n) ^ (1 - s)            a + n
    -- ----------------- = ----------------------
    --       s - 1          (a + n) ^ s * (s - 1)
    -- @I@ value in 4.8.5 formula.
    i :: a
    i = aPlusN / (powOfAPlusN * ((fromIntegral s) - 1))

    --      1
    -- -----------
    -- (a + n) ^ s
    constant2 :: a
    constant2 = recip $ powOfAPlusN

    -- [(s)_(2*k - 1) | k <- [1 ..]]
    pochhammer :: [a]
    pochhammer = map fromIntegral $ skipOdds $ scanl1 (*) [s ..]

    -- [(a + n) ^ (2*k - 1) | k <- [1 ..]]
    powers :: [a]
    powers = iterate ((aPlusN * aPlusN) *) aPlusN

    -- [ B_2k     (s)_(2*k - 1)    |             ]
    -- | ----- ------------------- | k <- [1 ..] |
    -- [ (2k)! (a + n) ^ (2*k - 1) |             ]
    second :: a
    second = sum $
             take digitsOfPrecision $
             zipWith4
             (\bern evenFac poch denom -> (fromRational bern * poch) / (denom * fromInteger evenFac))
             (tail $ skipOdds bernoulli)
             (tail $ skipOdds factorial)
             pochhammer
             powers

    -- @T@ value in 4.8.5 formula.
    t = constant2 * (0.5 + second)