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
  ( zeta
  ) where

import Data.List                      (zipWith4)

import Math.NumberTheory.Recurrencies (bernoulli, factorial)
import Math.NumberTheory.Zeta.Utils   (skipOdds, suminf)

-- | Value of Hurwitz zeta function evaluated at @ζ(s, a)@ with
-- @forall t1 t2 . (Floating t1, Ord t1, Integral t2) => s ∈ t2, a ∈ t1@.
-- The algorithm used was based on the Euler-Maclaurin formula and was derived
-- from the <https://dlmf.nist.gov/25.11#iii Digital Library of Mathematical Functions>
-- by the <https://www.nist.gov/ National Institute of Standards and Technology (NIST)>,
-- formula 25.11.5.
zeta :: forall a b . (Floating a, Ord a, Integral b) => a -> b -> a -> a
zeta eps s a = s' + i + t
  where
    -- When given @1e-14@ as the @eps@ argument, this'll be
    -- (length . takeWhile (>= 1) . iterate (/ 10) . recip) 1e-14 == 15@,
    -- meaning @n@ in formula 25.11.5 will be @15@.
    digitsOfPrecision :: Int
    digitsOfPrecision = length . takeWhile (>= 1) . iterate (/ 10) . recip $ eps

    -- @a + n@
    aPlusN :: a
    aPlusN = a + fromIntegral digitsOfPrecision

    -- @(a + n)^s@
    powOfAPlusN :: a
    powOfAPlusN = aPlusN ^^ s

    --                   [      1      ]
    -- \sum_{k=0}^\(n-1) | ----------- |
    --                   [ (a + k) ^ s ]
    s' :: a
    s' = sum .
         take digitsOfPrecision .
         map (recip . (^^ s) . (a +) . fromInteger) $ [0..]

    -- (a + n) ^ (1 - s)            a + n
    -- ----------------- = ----------------------
    --       s - 1          (a + n) ^ s * (s - 1)
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
    -- [ (2k)! (a + n) ^ (2*k -1)  |             ]
    second :: a
    second = suminf eps $ zipWith4
                           (\bern evenFac poch denom -> (fromRational bern * poch) / (denom * fromInteger evenFac))
                           (tail $ skipOdds bernoulli)
                           (tail $ skipOdds factorial)
                           pochhammer
                           powers

    t = constant2 * (0.5 + second)