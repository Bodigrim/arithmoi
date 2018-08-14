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

import Math.NumberTheory.Recurrencies (bernoulli, factorial)
import Math.NumberTheory.Zeta.Utils   (skipOdds, suminf)

-- | Value of Hurwitz zeta function evaluated at @ζ(s, a)@ with
-- @forall t1 t2 . (Floating t1, Ord t1, Integral t2) => s ∈ t2, a ∈ t1@.
-- The algorithm used was based on the Euler-Maclaurin formula and was derived
-- from the <https://dlmf.nist.gov/25.11#iii Digital Library of Mathematical Functions>
-- by the <https://www.nist.gov/ National Institute of Standards and Technology (NIST)>,
-- formula 25.11.5.
zeta :: forall a b . (Floating a, Ord a, Integral b) => a -> b -> a -> [a]
zeta eps s a = map (\second ->
                       firstSum + constant1 + (negate constant2) + second)
               seconds
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

    -- (a + n) ^ (1 - s)            a + n
    -- ----------------- = ----------------------
    --       s - 1          (a + n) ^ s * (s - 1)
    constant1 :: a
    constant1 = aPlusN / (powOfAPlusN * ((fromIntegral s) - 1))

    --        1
    -- ---------------
    -- 2 * (a + n) ^ s
    constant2 :: a
    constant2 = recip $ 2 * powOfAPlusN

    --               [      1      ]
    -- \sum_{k=0}^\n | ----------- |
    --               [ (a + k) ^ s ]
    firstSum :: a
    firstSum = suminf eps $ map (recip . (^^ s) . (a +) . fromInteger) [0..]

    -- [0!, 1!, 2!, 3! ..]
    factorial' :: [a]
    factorial' = map fromInteger factorial

    -- [ B_2k                ]
    -- | ----- | k <- [1 ..] |
    -- [ (2k)!               ]
    evenBernoulliDivByFac :: [a]
    evenBernoulliDivByFac = zipWith
                            (\bern fac -> fromRational $ bern / fromInteger fac)
                            (tail $ skipOdds bernoulli)
                            (tail $ skipOdds factorial)

    -- [ [ B_2k                ]  [ B_2k                ]  [ B_2k                ]   ]
    -- | | ----- | k <- [1 ..] |, | ----- | k <- [2 ..] |, | ----- | k <- [3 ..] | ..|
    -- [ [ (2k)!               ]  [ (2k)!               ]  [ (2k)!               ]   ]
    evenBernoulliFacFracs :: [[a]]
    evenBernoulliFacFracs = iterate tail evenBernoulliDivByFac

    -- [(s + 2*k - 2)! | k <- [1 ..]]
    secondSumNum :: [a]
    secondSumNum = skipOdds $ drop (fromIntegral s) factorial'

    -- [[(s + 2*k - 2)! | k <- [1 ..]], [(s + 2*k - 2)! | k <- [2 ..]], [(s + 2*k - 2)! | k <- [3 ..]] ..]
    secondSumNums :: [[a]]
    secondSumNums = iterate tail secondSumNum

    -- [(s - 1)! * (a + n) ^ s * (a + n) ^ (2*k - 1) | k <- [1 ..]]
    secondSumDenom :: [a]
    secondSumDenom = map ((factorial' !! (fromEnum s - 1) *))
                         (iterate ((aPlusN * aPlusN) *) (aPlusN * powOfAPlusN))

    -- [[(s - 1)! * (a + n) ^ s * (a + n) ^ (2*k - 1) | k <- [1 ..]], [(s - 1)! * (a + n) ^ s * (a + n) ^ (2*k - 1) | k <- [2 ..]] ..]
    secondSumDenoms :: [[a]]
    secondSumDenoms = iterate tail secondSumDenom

    -- [[ B_2k           (s + 2*k - 2)!           |             ]  [ B_2k           (s + 2*k - 2)!           |             ]   ]
    -- || ----- --------------------------------- | k <- [1 ..] |, | ----- --------------------------------- | k <- [2 ..] | ..|
    -- [[ (2k)! (s - 1)! * (a + n) ^ (s + 2*k -1) |             ]  [ (2k)! (s - 1)! * (a + n) ^ (s + 2*k -1) |             ]   ]
    seconds :: [a]
    seconds = zipWith3
              (\l1 l2 l3 ->
                  suminf eps $ zipWith3
                               (\bernFac num denom -> bernFac * (num / denom))
                               l1
                               l2
                               l3)
              evenBernoulliFacFracs
              secondSumNums
              secondSumDenoms

