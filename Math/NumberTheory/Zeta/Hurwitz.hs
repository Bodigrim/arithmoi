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
import Math.NumberTheory.Zeta.Utils   (skipEvens, suminf)

-- | Hurwitz zeta function for integer values.
zeta :: forall a b . (Floating a, Ord a, Integral b) => a -> b -> a -> [a]
zeta eps s a = map (\second ->
                       firstSum + constant1 + (negate constant2) + second)
               seconds
  where
    digitsOfPrecision :: Int
    digitsOfPrecision = length . takeWhile (>= 1) . iterate (/ 10) . recip $ eps

    aPlusN :: a
    aPlusN = a + fromIntegral digitsOfPrecision

    powOfAPlusN :: a
    powOfAPlusN = aPlusN ^^ s

    constant1 :: a
    constant1 = aPlusN / (powOfAPlusN * ((fromIntegral s) - 1))

    constant2 :: a
    constant2 = recip $ 2 * powOfAPlusN

    firstSum :: a
    firstSum = suminf eps $ map (recip . (^^ s) . (a +) . fromInteger) [0..]

    factorial' :: [a]
    factorial' = map fromInteger factorial

    evenBernoulliDivByFac :: [a]
    evenBernoulliDivByFac = zipWith
                            (\bern fac -> fromRational $ bern / fromInteger fac)
                            (skipEvens bernoulli)
                            (skipEvens factorial)

    evenBernoulliFacFracs :: [[a]]
    evenBernoulliFacFracs = iterate tail evenBernoulliDivByFac

    secondSumNum :: [a]
    secondSumNum = skipEvens $ drop (fromIntegral s) factorial'

    secondSumNums :: [[a]]
    secondSumNums = iterate tail secondSumNum

    secondSumDenom :: [a]
    secondSumDenom = map ((factorial' !! (fromEnum s - 1) *))
                         (iterate ((aPlusN * aPlusN) *) (aPlusN * powOfAPlusN))

    secondSumDenoms :: [[a]]
    secondSumDenoms = iterate tail secondSumDenom

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

