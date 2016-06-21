-- |
-- Module:      Math.NumberTheory.Recurrencies.Bilinear
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--

module Math.NumberTheory.Recurrencies.Bilinear
  ( binomial
  , stirling1
  , stirling2
  , eulerian1
  , eulerian2
  , bernoulli
  ) where

import Data.List
import Data.Ratio

factorial :: [Integer]
factorial = 1 : zipWith (*) [1..] factorial

binomial :: [[Integer]]
binomial = [1] : map f binomial
  where
    f xs = 1 : zipWith (+) xs (tail xs ++ [0])

stirling1 :: [[Integer]]
stirling1 = [1] : zipWith f [0..] stirling1
  where
    f n xs = 0 : zipWith (\x1 x -> x1 + n * x) xs (tail xs ++ [0])

stirling2 :: [[Integer]]
stirling2 = [1] : map f stirling2
  where
    f xs = 0 : zipWith3 (\k x1 x -> x1 + k * x) [1..] xs (tail xs ++ [0])

eulerian1 :: [[Integer]]
eulerian1 = [] : zipWith f [1..] eulerian1
  where
    f n xs = 1 : zipWith3 (\k x1 x -> (n - k) * x1 + (k + 1) * x) [1..] xs (tail xs ++ [0])

eulerian2 :: [[Integer]]
eulerian2 = [] : zipWith f [1..] eulerian2
  where
    f n xs = 1 : zipWith3 (\k x1 x -> (2 * n - k - 1) * x1 + (k + 1) * x) [1..] xs (tail xs ++ [0])

bernoulli :: [Rational]
bernoulli = map f stirling2
  where
    f = sum . zipWith4 (\sgn denom fact stir -> sgn * fact * stir % denom) (cycle [1, -1]) [1..] factorial
