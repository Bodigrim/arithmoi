-- |
-- Module:      Math.NumberTheory.Recurrencies.Bilinear
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Bilinear recurrent sequences and Bernoulli numbers,
-- roughly covering Ch. 5-6 of /Concrete Mathematics/
-- by R. L. Graham, D. E. Knuth and O. Patashnik.
--
-- __Note on memory leaks and memoization.__
-- Top-level definitions in this module are polymorphic, so the results of computations are not retained in memory.
-- Make them monomorphic to take advantages of memoization. Compare
--
-- > > :set +s
-- > > binomial !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.10 secs, 153,437,432 bytes)
-- > > binomial !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.10 secs, 153,432,856 bytes)
--
-- against
--
-- > > let binomial' = binomial :: [[Integer]]
-- > > binomial' !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.10 secs, 152,401,808 bytes)
-- > > binomial' !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.01 secs, 1,551,416 bytes)

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

factorial :: (Num a, Enum a) => [a]
factorial = 1 : zipWith (*) [1..] factorial

-- | Infinite zero-based table of binomial coefficients: @binomial !! n !! k == n! \/ k! \/ (n - k)!@.
--
-- > > take 5 (map (take 5) binomial)
-- > [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
--
-- One could also consider 'Math.Combinat.Numbers.binomial' to compute stand-alone values.
binomial :: Num a => [[a]]
binomial = [1] : map f binomial
  where
    f xs = 1 : zipWith (+) xs (tail xs ++ [0])

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind Stirling numbers of the first kind>.
--
-- > > take 5 (map (take 5) stirling1)
-- > [[1],[0,1],[0,1,1],[0,2,3,1],[0,6,11,6,1]]
--
-- One could also consider 'Math.Combinat.Numbers.unsignedStirling1st' to compute stand-alone values.
stirling1 :: (Num a, Enum a) => [[a]]
stirling1 = [1] : zipWith f [0..] stirling1
  where
    f n xs = 0 : zipWith (\x1 x -> x1 + n * x) xs (tail xs ++ [0])

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind Stirling numbers of the second kind>.
--
-- > > take 5 (map (take 5) stirling2)
-- > [[1],[0,1],[0,1,1],[0,1,3,1],[0,1,7,6,1]]
--
-- One could also consider 'Math.Combinat.Numbers.stirling2nd' to compute stand-alone values.
stirling2 :: (Num a, Enum a) => [[a]]
stirling2 = [1] : map f stirling2
  where
    f xs = 0 : zipWith3 (\k x1 x -> x1 + k * x) [1..] xs (tail xs ++ [0])

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Eulerian_number Eulerian numbers of the first kind>.
--
-- > > take 5 (map (take 5) eulerian1)
-- > [[],[1],[1,1],[1,4,1],[1,11,11,1]]
--
eulerian1 :: (Num a, Enum a) => [[a]]
eulerian1 = [] : zipWith f [1..] eulerian1
  where
    f n xs = 1 : zipWith3 (\k x1 x -> (n - k) * x1 + (k + 1) * x) [1..] xs (tail xs ++ [0])

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Eulerian_number#Eulerian_numbers_of_the_second_kind Eulerian numbers of the second kind>.
--
-- > > take 5 (map (take 5) eulerian2)
-- > [[],[1],[1,2],[1,8,6],[1,22,58,24]]
--
eulerian2 :: (Num a, Enum a) => [[a]]
eulerian2 = [] : zipWith f [1..] eulerian2
  where
    f n xs = 1 : zipWith3 (\k x1 x -> (2 * n - k - 1) * x1 + (k + 1) * x) [1..] xs (tail xs ++ [0])

-- | Infinite zero-based sequence of <https://en.wikipedia.org/wiki/Bernoulli_number Bernoulli numbers>,
-- computed via <https://en.wikipedia.org/wiki/Bernoulli_number#Connection_with_Stirling_numbers_of_the_second_kind connection>
-- with 'stirling2'.
--
-- > > take 5 bernoulli
-- > [1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30]
--
-- One could also consider 'Math.Combinat.Numbers.bernoulli' to compute stand-alone values.
bernoulli :: Integral a => [Ratio a]
bernoulli = map f stirling2
  where
    f = sum . zipWith4 (\sgn denom fact stir -> sgn * fact * stir % denom) (cycle [1, -1]) [1..] factorial
