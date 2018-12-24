-- |
-- Module:      Math.NumberTheory.Partition
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- <https://en.wikipedia.org/wiki/Partition_(number_theory)#Partition_function partition function>
-- at isolated values.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Partition
  ( partition
  ) where

import Math.NumberTheory.Powers.Squares (integerSquareRoot)

partition :: forall a b . (Integral a, Floating b) => a -> b
partition num = sum . map (_T num) $ [1 .. _N]
  where
    _N :: a
    _N = integerSquareRoot num

    _T :: a -> a -> b
    _T n k = sqrt (3 / fromIntegral k) * 4 / (24 * fromIntegral n - 1) * _A k (fromIntegral n) * _U (_C n / fromIntegral k)

    _U :: b -> b
    _U x = cosh x - sinh x / x

    _C :: a -> b
    _C n = pi / 6 * sqrt (24 * fromIntegral n - 1)

    _A :: a -> a -> b
    _A !k !n | k <= 1 = fromIntegral k
             | k == 2 = fromIntegral $ (-1) ^ n
             | otherwise = go 0 2 (n `mod` k) 0
               where
                 -- Algorith 5.3.1 to calculate @A_k@.
                 go :: b -> a -> a -> a -> b
                 go !s !r !m !l | l >= 2 * k = sqrt (fromIntegral k / 3) * s
                                | otherwise =
                                    let s' | m == 0 = s + fromIntegral ((-1)^l) * cos (pi * (6 * fromIntegral l + 1) / (6 * fromIntegral k))
                                           | otherwise = s
                                        -- Lines 7 and 8 from the algorithm.
                                        m' = m + r
                                        m'' | m' >= k = m' - k
                                            | otherwise = m'
                                        -- Lines 9 and 10 from the algorithm.
                                        r' = r + 3
                                        r'' | r' >= k = r' - k
                                            | otherwise = r'
                                    in go s' m'' r'' (l + 1)