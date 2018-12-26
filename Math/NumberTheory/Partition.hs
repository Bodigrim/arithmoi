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

import qualified Math.NumberTheory.Recurrences as R
import Numeric.Rounded (Rounding (..), Precision (..))

partition :: forall a b . (Integral a, Floating b) => a -> b
partition num | num < 128 = R.partition !! (fromIntegral num)
              | otherwise = sum . map (_T num) $ [1 .. _N]
  where
    invLog2 = (1.44269504088896340735992468 + 1e-12)
    hrrA = (1.1143183348516376904 + 1e-12)  -- 44*pi^2/(225*sqrt(3))
    hrrB = (0.0592384391754448833 + 1e-12)  -- pi*sqrt(2)/75
    hrrC = (2.5650996603237281911 + 1e-12)  -- pi*sqrt(2/3)
    hrrD = (1.2424533248940001551 + 1e-12)  -- log(2) + log(3)/2

    remBound :: Double -> Double -> Double
    remBound num terms =
        hrrA / sqrt terms +
        hrrB * sqrt (terms / (num - 1)) * sinh (hrrC * sqrt num / terms)

    logSinh :: Double -> Double
    logSinh x =
        if (x > 4) then x
        else log x + x * x * (1/6.0)

    remBoundLog2 :: Double -> Double -> Double
    remBoundLog2 n nN =
        let t1 = log hrrA - 0.5 * log nN
            t2 = log hrrB + 0.5 * (log nN - log (n-1)) + logSinh (hrrC * sqrt n / nN)
        in (1 + max t1 t2) * invLog2

    neededTerms :: a -> a
    neededTerms n =
        let n' = fromIntegral n
            go1 :: a -> a
            go1 !nN | remBoundLog2 n' (fromIntegral nN) > 10 = go1 (nN + 1)
                    | otherwise                               = nN

            go2 :: a -> a
            go2 !nN | remBound n' (fromIntegral nN) >
                      (if n > 1500 then 0.25 else 1.0) = go2 (nN + 1)
                    | otherwise                        = nN

        in go2 . go1 $ 1

    _N :: a
    _N = neededTerms $ num

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