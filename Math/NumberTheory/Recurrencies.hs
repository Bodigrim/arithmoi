
{-# LANGUAGE RankNTypes #-}

module Math.NumberTheory.Recurrencies
  where

import Numeric.Natural (Natural)

pentIndexes :: Integral a => [a]
pentIndexes = 0 : pentagonalIndexes [1..]

pentagonalIndexes :: Integral a => [a] -> [a]
pentagonalIndexes (n : ns) = n : (-n) : pentagonalIndexes ns
pentagonalIndexes _ = []

pent :: Integral a => a -> a
pent n = div (3 * (n * n) - n) 2

pents :: Integral a => [a]
pents = map pent pentIndexes

pentagonalSigns :: Integral a => Bool -> [a] -> [a]
pentagonalSigns b (x : y : ns) | b = x : y : pentagonalSigns b' ns
                               | otherwise = (-x) : (-y) : pentagonalSigns b' ns
  where b' = not b
pentagonalSigns b (x : _) | b = [x]
                          | otherwise = [-x]
pentagonalSigns _ _ = []

parts :: Integral a => a -> a
parts n = (sum .
           pentagonalSigns True .
           map (\m -> partition !! fromIntegral (n - m)) .
           takeWhile (\m -> n - m >= 0) .
           tail) pents

partition :: forall a . Integral a => [a]
partition = 1 : map parts [1..]
{-# SPECIALIZE partition :: [Int]     #-}
{-# SPECIALIZE partition :: [Word]    #-}
{-# SPECIALIZE partition :: [Integer] #-}
{-# SPECIALIZE partition :: [Natural] #-}