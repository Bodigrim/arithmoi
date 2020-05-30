-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Mertens
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Values of <https://en.wikipedia.org/wiki/Mertens_function Mertens function>.
--

{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.ArithmeticFunctions.Mertens
  ( mertens
  ) where

import qualified Data.Vector.Unboxed as U

import Math.NumberTheory.Roots
import Math.NumberTheory.ArithmeticFunctions.Moebius

-- | Compute individual values of Mertens function in O(n^(2/3)) time and space.
--
-- >>> map (mertens . (10 ^)) [0..9]
-- [1,-1,1,2,-23,-48,212,1037,1928,-222]
--
-- The implementation follows Theorem 3.1 from <https://arxiv.org/pdf/1610.08551.pdf Computations of the Mertens function and improved bounds on the Mertens conjecture> by G. Hurst, excluding segmentation of sieves.
mertens :: Word -> Int
mertens 0 = 0
mertens 1 = 1
mertens x = sumMultMoebius lookupMus (\n -> sfunc (x `quot` n)) [1 .. x `quot` u]
  where
    u = (integerSquareRoot x + 1) `max` (integerCubeRoot x ^ (2 :: Word) `quot` 2)

    sfunc :: Word -> Int
    sfunc y
      = 1
      - sum [ U.unsafeIndex mes (fromIntegral $ y `quot` n) |  n <- [y `quot` u + 1 .. kappa] ]
      + fromIntegral kappa * U.unsafeIndex mes (fromIntegral nu)
      - sumMultMoebius lookupMus (\n -> fromIntegral $ y `quot` n) [1 .. nu]
      where
        nu = integerSquareRoot y
        kappa = y `quot` (nu + 1)

    -- cacheSize ~ u
    cacheSize :: Word
    cacheSize = u `max` (x `quot` u) `max` integerSquareRoot x

    -- 1-based index
    mus :: U.Vector Moebius
    mus = runMoebiusOverBlock 1 cacheSize

    lookupMus :: Word -> Moebius
    lookupMus i = U.unsafeIndex mus (fromIntegral (i - 1))

    -- 0-based index
    mes :: U.Vector Int
    mes = U.scanl' go 0 mus
      where
        go acc = \case
          MoebiusN -> acc - 1
          MoebiusZ -> acc
          MoebiusP -> acc + 1

-- | Compute sum (map (\x -> runMoebius (mu x) * f x))
sumMultMoebius :: (Word -> Moebius) -> (Word -> Int) -> [Word] -> Int
sumMultMoebius mu f = foldl go 0
  where
    go acc i = case mu i of
      MoebiusN -> acc - f i
      MoebiusZ -> acc
      MoebiusP -> acc + f i
