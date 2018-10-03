-- |
-- Module:      Math.NumberTheory.Recurrencies.Bilinear
-- Description: Deprecated
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
-- #memory# __Note on memory leaks and memoization.__
-- Top-level definitions in this module are polymorphic, so the results of computations are not retained in memory.
-- Make them monomorphic to take advantages of memoization. Compare
--
-- >>> :set +s
-- >>> binomial !! 1000 !! 1000 :: Integer
-- 1
-- (0.01 secs, 1,385,512 bytes)
-- >>> binomial !! 1000 !! 1000 :: Integer
-- 1
-- (0.01 secs, 1,381,616 bytes)
--
-- against
--
-- >>> let binomial' = binomial :: [[Integer]]
-- >>> binomial' !! 1000 !! 1000 :: Integer
-- 1
-- (0.01 secs, 1,381,696 bytes)
-- >>> binomial' !! 1000 !! 1000 :: Integer
-- 1
-- (0.01 secs, 391,152 bytes)

module Math.NumberTheory.Recurrencies.Bilinear {-# DEPRECATED "Use `Math.NumberTheory.Recurrences.Bilinear` instead." #-}
    ( module Math.NumberTheory.Recurrences.Bilinear
    ) where

import Math.NumberTheory.Recurrences.Bilinear
