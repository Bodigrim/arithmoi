-- |
-- Module:      Math.NumberTheory.Zeta.ZetaUtils
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Shared utilities used by functions from @Math.NumberTheory.Zeta@.

module Math.NumberTheory.Zeta.ZetaUtils
  ( skipEvens
  , skipOdds
  , suminf
  ) where

-- | Skips every odd-indexed element from an infinite list.
-- Do NOT use with finite lists.
--
-- >>> take 10 (skipOdds [0, 1 ..])
-- [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]
skipOdds :: [a] -> [a]
skipOdds (x : _ : xs) = x : skipOdds xs
skipOdds xs = xs

-- | Skips every even-indexed element from an infinite list.
-- Do NOT use with finite lists.
--
-- >>> take 10 (skipEvens [0, 1 ..])
-- [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
skipEvens :: [a] -> [a]
skipEvens = skipOdds . tail

-- | Sums every element of an infinite list up to a certain precision.
-- I.e. once an element falls below the given threshold it stops traversing
-- the list.
--
-- >>> suminf 1e-14 (iterate (/ 10) 1)
-- 1.1111111111111112
suminf :: (Floating a, Ord a) => a -> [a] -> a
suminf eps = sum . takeWhile ((>= eps / 111) . abs)
