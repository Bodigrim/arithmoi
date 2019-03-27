-- |
-- Module:      Math.NumberTheory.Zeta.Utils
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Shared utilities used by functions from @Math.NumberTheory.Zeta@.

module Math.NumberTheory.Zeta.Utils
  ( intertwine
  , skipEvens
  , skipOdds
  ) where

-- | Joins two lists element-by-element together into one, starting with the
-- first one provided as argument.
--
-- >>> take 10 $ intertwine [0, 2 ..] [1, 3 ..]
-- [0,1,2,3,4,5,6,7,8,9]
intertwine :: [a] -> [a] -> [a]
intertwine [] ys = ys
intertwine (x : xs) ys = x : intertwine ys xs

-- | Skips every odd-indexed element from an infinite list.
-- Do NOT use with finite lists.
--
-- >>> take 10 (skipOdds [0, 1 ..])
-- [0,2,4,6,8,10,12,14,16,18]
skipOdds :: [a] -> [a]
skipOdds (x : _ : xs) = x : skipOdds xs
skipOdds xs = xs

-- | Skips every even-indexed element from an infinite list.
-- Do NOT use with finite lists.
--
-- >>> take 10 (skipEvens [0, 1 ..])
-- [1,3,5,7,9,11,13,15,17,19]
skipEvens :: [a] -> [a]
skipEvens = skipOdds . tail
