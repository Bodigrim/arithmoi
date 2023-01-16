-- |
-- Module:      Math.NumberTheory.Zeta.Utils
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Shared utilities used by functions from @Math.NumberTheory.Zeta@.

module Math.NumberTheory.Zeta.Utils
  ( skipEvens
  , skipOdds
  ) where

import Data.List.Infinite (Infinite(..))
import qualified Data.List.Infinite as Inf

-- | Skips every odd-indexed element from an infinite list.
-- Do NOT use with finite lists.
--
-- >>> take 10 (skipOdds [0, 1 ..])
-- [0,2,4,6,8,10,12,14,16,18]
skipOdds :: Infinite a -> Infinite a
skipOdds (x :< _ :< xs) = x :< skipOdds xs

-- | Skips every even-indexed element from an infinite list.
-- Do NOT use with finite lists.
--
-- >>> take 10 (skipEvens [0, 1 ..])
-- [1,3,5,7,9,11,13,15,17,19]
skipEvens :: Infinite a -> Infinite a
skipEvens = skipOdds . Inf.tail
