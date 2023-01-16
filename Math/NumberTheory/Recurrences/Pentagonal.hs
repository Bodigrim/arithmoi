-- |
-- Module:      Math.NumberTheory.Recurrences.Pentagonal
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Values of <https://en.wikipedia.org/wiki/Partition_(number_theory)#Partition_function partition function>.
--

{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TypeApplications #-}

module Math.NumberTheory.Recurrences.Pentagonal
  ( partition
  ) where

import qualified Data.Chimera as Ch
import Data.List.Infinite (Infinite(..), (...))
import qualified Data.List.Infinite as Inf
import Data.Vector (Vector)
import Numeric.Natural (Natural)

-- | Infinite list of generalized pentagonal numbers.
-- Example:
--
-- >>> take 10 pents
-- [0,1,2,5,7,12,15,22,26,35]
pents :: (Enum a, Num a) => Infinite a
pents = Inf.interleave
  (Inf.scanl (\acc n -> acc + 3 * n - 1) 0 (1...))
  (Inf.scanl (\acc n -> acc + 3 * n - 2) 1 (2...))

-- | @p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-11) + ...@, where @p(0) = 1@
-- and @p(k) = 0@ for a negative integer @k@. Uses a @Chimera@ from the
-- @chimera@ package to memoize previous results.
partitionF :: Num a => (Word -> a) -> Word -> a
partitionF _ 0 = 1
partitionF f n
  = sum
  $ zipWith (*) (cycle [1, 1, -1, -1])
  $ map (f . (n -))
  $ Inf.takeWhile (<= n)
  $ Inf.tail pents

-- | Infinite zero-based table of <https://oeis.org/A000041 partition numbers>.
--
-- >>> take 10 partition
-- [1,1,2,3,5,7,11,15,22,30]
--
-- >>> :set -XDataKinds
-- >>> import Data.Mod
-- >>> partition !! 1000 :: Mod 1000
-- (991 `modulo` 1000)
partition :: Num a => Infinite a
partition = Inf.tabulate (Ch.index ch)
  where
    ch = Ch.tabulateFix @Vector partitionF
{-# SPECIALIZE partition :: Infinite Int     #-}
{-# SPECIALIZE partition :: Infinite Word    #-}
{-# SPECIALIZE partition :: Infinite Integer #-}
{-# SPECIALIZE partition :: Infinite Natural #-}
