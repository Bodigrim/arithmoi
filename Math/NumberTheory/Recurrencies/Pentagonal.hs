-- Values of <https://en.wikipedia.org/wiki/Partition_(number_theory)#Partition_function partition function>.

{-# LANGUAGE RankNTypes   #-}

module Math.NumberTheory.Recurrencies.Pentagonal
  ( partition
  , pentagonalSigns
  , pents
  ) where

import qualified Data.IntMap as IM
import Numeric.Natural       (Natural)

-- | Infinite list of generalized pentagonal numbers.
-- Example:
--
-- >>> take 10 pents
-- [0, 1, 2, 5, 7, 12 ,15, 22, 26, 35]
pents :: (Enum a, Num a) => [a]
pents = interleave (scanl (\acc n -> acc + 3 * n - 1) 0 [1..])
                   (scanl (\acc n -> acc + 3 * n - 2) 1 [2..])
  where
    interleave :: [a] -> [a] -> [a]
    interleave (n : ns) (m : ms) = n : m : interleave ns ms
    interleave _ _ = []

-- | When calculating the @n@-th partition number @p(n)@ using the sum
-- @p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-11) + ...@, the signs of each
-- term alternate every two terms, starting with a positive sign.
-- @pentagonalSigns@ takes a list of numbers and produces such an alternated
-- sequence.
-- Examples:
--
-- >>> pentagonalSigns [1..5]
-- [1, 2, -3, -4, 5]
--
-- >>> pentagonalSigns [1..6]
-- [1, 2, -3, -4, 5, 6]
pentagonalSigns :: Num a => [a] -> [a]
pentagonalSigns = zipWith (*) (cycle [1, 1, -1, -1])

-- | @partition !! n@ calculates the @n@-th partition number:
-- @p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-11) + ...@, where @p(0) = 1@
-- and @p(k) = 0@ for a negative integer @k@. Uses a @Map@ from the
-- @containers@ package to memoize previous results.
-- 
-- Example: calculating @partition !! 10@, assuming the memoization map is
-- filled and called @dict :: Integral a => Map a a@.
-- 
-- * @tail [0, 1, 2, 5, 7, 12 ,15, 22, 26, 35, ..] == [1, 2, 5, 7, 12 ,15, 22, 26, 35, 40, ..]@.
-- * @takeWhile (\m -> 10 - m >= 0) == [1, 2, 5, 7]@.
-- * @map (\m -> dict ! fromIntegral (10 - m)) [1, 2, 5, 7] == [dict ! 9, dict ! 8, dict ! 5, dict ! 3] == [30, 22, 7, 3]@
-- * @pentagonalSigns [30, 22, 7, 3] == [30, 22, 7, 3] == [30, 22, -7, -3]@
-- * @sum [30, 22, -7, -3] == 42@
--
-- Note: @tail@ is applied to @pents@ because otherwise the calculation of
-- @p(n)@ would involve a duplicated @p(n-1)@ term (see the above example).
partition :: forall a . (Enum a, Num a, Ord a) => [a]
partition = 1 : go (IM.singleton 0 1) [1..]
  where
    go :: forall a . (Enum a, Num a, Ord a) => IM.IntMap a -> [Int] -> [a]
    go dict (n : ns) =
        let intN = fromEnum n
            n' = (sum .
                  pentagonalSigns .
                  map (\m -> dict IM.! (intN - m)) .
                  takeWhile (\m -> intN >= m) .
                  tail) (pents :: [Int])
            dict' = IM.insert intN n' dict
        in n' : go dict' ns
    go _ _ = []
{-# SPECIALIZE partition :: [Int]     #-}
{-# SPECIALIZE partition :: [Word]    #-}
{-# SPECIALIZE partition :: [Integer] #-}
{-# SPECIALIZE partition :: [Natural] #-}