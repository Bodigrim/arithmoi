-- |
-- Module:      Math.NumberTheory.Recurrencies.Bilinear
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
-- __Note on memory leaks and memoization.__
-- Top-level definitions in this module are polymorphic, so the results of computations are not retained in memory.
-- Make them monomorphic to take advantages of memoization. Compare
--
-- > > :set +s
-- > > binomial !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.01 secs, 1,385,512 bytes)
-- > > binomial !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.01 secs, 1,381,616 bytes)
--
-- against
--
-- > > let binomial' = binomial :: [[Integer]]
-- > > binomial' !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.01 secs, 1,381,696 bytes)
-- > > binomial' !! 1000 !! 1000 :: Integer
-- > 1
-- > (0.01 secs, 391,152 bytes)

{-# LANGUAGE CPP #-}

module Math.NumberTheory.Recurrencies.Bilinear
  ( binomial
  , stirling1
  , stirling2
  , eulerian1
  , eulerian2
  , bernoulli
  ) where

import Data.List
import Data.Ratio
import Numeric.Natural

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

factorial :: (Num a, Enum a) => [a]
factorial = scanl (*) 1 [1..]
{-# SPECIALIZE factorial :: [Int]     #-}
{-# SPECIALIZE factorial :: [Word]    #-}
{-# SPECIALIZE factorial :: [Integer] #-}
{-# SPECIALIZE factorial :: [Natural] #-}

-- | Infinite zero-based table of binomial coefficients: @binomial !! n !! k == n! \/ k! \/ (n - k)!@.
--
-- > > take 5 (map (take 5) binomial)
-- > [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
--
-- One could also consider 'Math.Combinat.Numbers.binomial' to compute stand-alone values.
binomial :: Integral a => [[a]]
binomial = map f [0..]
  where
    f n = scanl (\x k -> x * (n - k + 1) `div` k) 1 [1..n]
{-# SPECIALIZE binomial :: [[Int]]     #-}
{-# SPECIALIZE binomial :: [[Word]]    #-}
{-# SPECIALIZE binomial :: [[Integer]] #-}
{-# SPECIALIZE binomial :: [[Natural]] #-}

-- binomial_ :: Num a => [[a]]
-- binomial_ = map f [0..]
--   where
--     f n = go
-- (n k) = n! / k! / (n - k)! = n! / (k - 1)! / (n - k + 1)! * (n - k + 1) / k = (n k-1) * (n - k + 1) / k
--
-- {-# SPECIALIZE binomial_ :: [[Integer]] #-}
-- {-# SPECIALIZE binomial_ :: [[Natural]] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind Stirling numbers of the first kind>.
--
-- > > take 5 (map (take 5) stirling1)
-- > [[1],[0,1],[0,1,1],[0,2,3,1],[0,6,11,6,1]]
--
-- One could also consider 'Math.Combinat.Numbers.unsignedStirling1st' to compute stand-alone values.
stirling1 :: (Num a, Enum a) => [[a]]
stirling1 = scanl f [1] [0..]
  where
    f xs n = 0 : zipIndexedListWithTail (\_ x y -> x + n * y) 1 xs 0
{-# SPECIALIZE stirling1 :: [[Int]]     #-}
{-# SPECIALIZE stirling1 :: [[Word]]    #-}
{-# SPECIALIZE stirling1 :: [[Integer]] #-}
{-# SPECIALIZE stirling1 :: [[Natural]] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind Stirling numbers of the second kind>.
--
-- > > take 5 (map (take 5) stirling2)
-- > [[1],[0,1],[0,1,1],[0,1,3,1],[0,1,7,6,1]]
--
-- One could also consider 'Math.Combinat.Numbers.stirling2nd' to compute stand-alone values.
stirling2 :: (Num a, Enum a) => [[a]]
stirling2 = iterate f [1]
  where
    f xs = 0 : zipIndexedListWithTail (\k x y -> x + k * y) 1 xs 0
{-# SPECIALIZE stirling2 :: [[Int]]     #-}
{-# SPECIALIZE stirling2 :: [[Word]]    #-}
{-# SPECIALIZE stirling2 :: [[Integer]] #-}
{-# SPECIALIZE stirling2 :: [[Natural]] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Eulerian_number Eulerian numbers of the first kind>.
--
-- > > take 5 (map (take 5) eulerian1)
-- > [[],[1],[1,1],[1,4,1],[1,11,11,1]]
--
eulerian1 :: (Num a, Enum a) => [[a]]
eulerian1 = scanl f [] [1..]
  where
    f xs n = 1 : zipIndexedListWithTail (\k x y -> (n - k) * x + (k + 1) * y) 1 xs 0
{-# SPECIALIZE eulerian1 :: [[Int]]     #-}
{-# SPECIALIZE eulerian1 :: [[Word]]    #-}
{-# SPECIALIZE eulerian1 :: [[Integer]] #-}
{-# SPECIALIZE eulerian1 :: [[Natural]] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Eulerian_number#Eulerian_numbers_of_the_second_kind Eulerian numbers of the second kind>.
--
-- > > take 5 (map (take 5) eulerian2)
-- > [[],[1],[1,2],[1,8,6],[1,22,58,24]]
--
eulerian2 :: (Num a, Enum a) => [[a]]
eulerian2 = scanl f [] [1..]
  where
    f xs n = 1 : zipIndexedListWithTail (\k x y -> (2 * n - k - 1) * x + (k + 1) * y) 1 xs 0
{-# SPECIALIZE eulerian2 :: [[Int]]     #-}
{-# SPECIALIZE eulerian2 :: [[Word]]    #-}
{-# SPECIALIZE eulerian2 :: [[Integer]] #-}
{-# SPECIALIZE eulerian2 :: [[Natural]] #-}

-- | Infinite zero-based sequence of <https://en.wikipedia.org/wiki/Bernoulli_number Bernoulli numbers>,
-- computed via <https://en.wikipedia.org/wiki/Bernoulli_number#Connection_with_Stirling_numbers_of_the_second_kind connection>
-- with 'stirling2'.
--
-- > > take 5 bernoulli
-- > [1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30]
--
-- One could also consider 'Math.Combinat.Numbers.bernoulli' to compute stand-alone values.
bernoulli :: Integral a => [Ratio a]
bernoulli = map f stirling2
  where
    f = sum . zipWith4 (\sgn denom fact stir -> sgn * fact * stir % denom) (cycle [1, -1]) [1..] factorial
{-# SPECIALIZE bernoulli :: [Rational] #-}

-------------------------------------------------------------------------------
-- Utils

-- zipIndexedListWithTail f n as a == zipWith3 f [n..] as (tail as ++ [a])
-- but inlines much better and avoids checks for distinct sizes of lists.
zipIndexedListWithTail :: Enum b => (b -> a -> a -> b) -> b -> [a] -> a -> [b]
zipIndexedListWithTail f n as a = case as of
  []       -> []
  (x : xs) -> go n x xs
  where
    go m y ys = case ys of
      []       -> [f m y a]
      (z : zs) -> f m y z : go (succ m) z zs
{-# INLINE zipIndexedListWithTail #-}
