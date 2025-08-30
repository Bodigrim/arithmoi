-- |
-- Module:      Math.NumberTheory.Recurrences.Bilinear
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Bilinear recurrent sequences and Bernoulli numbers,
-- roughly covering Ch. 5-6 of /Concrete Mathematics/
-- by R. L. Graham, D. E. Knuth and O. Patashnik.
--
-- #memory# __Note on memory leaks and memoization.__
-- Top-level definitions in this module are polymorphic, so the results of computations are not retained in memory.
-- Make them monomorphic to take advantages of memoization. Compare
--
-- >>> binomial !! 1000 !! 1000 :: Integer -- (0.01 secs, 1,385,512 bytes)
-- 1
-- >>> binomial !! 1000 !! 1000 :: Integer -- (0.01 secs, 1,381,616 bytes)
-- 1
--
-- against
--
-- >>> let binomial' = binomial :: [[Integer]]
-- >>> binomial' !! 1000 !! 1000 :: Integer -- (0.01 secs, 1,381,696 bytes)
-- 1
-- >>> binomial' !! 1000 !! 1000 :: Integer -- (0.01 secs, 391,152 bytes)
-- 1

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Recurrences.Bilinear
  ( -- * Pascal triangle
    binomial
  , binomialRotated
  , binomialLine
  , binomialDiagonal
  , binomialFactors
    -- * Other recurrences
  , stirling1
  , stirling2
  , lah
  , eulerian1
  , eulerian2
  , bernoulli
  , euler
  , eulerPolyAt1
  , faulhaberPoly
  ) where

import Data.Euclidean (GcdDomain(..))
import Data.List (scanl', zipWith4)
import Data.List.Infinite (Infinite(..), (...))
import qualified Data.List.Infinite as Inf
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Ratio
import Data.Semiring (Semiring(..))
import Numeric.Natural

import Math.NumberTheory.Recurrences.Linear (factorial)
import Math.NumberTheory.Primes

-- | Infinite zero-based table of binomial coefficients (also known as Pascal triangle).
--
-- > binomial !! n !! k == n! / k! / (n - k)!
--
-- Note that 'binomial' !! n !! k is asymptotically slower
-- than 'binomialLine' n !! k,
-- but imposes only 'Semiring' constraint.
--
-- >>> take 6 binomial :: [[Int]]
-- [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
binomial :: Semiring a => Infinite [a]
binomial = Inf.iterate (\l -> zipWith plus (l ++ [zero]) (zero : l)) [one]
{-# SPECIALIZE binomial :: Infinite [Int]     #-}
{-# SPECIALIZE binomial :: Infinite [Word]    #-}
{-# SPECIALIZE binomial :: Infinite [Integer] #-}
{-# SPECIALIZE binomial :: Infinite [Natural] #-}

-- | Pascal triangle, rotated by 45 degrees.
--
-- > binomialRotated !! n !! k == (n + k)! / n! / k! == binomial !! (n + k) !! k
--
-- Note that 'binomialRotated' !! n !! k is asymptotically slower
-- than 'binomialDiagonal' n !! k,
-- but imposes only 'Semiring' constraint.
--
-- >>> take 6 (map (take 6) binomialRotated) :: [[Int]]
-- [[1,1,1,1,1,1],[1,2,3,4,5,6],[1,3,6,10,15,21],[1,4,10,20,35,56],[1,5,15,35,70,126],[1,6,21,56,126,252]]
binomialRotated :: Semiring a => Infinite (Infinite a)
binomialRotated = Inf.iterate (Inf.tail . Inf.scanl' plus zero) (Inf.repeat one)
{-# SPECIALIZE binomialRotated :: Infinite (Infinite Int)     #-}
{-# SPECIALIZE binomialRotated :: Infinite (Infinite Word)    #-}
{-# SPECIALIZE binomialRotated :: Infinite (Infinite Integer) #-}
{-# SPECIALIZE binomialRotated :: Infinite (Infinite Natural) #-}

-- | The n-th (zero-based) line of 'binomial'
-- (and the n-th diagonal of 'binomialRotated').
--
-- >>> binomialLine 5
-- [1,5,10,10,5,1]
binomialLine :: (Enum a, GcdDomain a) => a -> [a]
binomialLine n = scanl'
  (\x (k, nk1) -> fromJust $ (x `times` nk1) `divide` k)
  one
  (zip [one..n] [n, pred n..one])
{-# SPECIALIZE binomialLine :: Int     -> [Int]     #-}
{-# SPECIALIZE binomialLine :: Word    -> [Word]    #-}
{-# SPECIALIZE binomialLine :: Integer -> [Integer] #-}
{-# SPECIALIZE binomialLine :: Natural -> [Natural] #-}

-- | The n-th (zero-based) diagonal of 'binomial'
-- (and the n-th line of 'binomialRotated').
--
-- >>> take 6 (binomialDiagonal 5)
-- [1,6,21,56,126,252]
binomialDiagonal :: (Enum a, GcdDomain a) => a -> Infinite a
binomialDiagonal n = Inf.scanl'
  (\x k -> fromJust (x `times` (n `plus` k) `divide` k))
  one
  (one...)
{-# SPECIALIZE binomialDiagonal :: Int     -> Infinite Int     #-}
{-# SPECIALIZE binomialDiagonal :: Word    -> Infinite Word    #-}
{-# SPECIALIZE binomialDiagonal :: Integer -> Infinite Integer #-}
{-# SPECIALIZE binomialDiagonal :: Natural -> Infinite Natural #-}

-- | Prime factors of a binomial coefficient.
--
-- > binomialFactors n k == factorise (binomial !! n !! k)
--
-- >>> binomialFactors 10 4
-- [(Prime 2,1),(Prime 3,1),(Prime 5,1),(Prime 7,1)]
binomialFactors :: Word -> Word -> [(Prime Word, Word)]
binomialFactors n k
  | n < 2
  = []
  | otherwise
  = filter ((/= 0) . snd)
  $ map (\p -> (p, mult (unPrime p) n - mult (unPrime p) (n - k) - mult (unPrime p) k))
    [minBound .. precPrime n]
  where
    mult :: Word -> Word -> Word
    mult p m = go mp mp
      where
        mp = m `quot` p
        go !acc !x
          | x >= p = let xp = x `quot` p in go (acc + xp) xp
          | otherwise = acc

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind Stirling numbers of the first kind>.
--
-- >>> take 5 (map (take 5) stirling1)
-- [[1],[0,1],[0,1,1],[0,2,3,1],[0,6,11,6,1]]
--
-- Complexity: @stirling1 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @stirling1 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
--
-- One could also consider 'Math.Combinat.Numbers.unsignedStirling1st' from <http://hackage.haskell.org/package/combinat combinat> package to compute stand-alone values.
stirling1 :: (Num a, Enum a) => Infinite [a]
stirling1 = Inf.scanl f [1] (0...)
  where
    f xs n = 0 : zipIndexedListWithTail (\_ x y -> x + n * y) 1 xs 0
{-# SPECIALIZE stirling1 :: Infinite [Int]     #-}
{-# SPECIALIZE stirling1 :: Infinite [Word]    #-}
{-# SPECIALIZE stirling1 :: Infinite [Integer] #-}
{-# SPECIALIZE stirling1 :: Infinite [Natural] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind Stirling numbers of the second kind>.
--
-- >>> take 5 (map (take 5) stirling2)
-- [[1],[0,1],[0,1,1],[0,1,3,1],[0,1,7,6,1]]
--
-- Complexity: @stirling2 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @stirling2 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
--
-- One could also consider 'Math.Combinat.Numbers.stirling2nd' from <http://hackage.haskell.org/package/combinat combinat> package to compute stand-alone values.
stirling2 :: (Num a, Enum a) => Infinite [a]
stirling2 = Inf.iterate f [1]
  where
    f xs = 0 : zipIndexedListWithTail (\k x y -> x + k * y) 1 xs 0
{-# SPECIALIZE stirling2 :: Infinite [Int]     #-}
{-# SPECIALIZE stirling2 :: Infinite [Word]    #-}
{-# SPECIALIZE stirling2 :: Infinite [Integer] #-}
{-# SPECIALIZE stirling2 :: Infinite [Natural] #-}

-- | Infinite one-based table of <https://en.wikipedia.org/wiki/Lah_number Lah numbers>.
-- @lah !! n !! k@ equals to lah(n + 1, k + 1).
--
-- >>> take 5 (map (take 5) lah)
-- [[1],[2,1],[6,6,1],[24,36,12,1],[120,240,120,20,1]]
--
-- Complexity: @lah !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n ln n) time and forces thunks @lah !! n !! i@ for @0 <= i <= k@.
lah :: Integral a => Infinite [a]
-- Implementation was derived from code by https://github.com/grandpascorpion
lah = Inf.zipWith f (Inf.tail factorial) (1...)
  where
    f nf n = scanl (\x k -> x * (n - k) `div` (k * (k + 1))) nf [1..n-1]
{-# SPECIALIZE lah :: Infinite [Int]     #-}
{-# SPECIALIZE lah :: Infinite [Word]    #-}
{-# SPECIALIZE lah :: Infinite [Integer] #-}
{-# SPECIALIZE lah :: Infinite [Natural] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Eulerian_number Eulerian numbers of the first kind>.
--
-- >>> take 5 (map (take 5) eulerian1)
-- [[],[1],[1,1],[1,4,1],[1,11,11,1]]
--
-- Complexity: @eulerian1 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @eulerian1 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
--
eulerian1 :: (Num a, Enum a) => Infinite [a]
eulerian1 = Inf.scanl f [] (1...)
  where
    f xs n = 1 : zipIndexedListWithTail (\k x y -> (n - k) * x + (k + 1) * y) 1 xs 0
{-# SPECIALIZE eulerian1 :: Infinite [Int]     #-}
{-# SPECIALIZE eulerian1 :: Infinite [Word]    #-}
{-# SPECIALIZE eulerian1 :: Infinite [Integer] #-}
{-# SPECIALIZE eulerian1 :: Infinite [Natural] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Eulerian_number#Eulerian_numbers_of_the_second_kind Eulerian numbers of the second kind>.
--
-- >>> take 5 (map (take 5) eulerian2)
-- [[],[1],[1,2],[1,8,6],[1,22,58,24]]
--
-- Complexity: @eulerian2 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @eulerian2 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
--
eulerian2 :: (Num a, Enum a) => Infinite [a]
eulerian2 = Inf.scanl f [] (1...)
  where
    f xs n = 1 : zipIndexedListWithTail (\k x y -> (2 * n - k - 1) * x + (k + 1) * y) 1 xs 0
{-# SPECIALIZE eulerian2 :: Infinite [Int]     #-}
{-# SPECIALIZE eulerian2 :: Infinite [Word]    #-}
{-# SPECIALIZE eulerian2 :: Infinite [Integer] #-}
{-# SPECIALIZE eulerian2 :: Infinite [Natural] #-}

-- | Infinite zero-based sequence of <https://en.wikipedia.org/wiki/Bernoulli_number Bernoulli numbers>,
-- computed via <https://en.wikipedia.org/wiki/Bernoulli_number#Connection_with_Stirling_numbers_of_the_second_kind connection>
-- with 'stirling2'.
--
-- >>> take 5 bernoulli
-- [1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30]
--
-- Complexity: @bernoulli !! n@ is O(n ln n) bits long, its computation
-- takes O(n^3 ln n) time and forces thunks @stirling2 !! i !! j@ for @0 <= i <= n@ and @0 <= j <= i@.
--
-- One could also consider 'Math.Combinat.Numbers.bernoulli' from <http://hackage.haskell.org/package/combinat combinat> package to compute stand-alone values.
bernoulli :: Integral a => Infinite (Ratio a)
bernoulli = helperForBEEP id (Inf.map recip (1...))
{-# SPECIALIZE bernoulli :: Infinite (Ratio Int) #-}
{-# SPECIALIZE bernoulli :: Infinite (Rational) #-}

-- | <https://en.wikipedia.org/wiki/Faulhaber%27s_formula Faulhaber's formula>.
--
-- >>> sum (map (^ 10) [0..100])
-- 959924142434241924250
-- >>> sum $ zipWith (*) (faulhaberPoly 10) (iterate (* 100) 1)
-- 959924142434241924250 % 1
faulhaberPoly :: (GcdDomain a, Integral a) => Int -> [Ratio a]
-- Implementation by https://github.com/CarlEdman
faulhaberPoly p
  = zipWith (*) ((0:)
  $ reverse
  $ Inf.take (p + 1) bernoulli)
  $ map (% (fromIntegral p+1))
  $ zipWith (*) (iterate negate (if odd p then 1 else -1))
  $ binomial Inf.!! (fromIntegral (p+1))

-- | Infinite zero-based list of <https://en.wikipedia.org/wiki/Euler_number Euler numbers>.
-- The algorithm used was derived from <http://www.emis.ams.org/journals/JIS/VOL4/CHEN/AlgBE2.pdf Algorithms for Bernoulli numbers and Euler numbers>
-- by Kwang-Wu Chen, second formula of the Corollary in page 7.
-- Sequence <https://oeis.org/A122045 A122045> in OEIS.
--
-- >>> take 10 euler' :: [Rational]
-- [1 % 1,0 % 1,(-1) % 1,0 % 1,5 % 1,0 % 1,(-61) % 1,0 % 1,1385 % 1,0 % 1]
euler' :: forall a . Integral a => Infinite (Ratio a)
euler' = Inf.tail $ helperForBEEP (drop 1) as
  where
    as :: Infinite (Ratio a)
    as = Inf.zipWith3
        (\sgn frac ones -> (sgn * ones) % frac)
        (Inf.cycle (1 :| [1, 1, 1, -1, -1, -1, -1]))
        (dups (Inf.iterate (2 *) 1))
        (Inf.cycle (1 :| [1, 1, 0]))

    dups :: forall x . Infinite x -> Infinite x
    dups = Inf.foldr (\n list -> n :< n :< list)
{-# SPECIALIZE euler' :: Infinite (Ratio Int)     #-}
{-# SPECIALIZE euler' :: Infinite (Rational)      #-}

-- | The same sequence as @euler'@, but with type @[a]@ instead of @[Ratio a]@
-- as the denominators in @euler'@ are always @1@.
--
-- >>> take 10 euler :: [Integer]
-- [1,0,-1,0,5,0,-61,0,1385,0]
euler :: forall a . Integral a => Infinite a
euler = Inf.map numerator euler'

-- | Infinite zero-based list of the @n@-th order Euler polynomials evaluated at @1@.
-- The algorithm used was derived from <http://www.emis.ams.org/journals/JIS/VOL4/CHEN/AlgBE2.pdf Algorithms for Bernoulli numbers and Euler numbers>
-- by Kwang-Wu Chen, third formula of the Corollary in page 7.
-- Element-by-element division of sequences <https://oeis.org/A198631 A1986631>
-- and <https://oeis.org/A006519 A006519> in OEIS.
--
-- >>> take 10 eulerPolyAt1 :: [Rational]
-- [1 % 1,1 % 2,0 % 1,(-1) % 4,0 % 1,1 % 2,0 % 1,(-17) % 8,0 % 1,31 % 2]
eulerPolyAt1 :: forall a . Integral a => Infinite (Ratio a)
eulerPolyAt1 = Inf.tail $ helperForBEEP (drop 1) (Inf.map recip (Inf.iterate (2 *) 1))
{-# SPECIALIZE eulerPolyAt1 :: Infinite (Ratio Int)     #-}
{-# SPECIALIZE eulerPolyAt1 :: Infinite (Rational)      #-}

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
      []       -> let v = f m y a in [v]
      (z : zs) -> let v = f m y z in (v : go (succ m) z zs)
{-# INLINE zipIndexedListWithTail #-}

-- | Helper for common code in @bernoulli, euler, eulerPolyAt1. All three
-- sequences rely on @stirling2@ and have the same general structure of
-- zipping four lists together with multiplication, with one of those lists
-- being the sublists in @stirling2@, and two of them being the factorial
-- sequence and @cycle [1, -1]@. The remaining list is passed to
-- @helperForBEEP@ as an argument.
--
-- Note: This function has a @([Ratio a] -> [Ratio a])@ argument because
-- @bernoulli !! n@ will use, for all nonnegative @n@, every element in
-- @stirling2 !! n@, while @euler, eulerPolyAt1@ only use
-- @tail $ stirling2 !! n@. As such, this argument serves to pass @id@
-- in the former case, and @tail@ in the latter.
helperForBEEP :: Integral a => (forall b. [b] -> [b]) -> Infinite (Ratio a) -> Infinite (Ratio a)
helperForBEEP g xs = Inf.map (f . g) stirling2
  where
    f = sum . zipWith4 (\sgn fact x stir -> sgn * fact * x * stir) (cycle [1, -1]) (Inf.toList factorial) (Inf.toList xs)
{-# INLINABLE helperForBEEP #-}
