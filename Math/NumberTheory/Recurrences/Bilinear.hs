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

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Recurrences.Bilinear
  ( binomial
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
import Data.List
import Data.Maybe
import Data.Ratio
import Numeric.Natural

import Math.NumberTheory.Recurrences.Linear (factorial)

-- | Infinite zero-based table of binomial coefficients (also known as Pascal triangle):
-- @binomial !! n !! k == n! \/ k! \/ (n - k)!@.
--
-- >>> take 5 (map (take 5) binomial)
-- [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
--
-- Complexity: @binomial !! n !! k@ is O(n) bits long, its computation
-- takes O(k n) time and forces thunks @binomial !! n !! i@ for @0 <= i <= k@.
-- Use the symmetry of Pascal triangle @binomial !! n !! k == binomial !! n !! (n - k)@ to speed up computations.
--
-- One could also consider 'Math.Combinat.Numbers.binomial' to compute stand-alone values.
binomial :: (Num a, GcdDomain a) => [[a]]
binomial = map f [0..]
  where
    f n = scanl (\x k -> fromJust $ x * (fromInteger $ n - k + 1) `divide` fromInteger k) 1 [1..n]
{-# SPECIALIZE binomial :: [[Int]]     #-}
{-# SPECIALIZE binomial :: [[Word]]    #-}
{-# SPECIALIZE binomial :: [[Integer]] #-}
{-# SPECIALIZE binomial :: [[Natural]] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind Stirling numbers of the first kind>.
--
-- >>> take 5 (map (take 5) stirling1)
-- [[1],[0,1],[0,1,1],[0,2,3,1],[0,6,11,6,1]]
--
-- Complexity: @stirling1 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @stirling1 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
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
-- >>> take 5 (map (take 5) stirling2)
-- [[1],[0,1],[0,1,1],[0,1,3,1],[0,1,7,6,1]]
--
-- Complexity: @stirling2 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @stirling2 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
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

-- | Infinite one-based table of <https://en.wikipedia.org/wiki/Lah_number Lah numbers>.
-- @lah !! n !! k@ equals to lah(n + 1, k + 1).
--
-- >>> take 5 (map (take 5) lah)
-- [[1],[2,1],[6,6,1],[24,36,12,1],[120,240,120,20,1]]
--
-- Complexity: @lah !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n ln n) time and forces thunks @lah !! n !! i@ for @0 <= i <= k@.
lah :: Integral a => [[a]]
-- Implementation was derived from code by https://github.com/grandpascorpion
lah = zipWith f (tail factorial) [1..]
  where
    f nf n = scanl (\x k -> x * (n - k) `div` (k * (k + 1))) nf [1..n-1]
{-# SPECIALIZE lah :: [[Int]]     #-}
{-# SPECIALIZE lah :: [[Word]]    #-}
{-# SPECIALIZE lah :: [[Integer]] #-}
{-# SPECIALIZE lah :: [[Natural]] #-}

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Eulerian_number Eulerian numbers of the first kind>.
--
-- >>> take 5 (map (take 5) eulerian1)
-- [[],[1],[1,1],[1,4,1],[1,11,11,1]]
--
-- Complexity: @eulerian1 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @eulerian1 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
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
-- >>> take 5 (map (take 5) eulerian2)
-- [[],[1],[1,2],[1,8,6],[1,22,58,24]]
--
-- Complexity: @eulerian2 !! n !! k@ is O(n ln n) bits long, its computation
-- takes O(k n^2 ln n) time and forces thunks @eulerian2 !! i !! j@ for @0 <= i <= n@ and @max(0, k - n + i) <= j <= k@.
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
-- >>> take 5 bernoulli
-- [1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30]
--
-- Complexity: @bernoulli !! n@ is O(n ln n) bits long, its computation
-- takes O(n^3 ln n) time and forces thunks @stirling2 !! i !! j@ for @0 <= i <= n@ and @0 <= j <= i@.
--
-- One could also consider 'Math.Combinat.Numbers.bernoulli' to compute stand-alone values.
bernoulli :: Integral a => [Ratio a]
bernoulli = helperForB_E_EP id (map recip [1..])
{-# SPECIALIZE bernoulli :: [Ratio Int] #-}
{-# SPECIALIZE bernoulli :: [Rational] #-}

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
  $ take (p+1) $ bernoulli)
  $ map (% (fromIntegral p+1))
  $ zipWith (*) (iterate negate (if odd p then 1 else -1))
  $ binomial !! (p+1)

-- | Infinite zero-based list of <https://en.wikipedia.org/wiki/Euler_number Euler numbers>.
-- The algorithm used was derived from <http://www.emis.ams.org/journals/JIS/VOL4/CHEN/AlgBE2.pdf Algorithms for Bernoulli numbers and Euler numbers>
-- by Kwang-Wu Chen, second formula of the Corollary in page 7.
-- Sequence <https://oeis.org/A122045 A122045> in OEIS.
--
-- >>> take 10 euler' :: [Rational]
-- [1 % 1,0 % 1,(-1) % 1,0 % 1,5 % 1,0 % 1,(-61) % 1,0 % 1,1385 % 1,0 % 1]
euler' :: forall a . Integral a => [Ratio a]
euler' = tail $ helperForB_E_EP tail as
  where
    as :: [Ratio a]
    as = zipWith3
        (\sgn frac ones -> (sgn * ones) % frac)
        (cycle [1, 1, 1, 1, -1, -1, -1, -1])
        (dups (iterate (2 *) 1))
        (cycle [1, 1, 1, 0])

    dups :: forall x . [x] -> [x]
    dups = foldr (\n list -> n : n : list) []
{-# SPECIALIZE euler' :: [Ratio Int]     #-}
{-# SPECIALIZE euler' :: [Rational]      #-}

-- | The same sequence as @euler'@, but with type @[a]@ instead of @[Ratio a]@
-- as the denominators in @euler'@ are always @1@.
--
-- >>> take 10 euler :: [Integer]
-- [1,0,-1,0,5,0,-61,0,1385,0]
euler :: forall a . Integral a => [a]
euler = map numerator euler'

-- | Infinite zero-based list of the @n@-th order Euler polynomials evaluated at @1@.
-- The algorithm used was derived from <http://www.emis.ams.org/journals/JIS/VOL4/CHEN/AlgBE2.pdf Algorithms for Bernoulli numbers and Euler numbers>
-- by Kwang-Wu Chen, third formula of the Corollary in page 7.
-- Element-by-element division of sequences <https://oeis.org/A198631 A1986631>
-- and <https://oeis.org/A006519 A006519> in OEIS.
--
-- >>> take 10 eulerPolyAt1 :: [Rational]
-- [1 % 1,1 % 2,0 % 1,(-1) % 4,0 % 1,1 % 2,0 % 1,(-17) % 8,0 % 1,31 % 2]
eulerPolyAt1 :: forall a . Integral a => [Ratio a]
eulerPolyAt1 = tail $ helperForB_E_EP tail (map recip (iterate (2 *) 1))
{-# SPECIALIZE eulerPolyAt1 :: [Ratio Int]     #-}
{-# SPECIALIZE eulerPolyAt1 :: [Rational]      #-}

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
-- @helperForB_E_EP@ as an argument.
--
-- Note: This function has a @([Ratio a] -> [Ratio a])@ argument because
-- @bernoulli !! n@ will use, for all nonnegative @n@, every element in
-- @stirling2 !! n@, while @euler, eulerPolyAt1@ only use
-- @tail $ stirling2 !! n@. As such, this argument serves to pass @id@
-- in the former case, and @tail@ in the latter.
helperForB_E_EP :: Integral a => ([Ratio a] -> [Ratio a]) -> [Ratio a] -> [Ratio a]
helperForB_E_EP g xs = map (f . g) stirling2
  where
    f = sum . zipWith4 (\sgn fact x stir -> sgn * fact * x * stir) (cycle [1, -1]) factorial xs
{-# SPECIALIZE helperForB_E_EP :: ([Ratio Int] -> [Ratio Int]) -> [Ratio Int] -> [Ratio Int] #-}
{-# SPECIALIZE helperForB_E_EP :: ([Rational] -> [Rational]) -> [Rational] -> [Rational]     #-}
