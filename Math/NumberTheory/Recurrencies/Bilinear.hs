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

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Recurrencies.Bilinear
  ( binomial
  , stirling1
  , stirling2
  , lah
  , eulerian1
  , eulerian2
  , bernoulli

  , zetasEven
  , approximateValue
  , zetas
  ) where

import Data.List
import Data.Ratio
import Numeric.Natural

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Data.ExactPi

factorial :: (Num a, Enum a) => [a]
factorial = scanl (*) 1 [1..]
{-# SPECIALIZE factorial :: [Int]     #-}
{-# SPECIALIZE factorial :: [Word]    #-}
{-# SPECIALIZE factorial :: [Integer] #-}
{-# SPECIALIZE factorial :: [Natural] #-}

-- | Infinite zero-based table of binomial coefficients (also known as Pascal triangle):
-- @binomial !! n !! k == n! \/ k! \/ (n - k)!@.
--
-- > > take 5 (map (take 5) binomial)
-- > [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
--
-- Complexity: @binomial !! n !! k@ is O(n) bits long, its computation
-- takes O(k n) time and forces thunks @binomial !! n !! i@ for @0 <= i <= k@.
-- Use the symmetry of Pascal triangle @binomial !! n !! k == binomial !! n !! (n - k)@ to speed up computations.
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

-- | Infinite zero-based table of <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind Stirling numbers of the first kind>.
--
-- > > take 5 (map (take 5) stirling1)
-- > [[1],[0,1],[0,1,1],[0,2,3,1],[0,6,11,6,1]]
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
-- > > take 5 (map (take 5) stirling2)
-- > [[1],[0,1],[0,1,1],[0,1,3,1],[0,1,7,6,1]]
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
-- > > take 5 (map (take 5) lah)
-- > [[1],[2,1],[6,6,1],[24,36,12,1],[120,240,120,20,1]]
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
-- > > take 5 (map (take 5) eulerian1)
-- > [[],[1],[1,1],[1,4,1],[1,11,11,1]]
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
-- > > take 5 (map (take 5) eulerian2)
-- > [[],[1],[1,2],[1,8,6],[1,22,58,24]]
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
-- > > take 5 bernoulli
-- > [1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30]
--
-- Complexity: @bernoulli !! n@ is O(n ln n) bits long, its computation
-- takes O(n^3 ln n) time and forces thunks @stirling2 !! i !! j@ for @0 <= i <= n@ and @0 <= j <= i@.
--
-- One could also consider 'Math.Combinat.Numbers.bernoulli' to compute stand-alone values.
bernoulli :: Integral a => [Ratio a]
bernoulli = map f stirling2
  where
    f = sum . zipWith4 (\sgn denom fact stir -> sgn * fact * stir % denom) (cycle [1, -1]) [1..] factorial
{-# SPECIALIZE bernoulli :: [Ratio Int] #-}
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
      []       -> let v = f m y a in [v]
      (z : zs) -> let v = f m y z in (v : go (succ m) z zs)
{-# INLINE zipIndexedListWithTail #-}

-------------------------------------------------------------------------------
-- Zeta function

-- | Infinite sequence of exact values of Riemann zeta-function at even arguments, starting with @ζ(0)@.
-- Note that due to numerical errors convertation to 'Double' may return values below 1:
--
-- > > approximateValue (zetasEven !! 25) :: Double
-- > 0.9999999999999996
--
-- Use your favorite type for long-precision arithmetic. For instance, 'Data.Number.Fixed.Fixed' works fine:
--
-- > > approximateValue (zetasEven !! 25) :: Fixed Prec50
-- > 1.00000000000000088817842111574532859293035196051773
--
zetasEven :: [ExactPi]
zetasEven = zipWith Exact [0, 2 ..] $ zipWith (*) (skipOdds bernoulli) cs
  where
    cs = (- 1 % 2) : zipWith (\i f -> i * (-4) / fromInteger (2 * f * (2 * f - 1))) cs [1..]

skipOdds :: [a] -> [a]
skipOdds (x : _ : xs) = x : skipOdds xs
skipOdds xs = xs

zetasEven' :: (Floating a, Ord a) => [a]
zetasEven' = map approximateValue zetasEven

zetasOdd :: forall a. (Floating a, Ord a) => a -> [a]
zetasOdd eps = (1 / 0) : zets
  where
    zets :: [a] -- [zeta(3), zeta(5), zeta(7)...]
    zets = zipWith (*) zs (tail (iterate (* (- pi * pi)) 1))

    zs :: [a] -- [zeta(3) / (-pi^2), zeta(5) / pi^4, zeta(7) / (-pi^6)...]
    zs = zipWith (\w f -> negate (w / (1 + f))) ws fourth

    ys :: [a] -- [(1 - 1/4) * zeta(3) / (-pi^2), (1 - 1/4^2) * zeta(5) / pi^4...]
    ys = zipWith (*) zs fourth
    yss :: [[a]] -- [[], [ys !! 0], [ys !! 1, ys !! 0], [ys !! 2, ys !! 1, ys !! 0]...]
    yss = [] : zipWith (:) ys yss

    xs :: [a] -- first summand of RHS in (57) for m=[1..]
    xs = map (sum . zipWith (flip (/)) factorial2) yss

    ws :: [a] -- RHS in (57) for m=[1..]
    ws = zipWith (+) xs cs

    rs :: [a] -- [1, 1/2, 1/3, 1/4...]
    rs = map (\n -> recip (fromInteger n)) [1..]
    rss :: [[a]] -- [[1, 1/2, 1/3...], [1/2, 1/3, 1/4...], [1/3, 1/4...]]
    rss = rs : map tail rss

    factorial2 :: [a] -- [2!, 4!, 6!..]
    factorial2 = map fromInteger $ tail $ skipOdds factorial

    fourth :: [a] -- [1 - 1/4, 1 - 1/4^2, 1 - 1/4^3...]
    fourth = tail $ map (1 -) $ iterate (/ 4) 1

    as :: [a] -- [zeta(0), zeta(2)/4, zeta(2*2)/4^2, zeta(2*3)/4^3...]
    as = zipWith (/) zetasEven' (iterate (* 4) 1)

    bs :: [a] -- map (+ log 2) [b(1), b(2), b(3)...],
              -- where b(m) = \sum_{n=0}^\infty zeta(2n) / 4^n / (n + m)
    bs = map ((+ log 2) . suminf eps . zipWith (*) as) rss

    cs :: [a] -- second summand of RHS in (57) for m = [1..]
    cs = zipWith (\b f -> b / f) bs factorial2

suminf :: (Floating a, Ord a) => a -> [a] -> a
suminf eps = sum . takeWhile ((>= eps / 111) . abs)

-- | Infinite sequence of approximate (up to given precision)
-- values of Riemann zeta-function at integer arguments, starting with @ζ(0)@.
-- Computations for odd arguments are performed in accordance to
-- <https://cr.yp.to/bib/2000/borwein.pdf Computational strategies for the Riemann zeta function>
-- by J. M. Borwein, D. M. Bradley, R. E. Crandall, formula (57).
--
-- > > take 5 (zetas 1e-14) :: [Double]
-- > [-0.5,Infinity,1.6449340668482262,1.2020569031595942,1.0823232337111381]
--
-- Beware to force evaluation of @zetas !! 1@, if the type @a@ does not support infinite values
-- (for instance, 'Data.Number.Fixed.Fixed').
--
zetas :: (Floating a, Ord a) => a -> [a]
zetas eps = e : o : scanl1 f (intertwine es os)
  where
    e : es = zetasEven'
    o : os = zetasOdd eps

    intertwine (x : xs) (y : ys) = x : y : intertwine xs ys
    intertwine      xs       ys  = xs ++ ys

    -- Cap-and-floor to improve numerical stability:
    -- 0 < zeta(n + 1) - 1 < (zeta(n) - 1) / 2
    f x y = 1 `max` (y `min` (1 + (x - 1) / 2))
