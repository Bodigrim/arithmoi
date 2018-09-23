-- |
-- Module:      Math.NumberTheory.SmoothNumbers
-- Copyright:   (c) 2018 Frederick Schneider
-- Licence:     MIT
-- Maintainer:  Frederick Schneider <frederick.schneider2011@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- A <https://en.wikipedia.org/wiki/Smooth_number smooth number>
-- is an integer, which can be represented as a product of powers of elements
-- from a given set (smooth basis). E. g., 48 = 3 * 4 * 4 is smooth
-- over a set {3, 4}, and 24 is not.
--

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.SmoothNumbers
  ( -- * Create a smooth basis
    SmoothBasis
  , fromSet
  , fromList
  , fromSmoothUpperBound
    -- * Generate smooth numbers
  , smoothOver
  , smoothOverInRange
  , smoothOverInRangeBF
  ) where

import Prelude hiding (div, mod, gcd)
import Data.Coerce
import Data.List (nub)
import qualified Data.Set as S
import Math.NumberTheory.Euclidean
import Math.NumberTheory.Primes (unPrime)
import Math.NumberTheory.Primes.Sieve (primes)

-- | An abstract representation of a smooth basis.
-- It consists of a set of coprime numbers ≥2.
newtype SmoothBasis a = SmoothBasis { unSmoothBasis :: [a] } deriving (Eq, Show)

-- | Build a 'SmoothBasis' from a set of coprime numbers ≥2.
--
-- >>> import qualified Data.Set as Set
-- >>> fromSet (Set.fromList [2, 3])
-- Just (SmoothBasis [2, 3])
-- >>> fromSet (Set.fromList [2, 4]) -- should be coprime
-- Nothing
-- >>> fromSet (Set.fromList [1, 3]) -- should be >= 2
-- Nothing
fromSet :: Euclidean a => S.Set a -> Maybe (SmoothBasis a)
fromSet s = if isValid l then Just (SmoothBasis l) else Nothing where l = S.elems s

-- | Build a 'SmoothBasis' from a list of coprime numbers ≥2.
--
-- >>> fromList [2, 3]
-- Just (SmoothBasis [2, 3])
-- >>> fromList [2, 2]
-- Just (SmoothBasis [2])
-- >>> fromList [2, 4] -- should be coprime
-- Nothing
-- >>> fromList [1, 3] -- should be >= 2
-- Nothing
fromList :: Euclidean a => [a] -> Maybe (SmoothBasis a)
fromList l = if isValid l' then Just (SmoothBasis l') else Nothing
  where
    l' = nub l

-- | Build a 'SmoothBasis' from a list of primes below given bound.
--
-- >>> fromSmoothUpperBound 10
-- Just (SmoothBasis [2, 3, 5, 7])
-- >>> fromSmoothUpperBound 1
-- Nothing
fromSmoothUpperBound :: Integral a => a -> Maybe (SmoothBasis a)
fromSmoothUpperBound n = if (n < 2)
                         then Nothing
                         else Just $ SmoothBasis $ takeWhile (<= n) $ map unPrime primes

-- | Generate an infinite ascending list of
-- <https://en.wikipedia.org/wiki/Smooth_number smooth numbers>
-- over a given smooth basis.
--
-- >>> import Data.Maybe
-- >>> take 10 (smoothOver (fromJust (fromList [2, 5])))
-- [1, 2, 4, 5, 8, 10, 16, 20, 25, 32]
smoothOver :: Integral a => SmoothBasis a -> [a]
smoothOver pl = foldr (\p l -> mergeListLists $ iterate (map (p*)) l) [1] (unSmoothBasis pl)
  where
    {-# INLINE mergeListLists #-}
    mergeListLists      = foldr go1 []
      where
        go1 :: Ord a => [a] -> [a] -> [a]
        go1 (h:t) b = h:(go2 t b)
        go1 _     b = b

        go2 :: Ord a => [a] -> [a] -> [a]
        go2 a@(ah:at) b@(bh:bt)
          | bh < ah   = bh : (go2 a bt)
          | otherwise = ah : (go2 at b) -- no possibility of duplicates
        go2 a b = if null a then b else a

-- | Generate an ascending list of
-- <https://en.wikipedia.org/wiki/Smooth_number smooth numbers>
-- over a given smooth basis in a given range.
--
-- It may appear inefficient
-- for short, but distant ranges;
-- consider using 'smoothOverInRangeBF' in such cases.
--
-- >>> import Data.Maybe
-- >>> smoothOverInRange (fromJust (fromList [2, 5])) 100 200
-- [100, 125, 128, 160, 200]
smoothOverInRange :: forall a. Integral a => SmoothBasis a -> a -> a -> [a]
smoothOverInRange s lo hi
  = takeWhile (<= hi)
  $ dropWhile (< lo)
  $ coerce
  $ smoothOver (coerce s :: SmoothBasis (WrappedIntegral a))

-- | Generate an ascending list of
-- <https://en.wikipedia.org/wiki/Smooth_number smooth numbers>
-- over a given smooth basis in a given range.
--
-- It is inefficient
-- for large or starting near 0 ranges;
-- consider using 'smoothOverInRange' in such cases.
--
-- Suffix BF stands for the brute force algorithm, involving a lot of divisions.
--
-- >>> import Data.Maybe
-- >>> smoothOverInRangeBF (fromJust (fromList [2, 5])) 100 200
-- [100, 125, 128, 160, 200]
smoothOverInRangeBF :: forall a. Integral a => SmoothBasis a -> a -> a -> [a]
smoothOverInRangeBF prs lo hi
  = coerce
  $ filter (mf prs')
  $ coerce [lo..hi]
  where
    mf :: [WrappedIntegral a] -> WrappedIntegral a -> Bool
    mf _         0 = False
    mf []        n = n == 1 -- mf means manually factor
    mf pl@(p:ps) n = if mod n p == 0
                     then mf pl (div n p)
                     else mf ps n
    prs'           = coerce $ unSmoothBasis prs

-- | isValid assumes that the list is sorted and unique and then checks if the list is suitable to be a SmoothBasis.
isValid :: Euclidean a => [a] -> Bool
isValid pl = length pl /= 0 && v' pl
  where
    v' :: Euclidean a => [a] -> Bool
    v' []     = True
    v' (x:xs) = x /= 0 && abs x /= 1 && abs x == x && all (coprime x) xs && v' xs
