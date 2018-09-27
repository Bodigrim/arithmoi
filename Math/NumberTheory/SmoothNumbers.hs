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
  , smoothOver'
  , smoothOverInRange
  , smoothOverInRangeBF

  -- * Verify if a number is smooth
  , isSmooth
  ) where

import Prelude hiding (div, mod, gcd)
import Data.Coerce
import Data.List (nub)
import qualified Data.Set as S
import qualified Math.NumberTheory.Euclidean as E
import Math.NumberTheory.Primes (unPrime)
import Math.NumberTheory.Primes.Sieve (primes)

-- | An abstract representation of a smooth basis.
-- It consists of a set of numbers ≥2.
newtype SmoothBasis a = SmoothBasis { unSmoothBasis :: [a] } deriving (Eq, Show)

-- | Build a 'SmoothBasis' from a set of numbers ≥2.
--
-- >>> import qualified Data.Set as Set
-- >>> fromSet (Set.fromList [2, 3])
-- Just (SmoothBasis [2, 3])
-- >>> fromSet (Set.fromList [2, 4])
-- Just (SmoothBasis [2, 4])
-- >>> fromSet (Set.fromList [1, 3]) -- should be >= 2
-- Nothing
fromSet :: E.Euclidean a => S.Set a -> Maybe (SmoothBasis a)
fromSet s = if isValid l then Just (SmoothBasis l) else Nothing where l = S.elems s

-- | Build a 'SmoothBasis' from a list of numbers ≥2.
--
-- >>> fromList [2, 3]
-- Just (SmoothBasis [2, 3])
-- >>> fromList [2, 2]
-- Just (SmoothBasis [2])
-- >>> fromList [2, 4]
-- Just (SmoothBasis [2, 4])
-- >>> fromList [1, 3] -- should be >= 2
-- Nothing
fromList :: E.Euclidean a => [a] -> Maybe (SmoothBasis a)
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

-- | Helper used by @smoothOver@ (@Integral@ constraint) and @smoothOver'@
-- (@Euclidean@ constraint) Since the typeclass constraint is just
-- @Num@, it receives a @norm@ comparison function for the generated smooth
-- numbers.
-- This function relies on the fact that for any element of a smooth basis @p@
-- and any @a@ it is true that @norm (a * p) > norm a@.
-- This condition is not checked.
smoothOver' :: forall a b . (Eq a, Num a, Ord b) => (a -> b) -> SmoothBasis a -> [a]
smoothOver' norm pl =
    foldr
    (\p l -> mergeListLists $ iterate (map (p*)) l)
    [1]
    (nub $ unSmoothBasis pl)
  where
    {-# INLINE mergeListLists #-}
    mergeListLists      = foldr go1 []
      where
        go1 :: [a] -> [a] -> [a]
        go1 (h:t) b = h:(go2 t b)
        go1 _     b = b

        go2 :: [a] -> [a] -> [a]
        go2 a@(ah:at) b@(bh:bt)
          | norm bh < norm ah   = bh : (go2 a bt)
          | otherwise = ah : (go2 at b) -- no possibility of duplicates
        go2 a b = if null a then b else a

-- | Generate an infinite ascending list of
-- <https://en.wikipedia.org/wiki/Smooth_number smooth numbers>
-- over a given smooth basis.
--
-- >>> import Data.Maybe
-- >>> take 10 (smoothOver (fromJust (fromList [2, 5])))
-- [1, 2, 4, 5, 8, 10, 16, 20, 25, 32]
smoothOver :: Integral a => SmoothBasis a -> [a]
smoothOver = smoothOver' abs

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
  $ smoothOver (coerce s :: SmoothBasis (E.WrappedIntegral a))

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
smoothOverInRangeBF 
  :: forall a. (Enum a, E.Euclidean a)
  => SmoothBasis a
  -> a
  -> a
  -> [a]
smoothOverInRangeBF prs lo hi
  = coerce
  $ filter (isSmooth prs)
  $ coerce [lo..hi]

-- | isValid assumes that the list is sorted and unique and then checks if the list is suitable to be a SmoothBasis.
isValid :: (Eq a, Num a) => [a] -> Bool
isValid pl = length pl /= 0 && v' pl
  where
    v' :: (Eq a, Num a) => [a] -> Bool
    v' []     = True
    v' (x:xs) = x /= 0 && abs x /= 1 && abs x == x && v' xs

-- | @isSmooth@ checks if a given number is smooth under a certain @SmoothBasis@.
-- Does not check if the @SmoothBasis@ is valid.
isSmooth :: forall a . E.Euclidean a => SmoothBasis a -> a -> Bool
isSmooth prs x = mf (unSmoothBasis prs) x
  where
    mf :: [a] -> a -> Bool
    mf _         0 = False
    mf []        n = abs n == 1 -- mf means manually factor
    mf pl@(p:ps) n = if E.mod n p == 0
                     then mf pl (E.div n p)
                     else mf ps n