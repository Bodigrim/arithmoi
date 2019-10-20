-- |
-- Module:      Math.NumberTheory.SmoothNumbers
-- Copyright:   (c) 2018 Frederick Schneider
-- Licence:     MIT
-- Maintainer:  Frederick Schneider <frederick.schneider2011@gmail.com>
--
-- A <https://en.wikipedia.org/wiki/Smooth_number smooth number>
-- is an integer, which can be represented as a product of powers of elements
-- from a given set (smooth basis). E. g., 48 = 3 * 4 * 4 is smooth
-- over a set {3, 4}, and 24 is not.
--

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Math.NumberTheory.SmoothNumbers
  ( -- * Create a smooth basis
    SmoothBasis
  , fromList
    -- * Generate smooth numbers
  , smoothOver
  , smoothOver'
  -- * Verify if a number is smooth
  , isSmooth
  ) where

import Prelude hiding (div, mod, gcd)
import Data.List (nub)
import Data.Semiring (isZero)

import qualified Math.NumberTheory.Euclidean as E

-- | An abstract representation of a smooth basis.
-- It consists of a set of numbers ≥2.
newtype SmoothBasis a = SmoothBasis { unSmoothBasis :: [a] } deriving (Eq, Show)

-- | Build a 'SmoothBasis' from a list of numbers ≥2.
--
-- >>> fromList [2, 3]
-- Just (SmoothBasis {unSmoothBasis = [2,3]})
-- >>> fromList [2, 2]
-- Just (SmoothBasis {unSmoothBasis = [2]})
-- >>> fromList [2, 4]
-- Just (SmoothBasis {unSmoothBasis = [2,4]})
-- >>> fromList [1, 3] -- should be >= 2
-- Nothing
fromList :: (Eq a, E.GcdDomain a) => [a] -> Maybe (SmoothBasis a)
fromList l = if isValid l' then Just (SmoothBasis l') else Nothing
  where
    l' = nub l

-- | Helper used by @smoothOver@ (@Integral@ constraint) and @smoothOver'@
-- (@Euclidean@ constraint) Since the typeclass constraint is just
-- @Num@, it receives a @norm@ comparison function for the generated smooth
-- numbers.
-- This function relies on the fact that for any element of a smooth basis @p@
-- and any @a@ it is true that @norm (a * p) > norm a@.
-- This condition is not checked.
smoothOver'
  :: forall a b. (Eq a, Num a, Ord b)
  => (a -> b)
  -> SmoothBasis a
  -> [a]
smoothOver' norm pl =
    foldr
    (\p l -> mergeListLists $ iterate (map (* p)) l)
    [1]
    (nub $ unSmoothBasis pl)
  where
    {-# INLINE mergeListLists #-}
    mergeListLists :: [[a]] -> [a]
    mergeListLists = foldr go1 []
      where
        go1 :: [a] -> [a] -> [a]
        go1 []    b = b
        go1 (h:t) b = h:(go2 t b)

        go2 :: [a] -> [a] -> [a]
        go2 a [] = a
        go2 [] b = b
        go2 a@(ah:at) b@(bh:bt)
          | norm bh < norm ah = bh : (go2 a bt)
          | abs ah == abs bh  = ah : (go2 at bt)
          | otherwise         = ah : (go2 at b)

-- | Generate an infinite ascending list of
-- <https://en.wikipedia.org/wiki/Smooth_number smooth numbers>
-- over a given smooth basis.
--
-- >>> import Data.Maybe
-- >>> take 10 (smoothOver (fromJust (fromList [2, 5])))
-- [1,2,4,5,8,10,16,20,25,32]
smoothOver :: (Ord a, Num a) => SmoothBasis a -> [a]
smoothOver = smoothOver' abs

isValid :: (Eq a, E.GcdDomain a) => [a] -> Bool
isValid [] = False
isValid xs = all (\x -> not (isZero x) && not (E.isUnit x)) xs

-- | @isSmooth@ checks if a given number is smooth under a certain @SmoothBasis@.
-- Does not check if the @SmoothBasis@ is valid.
isSmooth :: (Eq a, E.GcdDomain a) => SmoothBasis a -> a -> Bool
isSmooth prs x = not (isZero x) && go (unSmoothBasis prs) x
  where
    go :: (Eq a, E.GcdDomain a) => [a] -> a -> Bool
    go [] n = E.isUnit n
    go pps@(p:ps) n = case n `E.divide` p of
      Nothing -> go ps n
      Just q  -> go pps q || go ps n
