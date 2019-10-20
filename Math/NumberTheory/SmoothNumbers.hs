-- |
-- Module:      Math.NumberTheory.SmoothNumbers
-- Copyright:   (c) 2018 Frederick Schneider, 2018-2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Frederick Schneider <frederick.schneider2011@gmail.com>
--
-- A <https://en.wikipedia.org/wiki/Smooth_number smooth number>
-- is an number, which can be represented as a product of powers of elements
-- from a given set (smooth basis). E. g., 48 = 3 * 4 * 4 is smooth
-- over a set {3, 4}, and 24 is not.
--

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Math.NumberTheory.SmoothNumbers
  ( SmoothBasis
  , unSmoothBasis
  , fromList
  , isSmooth
  , smoothOver
  , smoothOver'
  ) where

import Prelude hiding (div, mod, gcd)
import Data.Euclidean
import Data.List (nub)
import Data.Maybe
import Data.Semiring

-- | An abstract representation of a smooth basis.
newtype SmoothBasis a = SmoothBasis
  { unSmoothBasis :: [a]
  -- ^ Unwrap elements of a smooth basis.
  }
  deriving (Show)

-- | Build a 'SmoothBasis' from a list of numbers,
-- sanitizing it from duplicates, zeros and units.
--
-- >>> fromList [2, 3]
-- SmoothBasis {unSmoothBasis = [2,3]}
-- >>> fromList [2, 2]
-- SmoothBasis {unSmoothBasis = [2]}
-- >>> fromList [1, 3]
-- SmoothBasis {unSmoothBasis = [3]}
fromList :: (Eq a, GcdDomain a) => [a] -> SmoothBasis a
fromList
  = SmoothBasis
  . filter (\x -> not (isZero x) && isNothing (one `divide` x))
  . nub

-- | A generalization of 'smoothOver',
-- suitable for non-'Integral' domains.
-- The first argument must be an appropriate
-- <https://en.wikipedia.org/wiki/Ideal_norm Ideal norm> function,
-- like 'Math.NumberTheory.Quadratic.GaussianIntegers.norm'
-- or 'Math.NumberTheory.Quadratic.EisensteinIntegers.norm'.
--
-- This routine is more efficient than filtering with 'isSmooth'.
--
-- >>> import Math.NumberTheory.Quadratic.GaussianIntegers
-- >>> take 10 (smoothOver' norm (fromList [1+ι,3]))
-- [1,1+ι,2,2+2*ι,3,4,3+3*ι,4+4*ι,6,8]
smoothOver'
  :: (Eq a, Num a, Ord b)
  => (a -> b) -- ^ <https://en.wikipedia.org/wiki/Ideal_norm Ideal norm>
  -> SmoothBasis a
  -> [a]
smoothOver' norm (SmoothBasis pl) =
  foldr
  (\p l -> foldr skipHead [] $ iterate (map (abs . (Prelude.* p))) l)
  [1]
  pl
  where
    skipHead []      b = b
    skipHead (h : t) b = h : merge t b

    merge a [] = a
    merge [] b = b
    merge a@(ah : at) b@(bh : bt)
      | norm bh < norm ah = bh : merge a bt
      | ah == bh          = ah : merge at bt
      | otherwise         = ah : merge at b

-- | Generate an infinite ascending list of
-- <https://en.wikipedia.org/wiki/Smooth_number smooth numbers>
-- over a given smooth basis.
--
-- This routine is more efficient than filtering with 'isSmooth'.
--
-- >>> take 10 (smoothOver (fromList [2, 5]))
-- [1,2,4,5,8,10,16,20,25,32]
smoothOver :: Integral a => SmoothBasis a -> [a]
smoothOver = smoothOver' abs

-- | Check that a given number is smooth under a given 'SmoothBasis'.
--
-- >>> isSmooth (fromList [2,3]) 12
-- True
-- >>> isSmooth (fromList [2,3]) 15
-- False
isSmooth :: (Eq a, GcdDomain a) => SmoothBasis a -> a -> Bool
isSmooth prs x = not (isZero x) && go (unSmoothBasis prs) x
  where
    go :: (Eq a, GcdDomain a) => [a] -> a -> Bool
    go [] n = isJust (one `divide` n)
    go pps@(p:ps) n = case n `divide` p of
      Nothing -> go ps n
      Just q  -> go pps q || go ps n
