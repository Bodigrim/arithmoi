-- |
-- Module:      Math.NumberTheory.Utils.DirichletSeries
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- An abstract representation of a Dirichlet series over a semiring.
--

{-# LANGUAGE ScopedTypeVariables   #-}

module Math.NumberTheory.Utils.DirichletSeries
  ( DirichletSeries
  , fromDistinctAscList
  , last
  , filter
  , timesAndCrop
  ) where

import Prelude hiding (filter, last, rem, quot)
import qualified Data.List as L
import Data.Semiring (Semiring(..))

import Math.NumberTheory.Euclidean

-- Sparse Dirichlet series are represented by an ascending list of pairs.
-- For instance, [(a, b), (c, d)] represents 1 + b/s^a + d/s^c.
newtype DirichletSeries a b = DirichletSeries { unDirichletSeries :: [(a, b)] }
  deriving (Show)

fromDistinctAscList :: [(a, b)] -> DirichletSeries a b
fromDistinctAscList = DirichletSeries

-- TODO: represent Dirichlet series as a descending list of pairs.
-- This will allow to avoid traversal in L.last.
last :: (Eq a, Num a, Semiring b) => a -> DirichletSeries a b -> b
last 1 (DirichletSeries [(1, b)]) = one `plus` b
last 1 _ = one
last _ (DirichletSeries []) = zero
last a' (DirichletSeries xs) = case L.last xs of
  (a, b) -> if a == a' then b else zero

filter :: (a -> Bool) -> DirichletSeries a b -> DirichletSeries a b
filter predicate (DirichletSeries xs) = DirichletSeries $ L.filter (predicate . fst) xs

merge :: (Ord a, Semiring b) => [(a, b)] -> [(a, b)] -> [(a, b)]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x'@(x, fx) : xs) ys'@(y'@(y, fy) : ys) = case x `compare` y of
  LT -> x' : merge xs ys'
  EQ -> (x, fx `plus` fy) : merge xs ys
  GT -> y' : merge xs' ys

-- | Precondition: all pairwise products of keys are distinct.
-- In other words, no element repeats in the list
-- [ a <> b | (a, _) <- as, (b, _) <- bs ]
timesAndCrop
  :: (Euclidean a, Ord a, Semiring b)
  => a
  -> DirichletSeries a b -- ^ longer series
  -> DirichletSeries a b -- ^ shorter series
  -> DirichletSeries a b
timesAndCrop n (DirichletSeries as) (DirichletSeries [(b, fb)])
  = DirichletSeries
  -- TODO: fuse generation of as' and its merge with as
  -- into one function, maintaining two pointers to as.
  -- This will avoid generation of intermediate structures.
  $ [(b, fb)] `merge` (as `merge` as')
  where
    nb = n `quot` b
    as'
      = map (\(a, fa) -> (a * b, fa `times` fb))
      $ L.filter (\(a, _) -> nb `rem` a == 0)
      -- $ L.takeWhile (\(a, _) -> a <= nb)
      $ as
timesAndCrop n (DirichletSeries as) (DirichletSeries bs)
  = DirichletSeries
  $ foldl merge (as `merge` bs)
  $ map
    (\(b, fb) -> L.filter (\(ab, _) -> n `rem` ab == 0) $ map
      (\(a, fa) -> (a * b, fa `times` fb))
      as)
    bs
{-# SPECIALISE timesAndCrop :: Semiring b => (Integer -> Bool) -> DirichletSeries Integer b -> DirichletSeries Integer b -> DirichletSeries Integer b #-}
