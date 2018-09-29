-- |
-- Module:      Math.NumberTheory.Utils.Series
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- An abstract representation of a (power or Dirichlet) series over a semiring.
--

{-# LANGUAGE ScopedTypeVariables   #-}

module Math.NumberTheory.Utils.Series
  ( Series
  , PowerSeries
  , DirichletSeries
  , fromDistinctAscList
  , last
  , filter
  , timesAndCrop
  ) where

import Prelude hiding (filter, last)
import qualified Data.List as L
import Data.Semigroup
import Data.Semiring

type PowerSeries a b = Series (Sum a) b
type DirichletSeries a b = Series (Product a) b

newtype Series a b = Series { unSeries :: [(a, b)] }
  deriving (Show)

fromDistinctAscList :: [(a, b)] -> Series a b
fromDistinctAscList = Series

last :: Semiring b => Series a b -> b
last (Series xs) = case xs of
  [] -> zero
  _  -> snd $ L.last xs

filter :: (a -> Bool) -> Series a b -> Series a b
filter predicate (Series xs) = Series $ L.filter (predicate . fst) xs

instance (Ord a, Monoid a, Semiring b) => Semiring (Series a b) where
  zero  = Series mempty
  one   = Series [(mempty, one)]
  plus  = add
  times = timesAndCrop (const True)

add
  :: (Ord a, Semiring b)
  => Series a b
  -> Series a b
  -> Series a b
add (Series as) (Series bs) = Series $ merge as bs

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
  :: (Ord a, Monoid a, Semiring b)
  => (a -> Bool)
  -> Series a b -- ^ longer series
  -> Series a b -- ^ shorter series
  -> Series a b
timesAndCrop predicate (Series as) (Series bs)
  = foldl add zero
  $ map
    (\(b, fb) -> Series $ L.filter (predicate . fst) $ map
      (\(a, fa) -> (a `mappend` b, fa `times` fb))
      as)
    bs
