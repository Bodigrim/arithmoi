-- |
-- Module:      Math.NumberTheory.Utils.DirichletSeries
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- An abstract representation of a Dirichlet series over a semiring.
--

{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Math.NumberTheory.Utils.DirichletSeries
  ( DirichletSeries
  , fromDistinctAscList
  , last
  , filter
  , partition
  , unions
  , union
  , size
  , timesAndCrop
  ) where

import Prelude hiding (filter, last, rem, quot, snd)
import Data.Coerce
import Data.Semiring (Semiring(..))
import Numeric.Natural

import Math.NumberTheory.Euclidean

import Data.Map (Map)
import qualified Data.Map.Strict as M

-- Sparse Dirichlet series are represented by an ascending list of pairs.
-- For instance, [(a, b), (c, d)] represents 1 + b/s^a + d/s^c.
newtype DirichletSeries a b = DirichletSeries { unDirichletSeries :: Map a b }
  deriving (Show)

fromDistinctAscList :: forall a b. [(a, b)] -> DirichletSeries a b
fromDistinctAscList = coerce (M.fromDistinctAscList @a @b)

last :: (Eq a, Num a, Semiring b) => a -> DirichletSeries a b -> b
last 1 (DirichletSeries m)
  | [(1, b)] <- M.assocs m = one `plus` b
  | otherwise = one
last a' (DirichletSeries xs) = case M.maxViewWithKey xs of
  Nothing -> zero
  Just ((a, b), _) -> if a == a' then b else zero

filter :: forall a b. (a -> Bool) -> DirichletSeries a b -> DirichletSeries a b
filter predicate = coerce (M.filterWithKey @a @b (\k _ -> predicate k))

partition :: forall a b. (a -> Bool) -> DirichletSeries a b -> (DirichletSeries a b, DirichletSeries a b)
partition predicate = coerce (M.partitionWithKey @a @b (\k _ -> predicate k))

unions :: forall a b. (Ord a, Semiring b) => [DirichletSeries a b] -> DirichletSeries a b
unions = coerce (M.unionsWith plus :: [Map a b] -> Map a b)

union :: forall a b. (Ord a, Semiring b) => DirichletSeries a b -> DirichletSeries a b -> DirichletSeries a b
union = coerce (M.unionWith @a @b plus)

merge :: (Ord a, Semiring b) => Map a b -> Map a b -> Map a b
merge = M.unionWith plus

size :: forall a b. DirichletSeries a b -> Int
size = coerce (M.size @a @b)

-- | Precondition: all pairwise products of keys are distinct.
-- In other words, no element repeats in the list
-- [ a <> b | (a, _) <- as, (b, _) <- bs ]
timesAndCrop
  :: (Euclidean a, Ord a, Semiring b)
  => a
  -> DirichletSeries a b -- ^ longer series
  -> DirichletSeries a b -- ^ shorter series
  -> DirichletSeries a b
timesAndCrop n (DirichletSeries as) (DirichletSeries bs)
  = DirichletSeries
  $ merge (as `merge` bs)
  $ M.fromListWith plus
  [ (a * b, fa `times` fb)
  | (b, fb) <- M.assocs bs
  , let nb = n `quot` b
  , (a, fa) <- takeWhile ((<= nb) . fst) (M.assocs as)
  , nb `rem` a == 0
  ]
{-# SPECIALISE timesAndCrop :: Semiring b => Int -> DirichletSeries Int b -> DirichletSeries Int b -> DirichletSeries Int b #-}
{-# SPECIALISE timesAndCrop :: Semiring b => Word -> DirichletSeries Word b -> DirichletSeries Word b -> DirichletSeries Word b #-}
{-# SPECIALISE timesAndCrop :: Semiring b => Integer -> DirichletSeries Integer b -> DirichletSeries Integer b -> DirichletSeries Integer b #-}
{-# SPECIALISE timesAndCrop :: Semiring b => Natural -> DirichletSeries Natural b -> DirichletSeries Natural b -> DirichletSeries Natural b #-}
