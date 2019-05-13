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
  , lookup
  , filter
  , partition
  , unions
  , union
  , size
  , timesAndCrop
  ) where

import Prelude hiding (filter, last, rem, quot, snd, lookup)
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Semiring (Semiring(..))
import Numeric.Natural

import Math.NumberTheory.Euclidean

-- Sparse Dirichlet series are represented by an ascending list of pairs.
-- For instance, [(a, b), (c, d)] stands for 1 + b/a^s + d/c^s.
-- Note that the representation still may include a term (1, b), so
-- [(1, b), (c, d)] means (1 + b) + d/c^s.
newtype DirichletSeries a b = DirichletSeries { _unDirichletSeries :: Map a b }
  deriving (Show)

fromDistinctAscList :: forall a b. [(a, b)] -> DirichletSeries a b
fromDistinctAscList = coerce (M.fromDistinctAscList @a @b)

lookup :: (Ord a, Num a, Semiring b) => a -> DirichletSeries a b -> b
lookup 1 (DirichletSeries m) = M.findWithDefault zero 1 m `plus` one
lookup a (DirichletSeries m) = M.findWithDefault zero a m

filter :: forall a b. (a -> Bool) -> DirichletSeries a b -> DirichletSeries a b
filter predicate = coerce (M.filterWithKey @a @b (\k _ -> predicate k))

partition :: forall a b. (a -> Bool) -> DirichletSeries a b -> (DirichletSeries a b, DirichletSeries a b)
partition predicate = coerce (M.partitionWithKey @a @b (\k _ -> predicate k))

unions :: forall a b. (Ord a, Semiring b) => [DirichletSeries a b] -> DirichletSeries a b
unions = coerce (M.unionsWith plus :: [Map a b] -> Map a b)

union :: forall a b. (Ord a, Semiring b) => DirichletSeries a b -> DirichletSeries a b -> DirichletSeries a b
union = coerce (M.unionWith @a @b plus)

size :: forall a b. DirichletSeries a b -> Int
size = coerce (M.size @a @b)

-- | Let as = sum_i k_i/a_i^s and bs = sum_i l_i/b_i^s be Dirichlet series,
-- and all a_i and b_i are divisors of n. Return Dirichlet series cs,
-- which contains all terms as * bs = sum_i m_i/c_i^s such that c_i divides n.
timesAndCrop
  :: (Num a, Euclidean a, Ord a, Semiring b)
  => a
  -> DirichletSeries a b
  -> DirichletSeries a b
  -> DirichletSeries a b
timesAndCrop n (DirichletSeries as) (DirichletSeries bs)
  = DirichletSeries
  $ M.unionWith plus (M.unionWith plus as bs)
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
