-- |
-- Module:      Math.NumberTheory.Primes.IntSet
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- A newtype wrapper around 'IntSet'.
--
-- This module is intended to be imported qualified, e. g.,
--
-- > import Math.NumberTheory.Primes.IntSet (PrimeIntSet)
-- > import qualified Math.NumberTheory.Primes.IntSet as PrimeIntSet
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Math.NumberTheory.Primes.IntSet
  ( -- * Set type
    PrimeIntSet
  , unPrimeIntSet
  -- * Construction
  -- | Use 'Data.Monoid.mempty' to create an empty set.
  , singleton
  , fromList
  , fromAscList
  , fromDistinctAscList
  -- * Insertion
  , insert
  -- * Deletion
  , delete
  -- * Query
  , member
  , notMember
  , lookupEQ
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  , null
  , size
  , isSubsetOf
  , isProperSubsetOf
#if MIN_VERSION_containers(0,5,11)
  , disjoint
#endif
  -- * Combine
  -- | Use 'Data.Semigroup.<>' for unions.
  , difference
  , (\\)
  , intersection
  -- * Filter
  , filter
  , partition
  , split
  , splitMember
  , splitLookupEQ
  , splitRoot
  -- * Folds
  , foldr
  , foldl
  , foldr'
  , foldl'
  -- * Min/Max
  , deleteMin
  , deleteMax
  , minView
  , maxView
  -- * Conversion
  , toAscList
  , toDescList
  ) where

import Prelude (Eq, Ord, Show, Monoid, Bool, Maybe(..), Int, otherwise)
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Semigroup (Semigroup)
import qualified GHC.Exts (IsList(..))

import Math.NumberTheory.Primes.Types (Prime(..))

-- | A set of 'Prime' integers.
newtype PrimeIntSet = PrimeIntSet {
  -- | Convert to a set of integers.
  unPrimeIntSet :: IntSet
  }
  deriving (Eq, Ord, Data, Show, Semigroup, Monoid, NFData)

instance GHC.Exts.IsList PrimeIntSet where
  type Item PrimeIntSet = Prime Int
  fromList = coerce IS.fromList
  toList = coerce IS.toList

-- | Build a singleton set.
singleton :: Prime Int -> PrimeIntSet
singleton = coerce IS.singleton

-- | Build a set from a list of primes.
fromList :: [Prime Int] -> PrimeIntSet
fromList = coerce IS.fromList

-- | Build a set from an ascending list of primes
-- (the precondition is not checked).
fromAscList :: [Prime Int] -> PrimeIntSet
fromAscList = coerce IS.fromAscList

-- | Build a set from an ascending list of distinct primes
-- (the precondition is not checked).
fromDistinctAscList :: [Prime Int] -> PrimeIntSet
fromDistinctAscList = coerce IS.fromDistinctAscList

-- | Insert a prime into the set.
insert :: Prime Int -> PrimeIntSet -> PrimeIntSet
insert = coerce IS.insert

-- | Delete an integer from the set.
delete :: Int -> PrimeIntSet -> PrimeIntSet
delete = coerce IS.delete

-- | Check whether the given prime is a member of the set.
member :: Prime Int -> PrimeIntSet -> Bool
member = coerce IS.member

-- | Check whether the given prime is not a member of the set.
notMember :: Prime Int -> PrimeIntSet -> Bool
notMember = coerce IS.notMember

-- | Find a prime in the set,
-- equal to the given integer, if any exists.
lookupEQ :: Int -> PrimeIntSet -> Maybe (Prime Int)
lookupEQ x xs
  | coerce member x xs = Just (Prime x)
  | otherwise          = Nothing

-- | Find the largest prime in the set,
-- smaller than the given integer, if any exists.
lookupLT :: Int -> PrimeIntSet -> Maybe (Prime Int)
lookupLT = coerce IS.lookupLT

-- | Find the smallest prime in the set,
-- greater than the given integer, if any exists.
lookupGT :: Int -> PrimeIntSet -> Maybe (Prime Int)
lookupGT = coerce IS.lookupGT

-- | Find the largest prime in the set,
-- smaller or equal to the given integer, if any exists.
lookupLE :: Int -> PrimeIntSet -> Maybe (Prime Int)
lookupLE = coerce IS.lookupLE

-- | Find the smallest prime in the set,
-- greater or equal to the given integer, if any exists.
lookupGE :: Int -> PrimeIntSet -> Maybe (Prime Int)
lookupGE = coerce IS.lookupGE

-- | Check whether the set is empty.
null :: PrimeIntSet -> Bool
null = coerce IS.null

-- | Cardinality of the set.
size :: PrimeIntSet -> Int
size = coerce IS.size

-- | Check whether the first argument is a subset of the second one.
isSubsetOf :: PrimeIntSet -> PrimeIntSet -> Bool
isSubsetOf = coerce IS.isSubsetOf

-- | Check whether the first argument is a proper subset of the second one.
isProperSubsetOf :: PrimeIntSet -> PrimeIntSet -> Bool
isProperSubsetOf = coerce IS.isProperSubsetOf

#if MIN_VERSION_containers(0,5,11)
-- | Check whether two sets are disjoint.
disjoint :: PrimeIntSet -> PrimeIntSet -> Bool
disjoint = coerce IS.disjoint
#endif

-- | Difference between a set of primes and a set of integers.
difference :: PrimeIntSet -> IntSet -> PrimeIntSet
difference = coerce IS.difference

-- | An alias to 'difference'.
(\\) :: PrimeIntSet -> IntSet -> PrimeIntSet
(\\) = coerce (IS.\\)

infixl 9 \\{- -}

-- | Intersection of a set of primes and a set of integers.
intersection :: PrimeIntSet -> IntSet -> PrimeIntSet
intersection = coerce IS.intersection

-- | Filter primes satisfying a predicate.
filter :: (Prime Int -> Bool) -> PrimeIntSet -> PrimeIntSet
filter = coerce IS.filter

-- | Partition primes according to a predicate.
partition :: (Prime Int -> Bool) -> PrimeIntSet -> (PrimeIntSet, PrimeIntSet)
partition = coerce IS.partition

-- | Split into primes strictly less and strictly greater
-- than the first argument.
split :: Int -> PrimeIntSet -> (PrimeIntSet, PrimeIntSet)
split = coerce IS.split

-- | Simultaneous 'split' and 'member'.
splitMember :: Prime Int -> PrimeIntSet -> (PrimeIntSet, Bool, PrimeIntSet)
splitMember = coerce IS.splitMember

-- | Simultaneous 'split' and 'lookupEQ'.
splitLookupEQ :: Int -> PrimeIntSet -> (PrimeIntSet, Maybe (Prime Int), PrimeIntSet)
splitLookupEQ x xs = (lt, if eq then Just (Prime x) else Nothing, gt)
  where
    (lt, eq, gt) = coerce IS.splitMember x xs

-- | Decompose a set into pieces based on the structure of the underlying tree.
splitRoot :: PrimeIntSet -> [PrimeIntSet]
splitRoot = coerce IS.splitRoot

-- | Fold a set using the given right-associative operator.
foldr :: forall b. (Prime Int -> b -> b) -> b -> PrimeIntSet -> b
foldr = coerce (IS.foldr @b)

-- | Fold a set using the given left-associative operator.
foldl :: forall a. (a -> Prime Int -> a) -> a -> PrimeIntSet -> a
foldl = coerce (IS.foldl @a)

-- | A strict version of 'foldr'.
foldr' :: forall b. (Prime Int -> b -> b) -> b -> PrimeIntSet -> b
foldr' = coerce (IS.foldr' @b)

-- | A strict version of 'foldl'.
foldl' :: forall a. (a -> Prime Int -> a) -> a -> PrimeIntSet -> a
foldl' = coerce (IS.foldl' @a)

-- | Delete the smallest prime in the set.
deleteMin :: PrimeIntSet -> PrimeIntSet
deleteMin = coerce IS.deleteMin

-- | Delete the largest prime in the set.
deleteMax :: PrimeIntSet -> PrimeIntSet
deleteMax = coerce IS.deleteMax

-- | Split a set into the smallest prime and the rest, if non-empty.
minView :: PrimeIntSet -> Maybe (Prime Int, PrimeIntSet)
minView = coerce IS.minView

-- | Split a set into the largest prime and the rest, if non-empty.
maxView :: PrimeIntSet -> Maybe (Prime Int, PrimeIntSet)
maxView = coerce IS.maxView

-- | Convert the set to a list of ascending primes.
toAscList :: PrimeIntSet -> [Prime Int]
toAscList = coerce IS.toAscList

-- | Convert the set to a list of descending primes.
toDescList :: PrimeIntSet -> [Prime Int]
toDescList = coerce IS.toDescList
