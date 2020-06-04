-- |
-- Module:      Math.NumberTheory.Primes.IntSet
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Math.NumberTheory.Primes.IntSet
  ( PrimeIntSet
  , unPrimeIntSet
  , Key
  , empty
  , singleton
  , fromList
  , fromAscList
  , fromDistinctAscList
  , insert
  , delete
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
  , union
  , unions
  , difference
  , (\\)
  , intersection
  , filter
  , partition
  , split
  , splitMember
  , splitLookupEQ
  , splitRoot
  , foldr
  , foldl
  , foldr'
  , foldl'
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , maxView
  , minView
  , elems
  , toList
  , toAscList
  , toDescList
  ) where

import Prelude (Eq, Ord, Show, Monoid, Bool, Maybe(..), Foldable, Int, otherwise)
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Data (Data)
import qualified Data.Foldable
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Semigroup (Semigroup)
import qualified GHC.Exts (IsList(..))

import Math.NumberTheory.Primes.Types (Prime(..))

newtype PrimeIntSet = PrimeIntSet { unPrimeIntSet :: IntSet }
  deriving (Eq, Ord, Data, Show, Semigroup, Monoid, NFData)

type Key = Prime Int

instance GHC.Exts.IsList PrimeIntSet where
  type Item PrimeIntSet = Key
  fromList = fromList
  toList = toList

empty :: PrimeIntSet
empty = coerce IS.empty

singleton :: Key -> PrimeIntSet
singleton = coerce IS.singleton

fromList :: [Key] -> PrimeIntSet
fromList = coerce IS.fromList

fromAscList :: [Key] -> PrimeIntSet
fromAscList = coerce IS.fromAscList

fromDistinctAscList :: [Key] -> PrimeIntSet
fromDistinctAscList = coerce IS.fromDistinctAscList

insert :: Key -> PrimeIntSet -> PrimeIntSet
insert = coerce IS.insert

delete :: Int -> PrimeIntSet -> PrimeIntSet
delete = coerce IS.delete

member :: Key -> PrimeIntSet -> Bool
member = coerce IS.member

notMember :: Key -> PrimeIntSet -> Bool
notMember = coerce IS.notMember

lookupEQ :: Int -> PrimeIntSet -> Maybe Key
lookupEQ x xs
  | coerce member x xs = Just (Prime x)
  | otherwise          = Nothing

lookupLT :: Int -> PrimeIntSet -> Maybe Key
lookupLT = coerce IS.lookupLT

lookupGT :: Int -> PrimeIntSet -> Maybe Key
lookupGT = coerce IS.lookupGT

lookupLE :: Int -> PrimeIntSet -> Maybe Key
lookupLE = coerce IS.lookupLE

lookupGE :: Int -> PrimeIntSet -> Maybe Key
lookupGE = coerce IS.lookupGE

null :: PrimeIntSet -> Bool
null = coerce IS.null

size :: PrimeIntSet -> Int
size = coerce IS.size

isSubsetOf :: PrimeIntSet -> PrimeIntSet -> Bool
isSubsetOf = coerce IS.isSubsetOf

isProperSubsetOf :: PrimeIntSet -> PrimeIntSet -> Bool
isProperSubsetOf = coerce IS.isProperSubsetOf

#if MIN_VERSION_containers(0,5,11)
disjoint :: PrimeIntSet -> PrimeIntSet -> Bool
disjoint = coerce IS.disjoint
#endif

union :: PrimeIntSet -> PrimeIntSet -> PrimeIntSet
union = coerce IS.union

unions :: Foldable f => f PrimeIntSet -> PrimeIntSet
unions = Data.Foldable.foldl' union empty

difference :: PrimeIntSet -> IntSet -> PrimeIntSet
difference = coerce IS.difference

(\\) :: PrimeIntSet -> IntSet -> PrimeIntSet
(\\) = coerce (IS.\\)

intersection :: PrimeIntSet -> IntSet -> PrimeIntSet
intersection = coerce IS.intersection

filter :: (Key -> Bool) -> PrimeIntSet -> PrimeIntSet
filter = coerce IS.filter

partition :: (Key -> Bool) -> PrimeIntSet -> (PrimeIntSet, PrimeIntSet)
partition = coerce IS.partition

split :: Int -> PrimeIntSet -> (PrimeIntSet, PrimeIntSet)
split = coerce IS.split

splitMember :: Key -> PrimeIntSet -> (PrimeIntSet, Bool, PrimeIntSet)
splitMember = coerce IS.splitMember

splitLookupEQ :: Int -> PrimeIntSet -> (PrimeIntSet, Maybe Key, PrimeIntSet)
splitLookupEQ x xs = (lt, if eq then Just (Prime x) else Nothing, gt)
  where
    (lt, eq, gt) = coerce IS.splitMember x xs

splitRoot :: PrimeIntSet -> [PrimeIntSet]
splitRoot = coerce IS.splitRoot

foldr :: forall b. (Key -> b -> b) -> b -> PrimeIntSet -> b
foldr = coerce (IS.foldr @b)

foldl :: forall a. (a -> Key -> a) -> a -> PrimeIntSet -> a
foldl = coerce (IS.foldl @a)

foldr' :: forall b. (Key -> b -> b) -> b -> PrimeIntSet -> b
foldr' = coerce (IS.foldr' @b)

foldl' :: forall a. (a -> Key -> a) -> a -> PrimeIntSet -> a
foldl' = coerce (IS.foldl' @a)

findMin :: PrimeIntSet -> Key
findMin = coerce IS.findMin

findMax :: PrimeIntSet -> Key
findMax = coerce IS.findMax

deleteMin :: PrimeIntSet -> PrimeIntSet
deleteMin = coerce IS.deleteMin

deleteMax :: PrimeIntSet -> PrimeIntSet
deleteMax = coerce IS.deleteMax

deleteFindMin :: PrimeIntSet -> (Key, PrimeIntSet)
deleteFindMin = coerce IS.deleteFindMin

deleteFindMax :: PrimeIntSet -> (Key, PrimeIntSet)
deleteFindMax = coerce IS.deleteFindMax

maxView :: PrimeIntSet -> Maybe (Key, PrimeIntSet)
maxView = coerce IS.maxView

minView :: PrimeIntSet -> Maybe (Key, PrimeIntSet)
minView = coerce IS.minView

elems :: PrimeIntSet -> [Key]
elems = coerce IS.elems

toList :: PrimeIntSet -> [Key]
toList = coerce IS.toList

toAscList :: PrimeIntSet -> [Key]
toAscList = coerce IS.toAscList

toDescList :: PrimeIntSet -> [Key]
toDescList = coerce IS.toDescList
