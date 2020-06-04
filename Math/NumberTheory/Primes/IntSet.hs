-- |
-- Module:      Math.NumberTheory.Primes.IntSet
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Math.NumberTheory.Primes.IntSet
  ( PrimeIntSet
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
  , disjoint
  , union
  , unions
  , difference
  , (\\)
  , intersection
  , filter
  , partition
  , split
  , splitMember
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

import Prelude (Eq, Ord, Show, Semigroup, Monoid, Bool, Maybe(..), Foldable, Int, otherwise)
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Data (Data)
import qualified Data.Foldable
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified GHC.Exts (IsList(..))

import Math.NumberTheory.Primes.Types (Prime(..))

newtype PrimeIntSet = PrimeIntSet IntSet
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

-- TODO allow Int in the first argument
delete :: Key -> PrimeIntSet -> PrimeIntSet
delete = coerce IS.delete

-- TODO allow Int in the first argument
member :: Key -> PrimeIntSet -> Bool
member = coerce IS.member

-- TODO allow Int in the first argument
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

-- TODO allow IntSet in any argument
isSubsetOf :: PrimeIntSet -> PrimeIntSet -> Bool
isSubsetOf = coerce IS.isSubsetOf

-- TODO allow IntSet in any argument
isProperSubsetOf :: PrimeIntSet -> PrimeIntSet -> Bool
isProperSubsetOf = coerce IS.isProperSubsetOf

-- TODO allow IntSet in any argument
disjoint :: PrimeIntSet -> PrimeIntSet -> Bool
disjoint = coerce IS.disjoint

union :: PrimeIntSet -> PrimeIntSet -> PrimeIntSet
union = coerce IS.union

unions :: Foldable f => f PrimeIntSet -> PrimeIntSet
unions = Data.Foldable.foldl' union empty

-- TODO allow IntSet in the second argument
difference :: PrimeIntSet -> PrimeIntSet -> PrimeIntSet
difference = coerce IS.difference

-- TODO allow IntSet in the second argument
(\\) :: PrimeIntSet -> PrimeIntSet -> PrimeIntSet
(\\) = coerce (IS.\\)

-- TODO allow IntSet in exactly one argument
intersection :: PrimeIntSet -> PrimeIntSet -> PrimeIntSet
intersection = coerce IS.intersection

filter :: (Key -> Bool) -> PrimeIntSet -> PrimeIntSet
filter = coerce IS.filter

partition :: (Key -> Bool) -> PrimeIntSet -> (PrimeIntSet, PrimeIntSet)
partition = coerce IS.partition

split :: Int -> PrimeIntSet -> (PrimeIntSet, PrimeIntSet)
split = coerce IS.split

-- TODO allow Int in the first argument
splitMember :: Key -> PrimeIntSet -> (PrimeIntSet, Bool, PrimeIntSet)
splitMember = coerce IS.splitMember

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
