-- |
-- Module:      Math.NumberTheory.Unsafe
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Layer to switch between safe and unsafe arrays.
--
{-# LANGUAGE CPP #-}

module Math.NumberTheory.Unsafe
  ( STVector
  , Vector
  , unsafeIndex
  , unsafeFreeze
  , unsafeNew
  , unsafeRead
  , unsafeThaw
  , unsafeWrite
  , replicate
  , length
  , fromList
  ) where

import Data.Vector (Vector, fromList)
import Data.Vector.Mutable (STVector, replicate)
import Prelude hiding (replicate)
#ifdef CheckBounds
import Data.Vector

unsafeIndex :: Vector a -> Int -> a
unsafeIndex = (!)

unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
unsafeFreeze = freeze

unsafeNew :: PrimMonad m => Int -> m (MVector (PrimState m) a)
unsafeNew = new

unsafeRead :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
unsafeRead = read

unsafeThaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
unsafeThaw = thaw

unsafeWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite = write
#else
import Data.Vector (unsafeFreeze, unsafeIndex, unsafeThaw)
import Data.Vector.Mutable (unsafeNew, unsafeRead, unsafeWrite)
#endif
