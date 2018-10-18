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
  , fromList
  ) where

import Data.Vector (Vector, fromList)
import Data.Vector.Mutable (STVector, replicate)
import Prelude hiding (read, replicate)
#ifdef CheckBounds
import Control.Monad.ST (ST)
import Data.Vector ((!), freeze, thaw)
import Data.Vector.Mutable (new, read, write)

unsafeIndex :: Vector a -> Int -> a
unsafeIndex = (!)

unsafeFreeze :: STVector s a -> ST s (Vector a)
unsafeFreeze = freeze

unsafeNew :: Int -> ST s (STVector s a)
unsafeNew = new

unsafeRead :: STVector s a -> Int -> ST s a
unsafeRead = read

unsafeThaw :: Vector a -> ST s (STVector s a)
unsafeThaw = thaw

unsafeWrite :: STVector s a -> Int -> a -> ST s ()
unsafeWrite = write
#else
import Data.Vector (unsafeFreeze, unsafeIndex, unsafeThaw)
import Data.Vector.Mutable (unsafeNew, unsafeRead, unsafeWrite)
#endif
