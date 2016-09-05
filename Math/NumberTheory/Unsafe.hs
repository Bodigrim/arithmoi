-- |
-- Module:      Math.NumberTheory.Unsafe
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Layer to switch between safe and unsafe arrays.
--

{-# LANGUAGE CPP #-}

module Math.NumberTheory.Unsafe
  ( UArray
  , bounds
  , castSTUArray
  , unsafeAt
  , unsafeFreeze
  , unsafeNewArray_
  , unsafeRead
  , unsafeThaw
  , unsafeWrite
  ) where

#ifdef CheckBounds

import Data.Array.Base
  ( UArray
  , castSTUArray
  )
import Data.Array.IArray
  ( IArray
  , bounds
  , (!)
  )
import Data.Array.MArray

unsafeAt :: (IArray a e, Ix i) => a i e -> i -> e
unsafeAt = (!)

unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = freeze

unsafeNewArray_ :: (Ix i, MArray a e m) => (i, i) -> m (a i e)
unsafeNewArray_ = newArray_

unsafeRead :: (MArray a e m, Ix i) => a i e -> i -> m e
unsafeRead = readArray

unsafeThaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
unsafeThaw = thaw

unsafeWrite :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
unsafeWrite = writeArray

#else

import Data.Array.Base
  ( UArray
  , bounds
  , castSTUArray
  , unsafeAt
  , unsafeFreeze
  , unsafeNewArray_
  , unsafeRead
  , unsafeThaw
  , unsafeWrite
  )

#endif
