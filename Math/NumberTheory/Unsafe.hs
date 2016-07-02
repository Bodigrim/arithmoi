-- |
-- Module:      Math.NumberTheory.Unsafe
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Layer to switch between safe and unsafe operations, including arrays and type casts.
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

  , wordToInt
  , wordToInteger
  , intToWord
  , intToInteger
  , naturalToInteger
  , integerToNatural
  , integerToWord
  ) where

import Numeric.Natural

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
#if !MIN_VERSION_array(0,5,0)
  hiding (unsafeFreeze, unsafeThaw)
#endif

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

wordToInt :: Word -> Int
wordToInt = fromIntegral

wordToInteger :: Word -> Integer
wordToInteger = fromIntegral

intToWord :: Int -> Word
intToWord = fromIntegral

intToInteger :: Int -> Integer
intToInteger = fromIntegral

naturalToInteger :: Natural -> Integer
naturalToInteger = fromIntegral

integerToNatural :: Integer -> Natural
integerToNatural = fromIntegral

integerToWord :: Integer -> Word
integerToWord = fromIntegral

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

import Unsafe.Coerce

wordToInt :: Word -> Int
wordToInt = unsafeCoerce

wordToInteger :: Word -> Integer
wordToInteger = unsafeCoerce

intToWord :: Int -> Word
intToWord = unsafeCoerce

intToInteger :: Int -> Integer
intToInteger = unsafeCoerce

naturalToInteger :: Natural -> Integer
naturalToInteger = unsafeCoerce

integerToNatural :: Integer -> Natural
integerToNatural = unsafeCoerce

integerToWord :: Integer -> Word
integerToWord = unsafeCoerce

#endif
