-- |
-- Module:      Math.NumberTheory.Utils.FromIntegral
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Monomorphic `fromIntegral`.
--

{-# LANGUAGE CPP #-}

module Math.NumberTheory.Utils.FromIntegral
  ( wordToInt
  , wordToInteger
  , intToWord
  , intToInt8
  , intToInt64
  , int8ToInt64
  , intToWord8
  , intToWord64
  , int8ToInt
  , int64ToInt
  , word8ToInt
  , word64ToInt
  , intToInteger
  , int16ToInteger
  , int64ToInteger
  , word64ToInteger
  , naturalToInteger
  , integerToNatural
  , integerToWord
  , integerToWord64
  , integerToInt
  , integerToInt64
  , intToNatural
  , naturalToInt
  , intToDouble
  , fromIntegral'
  ) where

import Data.Int
import Data.Word
import Numeric.Natural

wordToInt :: Word -> Int
wordToInt = fromIntegral
{-# INLINE wordToInt #-}

wordToInteger :: Word -> Integer
wordToInteger = fromIntegral
{-# INLINE wordToInteger #-}

intToWord :: Int -> Word
intToWord = fromIntegral
{-# INLINE intToWord #-}

intToInt8 :: Int -> Int8
intToInt8 = fromIntegral
{-# INLINE intToInt8 #-}

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral
{-# INLINE intToInt64 #-}

int8ToInt64 :: Int8 -> Int64
int8ToInt64 = fromIntegral
{-# INLINE int8ToInt64 #-}

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral
{-# INLINE intToWord8 #-}

intToWord64 :: Int -> Word64
intToWord64 = fromIntegral
{-# INLINE intToWord64 #-}

int8ToInt :: Int8 -> Int
int8ToInt = fromIntegral
{-# INLINE int8ToInt #-}

int64ToInt :: Int64 -> Int
int64ToInt = fromIntegral
{-# INLINE int64ToInt #-}

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
{-# INLINE word8ToInt #-}

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral
{-# INLINE word64ToInt #-}

intToInteger :: Int -> Integer
intToInteger = fromIntegral
{-# INLINE intToInteger #-}

int16ToInteger :: Int16 -> Integer
int16ToInteger = fromIntegral
{-# INLINE int16ToInteger #-}

int64ToInteger :: Int64 -> Integer
int64ToInteger = fromIntegral
{-# INLINE int64ToInteger #-}

word64ToInteger :: Word64 -> Integer
word64ToInteger = fromIntegral
{-# INLINE word64ToInteger #-}

naturalToInteger :: Natural -> Integer
naturalToInteger = fromIntegral
{-# INLINE naturalToInteger #-}

integerToNatural :: Integer -> Natural
integerToNatural = fromIntegral'
{-# INLINE integerToNatural #-}

integerToWord :: Integer -> Word
integerToWord = fromIntegral
{-# INLINE integerToWord #-}

integerToWord64 :: Integer -> Word64
integerToWord64 = fromIntegral
{-# INLINE integerToWord64 #-}

integerToInt :: Integer -> Int
integerToInt = fromIntegral
{-# INLINE integerToInt #-}

integerToInt64 :: Integer -> Int64
integerToInt64 = fromIntegral
{-# INLINE integerToInt64 #-}

intToNatural :: Int -> Natural
intToNatural = fromIntegral
{-# INLINE intToNatural #-}

naturalToInt :: Natural -> Int
naturalToInt = fromIntegral
{-# INLINE naturalToInt #-}

intToDouble :: Int -> Double
intToDouble = fromIntegral
{-# INLINE intToDouble #-}

fromIntegral' :: (Integral a, Num b) => a -> b
#if __GLASGOW_HASKELL__ == 900 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1
-- Cannot use fromIntegral because of https://gitlab.haskell.org/ghc/ghc/-/issues/19411
fromIntegral' = fromInteger . toInteger
#else
fromIntegral' = fromIntegral
#endif
{-# INLINE fromIntegral' #-}
