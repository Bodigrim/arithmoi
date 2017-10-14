-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.SieveBlock.Unboxed
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Bulk evaluation of arithmetic functions without factorisation
-- of arguments.
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Math.NumberTheory.ArithmeticFunctions.SieveBlock.Unboxed
  ( SieveBlockConfig(..)
  , sieveBlockUnboxed
  ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Exts

import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Utils (splitOff#)
import Math.NumberTheory.Utils.FromIntegral

data SieveBlockConfig a = SieveBlockConfig
  { sbcEmpty                :: a
  , sbcAppend               :: a -> a -> a
  , sbcFunctionOnPrimePower :: Word -> Word -> a
  , sbcBlockLowBound        :: Word
  , sbcBlockLength          :: Word
  }

sieveBlockUnboxed
  :: V.Unbox a
  => SieveBlockConfig a
  -> V.Vector a
sieveBlockUnboxed (SieveBlockConfig empty append f lowIndex' len') = runST $ do

    let lowIndex :: Int
        lowIndex = wordToInt lowIndex'

        len :: Int
        len = wordToInt len'

    as <- V.unsafeThaw $ V.enumFromN lowIndex' len
    bs <- MV.replicate len empty

    let highIndex :: Int
        highIndex = lowIndex + len - 1

        ps :: [Int]
        ps = takeWhile (<= integerSquareRoot highIndex) $ map fromInteger primes

        logToIx :: Double
        logToIx = log (fromIntegral highIndex)

    forM_ ps $ \p -> do

      let p# :: Word#
          !p'@(W# p#) = intToWord p

          fs = V.generate
            (floor (logToIx / log (fromIntegral p) + 0.001))
            (\k -> f p' (intToWord k + 1))

          offset :: Int
          offset = negate lowIndex `mod` p

      forM_ [offset, offset + p .. len - 1] $ \ix -> do
        W# a# <- MV.unsafeRead as ix
        let !(# pow#, a'# #) = splitOff# p# (a# `quotWord#` p#)
        MV.unsafeWrite as ix (W# a'#)
        MV.unsafeModify bs (\y -> y `append` V.unsafeIndex fs (I# pow#)) ix

    forM_ [0 .. MV.length as] $ \k -> do
      a <- MV.unsafeRead as k
      MV.unsafeModify bs (\b -> if a /= 1 then b `append` f a 1 else b) k

    V.unsafeFreeze bs

{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Int  -> V.Vector Int  #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Word -> V.Vector Word #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Bool -> V.Vector Bool #-}
