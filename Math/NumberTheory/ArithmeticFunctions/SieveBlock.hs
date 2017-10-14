-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.SieveBlock
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Bulk evaluation of arithmetic functions.
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Math.NumberTheory.ArithmeticFunctions.SieveBlock
  ( runFunctionOverBlock
  , SieveBlockConfig(..)
  , sieveBlock
  , sieveBlockUnboxed
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Coerce
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Exts
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid
#endif

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.ArithmeticFunctions.SieveBlock.Unboxed
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Utils (splitOff#)
import Math.NumberTheory.Utils.FromIntegral

runFunctionOverBlock
  :: ArithmeticFunction Word a
  -> Word
  -> Word
  -> V.Vector a
runFunctionOverBlock (ArithmeticFunction f g) lowIndex len = V.map g $ sieveBlock $ SieveBlockConfig
  { sbcEmpty                = mempty
  , sbcAppend               = mappend
  , sbcFunctionOnPrimePower = coerce f
  , sbcBlockLowBound        = lowIndex
  , sbcBlockLength          = len
  }

-- | Similar to 'sieveBlockUnboxed' up to flavour of 'Data.Vector'.
sieveBlock
  :: SieveBlockConfig a
  -> V.Vector a
sieveBlock (SieveBlockConfig empty append f lowIndex' len') = runST $ do

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
