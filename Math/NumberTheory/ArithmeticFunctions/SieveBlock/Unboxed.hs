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
  , multiplicativeSieveBlockConfig
  , additiveSieveBlockConfig
  , sieveBlockUnboxed
  ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Exts

import Math.NumberTheory.Logarithms (integerLogBase')
import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Utils (splitOff#)
import Math.NumberTheory.Utils.FromIntegral

-- | A record, which specifies a function to evaluate over a block.
--
-- For example, here is a configuration for the totient function:
--
-- > SieveBlockConfig
-- >   { sbcEmpty                = 1
-- >   , sbcFunctionOnPrimePower = (\p a -> (p - 1) * p ^ (a - 1)
-- >   , sbcAppend               = (*)
-- >   }
data SieveBlockConfig a = SieveBlockConfig
  { sbcEmpty                :: a
    -- ^ value of a function on 1
  , sbcFunctionOnPrimePower :: Word -> Word -> a
    -- ^ how to evaluate a function on prime powers
  , sbcAppend               :: a -> a -> a
    -- ^ how to combine values of a function on coprime arguments
  }

-- | Create a config for a multiplicative function from its definition on prime powers.
multiplicativeSieveBlockConfig :: Num a => (Word -> Word -> a) -> SieveBlockConfig a
multiplicativeSieveBlockConfig f = SieveBlockConfig
  { sbcEmpty                = 1
  , sbcFunctionOnPrimePower = f
  , sbcAppend               = (*)
  }

-- | Create a config for an additive function from its definition on prime powers.
additiveSieveBlockConfig :: Num a => (Word -> Word -> a) -> SieveBlockConfig a
additiveSieveBlockConfig f = SieveBlockConfig
  { sbcEmpty                = 0
  , sbcFunctionOnPrimePower = f
  , sbcAppend               = (+)
  }

-- | Evaluate a function over a block in accordance to provided configuration.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- Based on Algorithm M of <https://arxiv.org/pdf/1305.1639.pdf Parity of the number of primes in a given interval and algorithms of the sublinear summation> by A. V. Lelechenko. See Lemma 2 on p. 5 on its algorithmic complexity. For the majority of use-cases its time complexity is O(x^(1+ε)).
--
-- For example, here is an analogue of divisor function 'tau':
--
-- > > sieveBlockUnboxed (SieveBlockConfig 1 (*) (\_ a -> a + 1) 1 10)
-- > [1,2,2,3,2,4,2,4,3,4]
sieveBlockUnboxed
  :: V.Unbox a
  => SieveBlockConfig a
  -> Word
  -> Word
  -> V.Vector a
sieveBlockUnboxed (SieveBlockConfig empty f append) lowIndex' len' = runST $ do

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

    forM_ ps $ \p -> do

      let p# :: Word#
          !p'@(W# p#) = intToWord p

          fs = V.generate
            (integerLogBase' (toInteger p) (toInteger highIndex))
            (\k -> f p' (intToWord k + 1))

          offset :: Int
          offset = negate lowIndex `mod` p

      forM_ [offset, offset + p .. len - 1] $ \ix -> do
        W# a# <- MV.unsafeRead as ix
        let !(# pow#, a'# #) = splitOff# p# (a# `quotWord#` p#)
        MV.unsafeWrite as ix (W# a'#)
        MV.unsafeModify bs (\y -> y `append` V.unsafeIndex fs (I# pow#)) ix

    forM_ [0 .. MV.length as - 1] $ \k -> do
      a <- MV.unsafeRead as k
      MV.unsafeModify bs (\b -> if a /= 1 then b `append` f a 1 else b) k

    V.unsafeFreeze bs

{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Int  -> Word -> Word -> V.Vector Int  #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Word -> Word -> Word -> V.Vector Word #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Bool -> Word -> Word -> V.Vector Bool #-}
