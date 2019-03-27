-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.SieveBlock.Unboxed
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
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

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Exts

import Math.NumberTheory.ArithmeticFunctions.Moebius (Moebius)
import Math.NumberTheory.Logarithms (integerLogBase')
import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Types (Prime(..))
import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Math.NumberTheory.Utils (splitOff)
import Math.NumberTheory.Utils.FromIntegral (wordToInt, intToWord)

-- | A record, which specifies a function to evaluate over a block.
--
-- For example, here is a configuration for the totient function:
--
-- > SieveBlockConfig
-- >   { sbcEmpty                = 1
-- >   , sbcFunctionOnPrimePower = \p a -> (unPrime p - 1) * unPrime p ^ (a - 1)
-- >   , sbcAppend               = (*)
-- >   }
data SieveBlockConfig a = SieveBlockConfig
  { sbcEmpty                :: a
    -- ^ value of a function on 1
  , sbcFunctionOnPrimePower :: Prime Word -> Word -> a
    -- ^ how to evaluate a function on prime powers
  , sbcAppend               :: a -> a -> a
    -- ^ how to combine values of a function on coprime arguments
  }

-- | Create a config for a multiplicative function from its definition on prime powers.
multiplicativeSieveBlockConfig :: Num a => (Prime Word -> Word -> a) -> SieveBlockConfig a
multiplicativeSieveBlockConfig f = SieveBlockConfig
  { sbcEmpty                = 1
  , sbcFunctionOnPrimePower = f
  , sbcAppend               = (*)
  }

-- | Create a config for an additive function from its definition on prime powers.
additiveSieveBlockConfig :: Num a => (Prime Word -> Word -> a) -> SieveBlockConfig a
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
-- For example, here is an analogue of divisor function 'Math.NumberTheory.ArithmeticFunctions.tau':
--
-- >>> sieveBlockUnboxed (SieveBlockConfig 1 (\_ a -> a + 1) (*)) 1 10
-- [1,2,2,3,2,4,2,4,3,4]
sieveBlockUnboxed
  :: V.Unbox a
  => SieveBlockConfig a
  -> Word
  -> Word
  -> V.Vector a
sieveBlockUnboxed _ _ 0 = V.empty
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
        ps = takeWhile (<= integerSquareRoot highIndex) $ map unPrime primes

    forM_ ps $ \p -> do

      let p# :: Word#
          !p'@(W# p#) = intToWord p

          fs = V.generate
            (integerLogBase' (toInteger p) (toInteger highIndex))
            (\k -> f (Prime p') (intToWord k + 1))

          offset :: Int
          offset = negate lowIndex `mod` p

      forM_ [offset, offset + p .. len - 1] $ \ix -> do
        W# a# <- MV.unsafeRead as ix
        let !(W# pow#, W# a'#) = splitOff (W# p#) (W# (a# `quotWord#` p#))
        MV.unsafeWrite as ix (W# a'#)
        MV.unsafeModify bs (\y -> y `append` V.unsafeIndex fs (I# (word2Int# pow#))) ix

    forM_ [0 .. len - 1] $ \k -> do
      a <- MV.unsafeRead as k
      MV.unsafeModify bs (\b -> if a /= 1 then b `append` f (Prime a) 1 else b) k

    V.unsafeFreeze bs

{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Int  -> Word -> Word -> V.Vector Int  #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Word -> Word -> Word -> V.Vector Word #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Bool -> Word -> Word -> V.Vector Bool #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Moebius -> Word -> Word -> V.Vector Moebius #-}
