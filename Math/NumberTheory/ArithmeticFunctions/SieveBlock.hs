-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.SieveBlock
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Bulk evaluation of arithmetic functions over continuous intervals
-- without factorisation.
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Math.NumberTheory.ArithmeticFunctions.SieveBlock
  ( runFunctionOverBlock
  , SieveBlockConfig(..)
  , multiplicativeSieveBlockConfig
  , additiveSieveBlockConfig
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
import Math.NumberTheory.Logarithms (integerLogBase')
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Utils (splitOff#)
import Math.NumberTheory.Utils.FromIntegral

-- | 'runFunctionOverBlock' @f@ @x@ @l@ evaluates an arithmetic function
-- for integers between @x@ and @x+l-1@ and returns a vector of length @l@.
-- It completely avoids factorisation, so it is asymptotically faster than
-- pointwise evaluation of @f@.
--
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- Beware that for underlying non-commutative monoids the results may potentially
-- differ from pointwise application via 'runFunction'.
--
-- This is a thin wrapper over 'sieveBlock', read more details there.
--
-- > > runFunctionOverBlock carmichaelA 1 10
-- > [1,1,2,2,4,2,6,2,6,4]
runFunctionOverBlock
  :: ArithmeticFunction Word a
  -> Word
  -> Word
  -> V.Vector a
runFunctionOverBlock (ArithmeticFunction f g) = (V.map g .) . sieveBlock SieveBlockConfig
  { sbcEmpty                = mempty
  , sbcAppend               = mappend
  , sbcFunctionOnPrimePower = coerce f
  }

-- | Evaluate a function over a block in accordance to provided configuration.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- Based on Algorithm M of <https://arxiv.org/pdf/1305.1639.pdf Parity of the number of primes in a given interval and algorithms of the sublinear summation> by A. V. Lelechenko. See Lemma 2 on p. 5 on its algorithmic complexity. For the majority of use-cases its time complexity is O(x^(1+ε)).
--
-- 'sieveBlock' is similar to 'sieveBlockUnboxed' up to flavour of 'Data.Vector',
-- but is typically 7x-10x slower and consumes 3x memory.
-- Use unboxed version whenever possible.
--
-- For example, following code lists smallest prime factors:
--
-- > > sieveBlock (SieveBlockConfig maxBound min (\p _ -> p) 2 10)
-- > [2,3,2,5,2,7,2,3,2,11]
sieveBlock
  :: SieveBlockConfig a
  -> Word
  -> Word
  -> V.Vector a
sieveBlock (SieveBlockConfig empty f append) lowIndex' len' = runST $ do

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
