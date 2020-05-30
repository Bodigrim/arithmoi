-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.SieveBlock
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Bulk evaluation of arithmetic functions over continuous intervals
-- without factorisation.
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.ArithmeticFunctions.SieveBlock
  ( runFunctionOverBlock
  , runMoebiusOverBlock
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.ArithmeticFunctions.Moebius (Moebius, runMoebiusOverBlock)
import Math.NumberTheory.Logarithms (wordLog2, integerLogBase')
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Utils.FromIntegral (wordToInt, intToWord)

-- | Evaluate a function over a block in accordance to provided configuration.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- Based on Algorithm M of <https://arxiv.org/pdf/1305.1639.pdf Parity of the number of primes in a given interval and algorithms of the sublinear summation> by A. V. Lelechenko. See Lemma 2 on p. 5 on its algorithmic complexity. For the majority of use-cases its time complexity is O(x^(1+ε)).
--
-- For example, following code lists smallest prime factors:
--
-- >>> runFunctionOverBlock (ArithmeticFunction maxBound (\p _ -> unPrime p) min) 2 10 :: Data.Vector.Vector Word
-- [2,3,2,5,2,7,2,3,2,11]
--
-- And this is how to factorise all numbers in a block:
--
-- >>> runFunctionOverBlock (ArithmeticFunction [] (\p k -> [(unPrime p, k)]) (++)) 2 10 :: Data.Vector.Vector [(Word, Word)]
-- [[(2,1)],[(3,1)],[(2,2)],[(5,1)],[(2,1),(3,1)],[(7,1)],[(2,3)],[(3,2)],[(2,1),(5,1)],[(11,1)]]
runFunctionOverBlock
  :: forall v a.
     G.Vector v a
  => ArithmeticFunction Word a
  -> Word
  -> Word
  -> v a
runFunctionOverBlock _ _ 0 = G.empty
runFunctionOverBlock (ArithmeticFunction empty f append) !lowIndex' len' = runST $ do

    let lowIndex :: Int
        lowIndex = wordToInt lowIndex'

        len :: Int
        len = wordToInt len'

        highIndex :: Int
        highIndex = lowIndex + len - 1

        highIndex' :: Word
        highIndex' = intToWord highIndex

        ps :: [Int]
        ps = if highIndex < 4 then [] else map unPrime [nextPrime 2 .. precPrime (integerSquareRoot highIndex)]

    as <- MU.replicate len 1
    bs <- MG.replicate len empty

    let doPrime 2 = do
          let fs = V.generate (wordLog2 highIndex')
                (\k -> f (Prime 2) (intToWord k + 1))
              npLow  = (lowIndex' + 1) `shiftR` 1
              npHigh = highIndex'      `shiftR` 1
          forM_ [npLow .. npHigh] $ \np@(W# np#) -> do
            let ix = wordToInt (np `shiftL` 1) - lowIndex :: Int
                tz = I# (word2Int# (ctz# np#))
            MU.unsafeModify as (\x -> x `shiftL` (tz + 1)) ix
            MG.unsafeModify bs (\y -> y `append` V.unsafeIndex fs tz) ix

        doPrime p = do
          let p' = intToWord p
              f0 = f (Prime p') 1
              logp = integerLogBase' (toInteger p) (toInteger highIndex) - 1
              fs = V.generate logp (\k -> f (Prime p') (intToWord k + 2))
              npLow  = (lowIndex + p - 1) `quot` p
              npHigh = highIndex          `quot` p

          forM_ [npLow .. npHigh] $ \np -> do
            let !(I# ix#) = np * p - lowIndex
                (q, r) = np `quotRem` p
            if r /= 0
            then do
              MU.unsafeModify as (* p')        (I# ix#)
              MG.unsafeModify bs (`append` f0) (I# ix#)
            else do
              let pow = highestPowerDividing p q
              MU.unsafeModify as (\x -> x * p' ^ (pow + 2))                          (I# ix#)
              MG.unsafeModify bs (\y -> y `append` V.unsafeIndex fs (wordToInt pow)) (I# ix#)

    forM_ ps doPrime

    forM_ [0 .. len - 1] $ \k -> do
      a <- MU.unsafeRead as k
      let a' = intToWord (k + lowIndex)
      when (a /= a') $
        MG.unsafeModify bs (\b -> b `append` f (Prime $ a' `quot` a) 1) k

    G.unsafeFreeze bs

-- This is a variant of 'Math.NumberTheory.Utils.splitOff',
-- specialized for better performance.
highestPowerDividing :: Int -> Int -> Word
highestPowerDividing !_ 0 = 0
highestPowerDividing p n = go 0 n
  where
    go !k m = case m `quotRem` p of
      (q, 0) -> go (k + 1) q
      _      -> k

{-# SPECIALIZE runFunctionOverBlock :: ArithmeticFunction Word Int  -> Word -> Word -> U.Vector Int  #-}
{-# SPECIALIZE runFunctionOverBlock :: ArithmeticFunction Word Word -> Word -> Word -> U.Vector Word #-}
{-# SPECIALIZE runFunctionOverBlock :: ArithmeticFunction Word Bool -> Word -> Word -> U.Vector Bool #-}
{-# SPECIALIZE runFunctionOverBlock :: ArithmeticFunction Word Moebius -> Word -> Word -> U.Vector Moebius #-}
