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
{-# LANGUAGE UnboxedTuples       #-}

module Math.NumberTheory.ArithmeticFunctions.SieveBlock
  ( runFunctionOverBlock
  , SieveBlockConfig(..)
  , multiplicativeSieveBlockConfig
  , additiveSieveBlockConfig
  , sieveBlock
  , sieveBlockUnboxed
  , sieveBlockMoebius
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Bits
import Data.Coerce
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.ArithmeticFunctions.Moebius (Moebius, sieveBlockMoebius)
import Math.NumberTheory.Logarithms (wordLog2, integerLogBase')
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Roots.Squares (integerSquareRoot)
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
-- >>> import Math.NumberTheory.ArithmeticFunctions
-- >>> runFunctionOverBlock carmichaelA 1 10
-- [1,1,2,2,4,2,6,2,6,4]
runFunctionOverBlock
  :: ArithmeticFunction Word a
  -> Word
  -> Word
  -> V.Vector a
runFunctionOverBlock (ArithmeticFunction f g) = (G.map g .) . sieveBlock SieveBlockConfig
  { sbcEmpty                = mempty
  , sbcAppend               = mappend
  , sbcFunctionOnPrimePower = coerce f
  }

-- | Evaluate a function over a block in accordance to provided configuration.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- Based on Algorithm M of <https://arxiv.org/pdf/1305.1639.pdf Parity of the number of primes in a given interval and algorithms of the sublinear summation> by A. V. Lelechenko. See Lemma 2 on p. 5 on its algorithmic complexity. For the majority of use-cases its time complexity is O(x^(1+ε)).
--
-- For example, following code lists smallest prime factors:
--
-- >>> sieveBlock (SieveBlockConfig maxBound (\p _ -> unPrime p) min) 2 10
-- [2,3,2,5,2,7,2,3,2,11]
--
-- And this is how to factorise all numbers in a block:
--
-- >>> sieveBlock (SieveBlockConfig [] (\p k -> [(unPrime p, k)]) (++)) 2 10
-- [[(2,1)],[(3,1)],[(2,2)],[(5,1)],[(2,1),(3,1)],[(7,1)],[(2,3)],[(3,2)],[(2,1),(5,1)],[(11,1)]]
sieveBlock
  :: forall v a.
     G.Vector v a
  => SieveBlockConfig a
  -> Word
  -> Word
  -> v a
sieveBlock _ _ 0 = G.empty
sieveBlock (SieveBlockConfig empty f append) !lowIndex' len' = runST $ do

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
              MU.unsafeModify as (\x -> x * p')        (I# ix#)
              MG.unsafeModify bs (\y -> y `append` f0) (I# ix#)
            else do
              let (pow, _) = splitOff p q
              MU.unsafeModify as (\x -> x * p' ^ (pow + 2))                          (I# ix#)
              MG.unsafeModify bs (\y -> y `append` V.unsafeIndex fs (wordToInt pow)) (I# ix#)

    forM_ ps doPrime

    forM_ [0 .. len - 1] $ \k -> do
      a <- MU.unsafeRead as k
      let a' = intToWord (k + lowIndex)
      when (a /= a') $
        MG.unsafeModify bs (\b -> b `append` f (Prime $ a' `quot` a) 1) k

    G.unsafeFreeze bs

-- | This is 'sieveBlock' specialized to unboxed vectors.
--
-- >>> sieveBlockUnboxed (SieveBlockConfig 1 (\_ a -> a + 1) (*)) 1 10
-- [1,2,2,3,2,4,2,4,3,4]
sieveBlockUnboxed
  :: U.Unbox a
  => SieveBlockConfig a
  -> Word
  -> Word
  -> U.Vector a
sieveBlockUnboxed = sieveBlock

{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Int  -> Word -> Word -> U.Vector Int  #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Word -> Word -> Word -> U.Vector Word #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Bool -> Word -> Word -> U.Vector Bool #-}
{-# SPECIALIZE sieveBlockUnboxed :: SieveBlockConfig Moebius -> Word -> Word -> U.Vector Moebius #-}
