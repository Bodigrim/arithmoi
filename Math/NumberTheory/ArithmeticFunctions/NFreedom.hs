-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.NFreedom
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- N-free number generation.
--

module Math.NumberTheory.ArithmeticFunctions.NFreedom
  ( sieveBlockNFree
  ) where

import Control.Monad                        (forM_)
import Control.Monad.ST                     (runST)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Math.NumberTheory.Powers.Squares     (integerSquareRoot)
import Math.NumberTheory.Primes             (primes, unPrime)
import Math.NumberTheory.Utils.FromIntegral (wordToInt)

-- | Evaluate the `isNFreeA` function over a block.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- >>> sieveBlockNFree 2 1 10
-- [True, True, True, False, True, True, True, False, False, True]
sieveBlockNFree
  :: Word
  -> Word
  -> Word
  -> U.Vector Bool
sieveBlockNFree _ _ 0 = U.empty
sieveBlockNFree n lowIndex' len'
  = runST $ do
    as <- MU.replicate len True
    forM_ ps $ \p -> do
      let pPow = p ^ n
          offset = negate lowIndex `mod` pPow
      forM_ [offset, offset + pPow .. len - 1] $ \ix -> do
          MU.unsafeWrite as ix False
    U.unsafeFreeze as

  where
    lowIndex :: Int
    lowIndex = wordToInt lowIndex'

    len :: Int
    len = wordToInt len'

    highIndex :: Int
    highIndex = lowIndex + len - 1

    ps :: [Int]
    ps = takeWhile (<= integerSquareRoot highIndex) $ map unPrime primes