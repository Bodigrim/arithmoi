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

import Control.Monad                        (forM_, liftM)
import Control.Monad.ST                     (runST)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Math.NumberTheory.Primes (primes, unPrime)

-- | Evaluate the `isNFreeA` function over a block.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- >>> sieveBlockNFree 1 10
-- [MoebiusP, MoebiusN, MoebiusN, MoebiusZ, MoebiusN, MoebiusP, MoebiusN, MoebiusZ, MoebiusZ, MoebiusP]
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
      let offset  = negate lowIndex `mod` p
          offset2 = negate lowIndex `mod` (p * p)
      forM_ [offset, offset + p .. len - 1] $ \ix -> do
        MU.unsafeModify as (\y -> y + l) ix
      forM_ [offset2, offset2 + p ^ n .. len - 1] $ \ix -> do
        MU.unsafeWrite as ix False
    forM_ [0 .. len - 1] $ \ix -> do
      MU.unsafeModify as (mapper ix) ix
    U.unsafeFreeze as

  where
    lowIndex :: Int
    lowIndex = wordToInt lowIndex'

    len :: Int
    len = wordToInt len'

    ps :: [Integer]
    ps = map unPrime primes