-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.NFreedom
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- N-free number generation.
--

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.ArithmeticFunctions.NFreedom
  ( nFrees
  , sieveBlockNFree
  ) where

import Control.Monad                         (forM_)
import Control.Monad.ST                      (runST)
import Data.List                             (scanl')
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Math.NumberTheory.Powers.Squares      (integerSquareRoot)
import Math.NumberTheory.Primes              (primes, unPrime)
import Math.NumberTheory.UniqueFactorisation (UniqueFactorisation)
import Math.NumberTheory.Utils.FromIntegral  (wordToInt)

-- | Evaluate the `isNFreeA` function over a block.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- >>> sieveBlockNFree 2 1 10
-- [True, True, True, False, True, True, True, False, False, True]
sieveBlockNFree
  :: forall a . Integral a
  => Word
  -> a
  -> Word
  -> U.Vector Bool
sieveBlockNFree _ _ 0 = U.empty
sieveBlockNFree n lowIndex len'
  = runST $ do
    as <- MU.replicate (wordToInt len') True
    forM_ ps $ \p -> do
      let pPow :: a
          pPow = p ^ n
          offset :: a
          offset = negate lowIndex `mod` pPow
          indices :: [a]
          indices = [offset, offset + pPow .. minimum [ fromIntegral . pred $ (maxBound :: Int)
                                                      , len - 1
                                                      , fromIntegral highIndex]]
      forM_ indices $ \ix -> do
          MU.write as (fromIntegral ix) False
    U.freeze as

  where
    len :: a
    len = fromIntegral len'

    highIndex :: a
    highIndex = lowIndex + len - 1

    ps :: [a]
    ps = takeWhile (<= integerSquareRoot highIndex) $ map unPrime primes

-- | For a given nonnegative integer power @n@, generate all @n@-free
-- numbers in ascending order, starting at @1@.
nFrees :: forall a . (Integral a, UniqueFactorisation a) => Word -> [a]
nFrees 0 = [1]
nFrees 1 = [1]
nFrees n = concatMap nFreesListInternal nFreeList
  where
    strides :: [Word]
    strides = take 55 (iterate (2 *) 256) ++ repeat (fromIntegral (maxBound :: Int))
    bounds :: [a]
    bounds = scanl' (+) 1 $ map fromIntegral strides
    nFreeList :: [(U.Vector Bool, a, Word)]
    nFreeList =
       zipWith (\lo strd -> (sieveBlockNFree n lo strd, lo, strd)) bounds strides
    nFreesListInternal :: (U.Vector Bool, a, Word) -> [a]
    nFreesListInternal (bs, lo, strd) =
        let strd' :: a
            strd' = fromIntegral strd
            strd'' :: Int
            strd'' = fromIntegral strd
        in map snd .
           filter ((bs U.!) . fst) .
           zip [0 .. strd'' - 1] $ [lo .. lo + strd' - 1]