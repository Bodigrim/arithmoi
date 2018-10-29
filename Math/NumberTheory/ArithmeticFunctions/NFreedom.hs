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
-- Value at @0@, if zero falls into block, is undefined.
-- This function should be used with a negative lower bound. If it is, the
-- result is undefined.
--
-- >>> sieveBlockNFree 2 1 10
-- [True, True, True, False, True, True, True, False, False, True]
sieveBlockNFree
  :: forall a . Integral a
  => Word
  -- ^ Power whose @n@-freedom will be checked.
  -> a
  -- ^ Lower index of the block.
  -> Word
  -- ^ Length of the block.
  -> U.Vector Bool
  -- ^ Vector of flags, where @True@ at index @i@ means the @i@-th element of
  -- the block is @n@-free.
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
--
-- When @n@ is @0@ or @1@, the resulting list is @[1]@.
nFrees :: forall a . (Integral a, UniqueFactorisation a) => Word -> [a]
nFrees 0 = [1]
nFrees 1 = [1]
nFrees n = concatMap nFreesListInternal nFreeList
  where
    -- The 56th element of @iterate (2 *) 256@ is @2^64 :: Word == 0@, so to
    -- avoid overflow only the first 55 elements of this list are used.
    -- After those, since @maxBound :: Int@ is the largest a vector can be,
    -- this value is just repeated. This means after a few dozen iterations,
    -- the sieve will stop increasing in size.
    strides :: [Word]
    strides = take 55 (iterate (2 *) 256) ++ repeat (fromIntegral (maxBound :: Int))

    -- Infinite list of lower bounds at which @sieveBlockNFrees@ will be
    -- applied. This has type @Integral a => a@ instead of @Word@ because
    -- unlike the sizes of the sieve that eventually stop increasing (see
    -- above comment), the lower bound at which @sieveBlockNFrees@ does not.
    bounds :: [a]
    bounds = scanl' (+) 1 $ map fromIntegral strides

    nFreeList :: [(U.Vector Bool, a, Word)]
    nFreeList =
       zipWith (\lo strd -> (sieveBlockNFree n lo strd, lo, strd)) bounds strides

    nFreesListInternal :: (U.Vector Bool, a, Word) -> [a]
    nFreesListInternal (bs, lo, strd) =
        let -- When indexing the array of flags @bs@, the index has to be an
            -- @Int@. As such, it's necessary to cast @strd@ twice.
            -- Once, immediately below, to create the range of values whose
            -- @n@-freedom will be tested. Since @nFrees@ has return type
            -- @[a]@, this cannot be avoided as @strides@ has type @[Word]@.
            strd' :: a
            strd' = fromIntegral strd
            -- Twice, immediately below, to create the range of indices with
            -- which to query @bs@.
            strd'' :: Int
            strd'' = fromIntegral strd
        in map snd .
           filter ((bs U.!) . fst) .
           zip [0 .. strd'' - 1] $ [lo .. lo + strd' - 1]