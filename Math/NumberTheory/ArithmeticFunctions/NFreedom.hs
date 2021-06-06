-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.NFreedom
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- N-free number generation.
--

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.ArithmeticFunctions.NFreedom
  ( nFrees
  , nFreesBlock
  , sieveBlockNFree
  ) where

import Control.Monad                         (forM_)
import Control.Monad.ST                      (runST)
import Data.Bits (Bits)
import Data.List                             (scanl')
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
import Math.NumberTheory.Utils.FromIntegral

-- | Evaluate the `Math.NumberTheory.ArithmeticFunctions.isNFree` function over a block.
-- Value at @0@, if zero falls into block, is undefined.
--
-- This function should __**not**__ be used with a negative lower bound.
-- If it is, the result is undefined.
-- Furthermore, do not:
--
-- * use a block length greater than @maxBound :: Int@.
-- * use a power that is either of @0, 1@.
--
-- None of these preconditions are checked, and if any occurs, the result is
-- undefined, __if__ the function terminates.
--
-- >>> sieveBlockNFree 2 1 10
-- [True,True,True,False,True,True,True,False,False,True]
sieveBlockNFree
  :: forall a. (Integral a, Enum (Prime a), Bits a, UniqueFactorisation a)
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
          -- The second argument in @Data.Vector.Unboxed.Mutable.write@ is an
          -- @Int@, so to avoid segmentation faults or out-of-bounds errors,
          -- the enumeration's higher bound must always be less than
          -- @maxBound :: Int@.
          -- As mentioned above, this is not checked when using this function
          -- by itself, but is carefully managed when this function is called
          -- by @nFrees@, see the comments in it.
          indices :: [a]
          indices = [offset, offset + pPow .. len - 1]
      forM_ indices $ \ix ->
          MU.write as (fromIntegral ix) False
    U.freeze as

  where
    len :: a
    len = fromIntegral len'

    highIndex :: a
    highIndex = lowIndex + len - 1

    ps :: [a]
    ps = if highIndex < 4 then [] else map unPrime [nextPrime 2 .. precPrime (integerSquareRoot highIndex)]

-- | For a given nonnegative integer power @n@, generate all @n@-free
-- numbers in ascending order, starting at @1@.
--
-- When @n@ is @0@ or @1@, the resulting list is @[1]@.
nFrees
    :: forall a. (Integral a, Bits a, UniqueFactorisation a, Enum (Prime a))
    => Word
    -- ^ Power @n@ to be used to generate @n@-free numbers.
    -> [a]
    -- ^ Generated infinite list of @n@-free numbers.
nFrees 0 = [1]
nFrees 1 = [1]
nFrees n = concatMap (uncurry (nFreesBlock n)) $ zip bounds strides
  where
    -- The 56th element of @iterate (2 *) 256@ is @2^64 :: Word == 0@, so to
    -- avoid overflow only the first 55 elements of this list are used.
    -- After those, since @maxBound :: Int@ is the largest a vector can be,
    -- this value is just repeated. This means after a few dozen iterations,
    -- the sieve will stop increasing in size.
    strides :: [Word]
    strides = take 55 (iterate (2 *) 256) ++ repeat (intToWord (maxBound :: Int))

    -- Infinite list of lower bounds at which @sieveBlockNFree@ will be
    -- applied. This has type @Integral a => a@ instead of @Word@ because
    -- unlike the sizes of the sieve that eventually stop increasing (see
    -- above comment), the lower bound at which @sieveBlockNFree@ is called does not.
    bounds :: [a]
    bounds = scanl' (+) 1 $ map fromIntegral strides

-- | Generate @n@-free numbers in a block starting at a certain value.
-- The length of the list is determined by the value passed in as the third
-- argument. It will be lesser than or equal to this value.
--
-- This function should not be used with a negative lower bound. If it is,
-- the result is undefined.
--
-- The block length cannot exceed @maxBound :: Int@, this precondition is not
-- checked.
--
-- As with @nFrees@, passing @n = 0, 1@ results in an empty list.
nFreesBlock
    :: forall a . (Integral a, Bits a, UniqueFactorisation a, Enum (Prime a))
    => Word
    -- ^ Power @n@ to be used to generate @n@-free numbers.
    -> a
    -- ^ Starting number in the block.
    -> Word
    -- ^ Maximum length of the block to be generated.
    -> [a]
    -- ^ Generated list of @n@-free numbers.
nFreesBlock 0 lo _ = help lo
nFreesBlock 1 lo _ = help lo
nFreesBlock n lowIndex len =
    let -- When indexing the array of flags @bs@, the index has to be an
        -- @Int@. As such, it's necessary to cast @strd@ twice.
        -- * Once, immediately below, to create the range of values whose
        -- @n@-freedom will be tested. Since @nFrees@ has return type
        -- @[a]@, this cannot be avoided as @strides@ has type @[Word]@.
        len' :: Int
        len' = wordToInt len
        -- * Twice, immediately below, to create the range of indices with
        -- which to query @bs@.
        len'' :: a
        len'' = fromIntegral len
        bs  = sieveBlockNFree n lowIndex len
    in map snd .
       filter ((bs U.!) . fst) .
       zip [0 .. len' - 1] $ [lowIndex .. lowIndex + len'']
{-# INLINE nFreesBlock #-}

help :: Integral a => a -> [a]
help 1 = [1]
help _ = []
{-# INLINE help #-}
