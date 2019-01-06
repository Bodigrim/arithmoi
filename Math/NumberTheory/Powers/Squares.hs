-- |
-- Module:      Math.NumberTheory.Powers.Squares
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Functions dealing with squares. Efficient calculation of integer square roots
-- and efficient testing for squareness.
{-# LANGUAGE MagicHash, BangPatterns, PatternGuards, CPP, FlexibleContexts #-}
module Math.NumberTheory.Powers.Squares
    ( -- * Square root calculation
      integerSquareRoot
    , integerSquareRoot'
    , integerSquareRootRem
    , integerSquareRootRem'
    , exactSquareRoot
      -- * Tests for squares
    , isSquare
    , isSquare'
    , isPossibleSquare
    , isPossibleSquare2
    ) where

#include "MachDeps.h"

import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Numeric.Natural

import Math.NumberTheory.Powers.Squares.Internal

-- | Calculate the integer square root of a nonnegative number @n@,
--   that is, the largest integer @r@ with @r*r <= n@.
--   Throws an error on negative input.
{-# SPECIALISE integerSquareRoot :: Int -> Int,
                                    Word -> Word,
                                    Integer -> Integer,
                                    Natural -> Natural
  #-}
integerSquareRoot :: Integral a => a -> a
integerSquareRoot n
  | n < 0       = error "integerSquareRoot: negative argument"
  | otherwise   = integerSquareRoot' n

-- | Calculate the integer square root of a nonnegative number @n@,
--   that is, the largest integer @r@ with @r*r <= n@.
--   The precondition @n >= 0@ is not checked.
{-# RULES
"integerSquareRoot'/Int"     integerSquareRoot' = isqrtInt'
"integerSquareRoot'/Word"    integerSquareRoot' = isqrtWord
"integerSquareRoot'/Integer" integerSquareRoot' = isqrtInteger
  #-}
{-# INLINE [1] integerSquareRoot' #-}
integerSquareRoot' :: Integral a => a -> a
integerSquareRoot' = isqrtA

-- | Calculate the integer square root of a nonnegative number as well as
--   the difference of that number with the square of that root, that is if
--   @(s,r) = integerSquareRootRem n@ then @s^2 <= n == s^2+r < (s+1)^2@.
{-# SPECIALISE integerSquareRootRem ::
        Int -> (Int, Int),
        Word -> (Word, Word),
        Integer -> (Integer, Integer),
        Natural -> (Natural, Natural)
  #-}
integerSquareRootRem :: Integral a => a -> (a, a)
integerSquareRootRem n
  | n < 0       = error "integerSquareRootRem: negative argument"
  | otherwise   = integerSquareRootRem' n

-- | Calculate the integer square root of a nonnegative number as well as
--   the difference of that number with the square of that root, that is if
--   @(s,r) = integerSquareRootRem' n@ then @s^2 <= n == s^2+r < (s+1)^2@.
--   The precondition @n >= 0@ is not checked.
{-# RULES
"integerSquareRootRem'/Integer" integerSquareRootRem' = karatsubaSqrt
  #-}
{-# INLINE [1] integerSquareRootRem' #-}
integerSquareRootRem' :: Integral a => a -> (a, a)
integerSquareRootRem' n = (s, n - s * s)
  where
    s = integerSquareRoot' n

-- | Returns 'Nothing' if the argument is not a square,
--   @'Just' r@ if @r*r == n@ and @r >= 0@. Avoids the expensive calculation
--   of the square root if @n@ is recognized as a non-square
--   before, prevents repeated calculation of the square root
--   if only the roots of perfect squares are needed.
--   Checks for negativity and 'isPossibleSquare'.
{-# SPECIALISE exactSquareRoot :: Int -> Maybe Int,
                                  Word -> Maybe Word,
                                  Integer -> Maybe Integer,
                                  Natural -> Maybe Natural
  #-}
exactSquareRoot :: Integral a => a -> Maybe a
exactSquareRoot n
  | n >= 0
  , isPossibleSquare n
  , (r, 0) <- integerSquareRootRem' n = Just r
  | otherwise                         = Nothing

-- | Test whether the argument is a square.
--   After a number is found to be positive, first 'isPossibleSquare'
--   is checked, if it is, the integer square root is calculated.
{-# SPECIALISE isSquare :: Int -> Bool,
                           Word -> Bool,
                           Integer -> Bool,
                           Natural -> Bool
  #-}
isSquare :: Integral a => a -> Bool
isSquare n = n >= 0 && isSquare' n

-- | Test whether the input (a nonnegative number) @n@ is a square.
--   The same as 'isSquare', but without the negativity test.
--   Faster if many known positive numbers are tested.
--
--   The precondition @n >= 0@ is not tested, passing negative
--   arguments may cause any kind of havoc.
{-# SPECIALISE isSquare' :: Int -> Bool,
                            Word -> Bool,
                            Integer -> Bool,
                            Natural -> Bool
  #-}
isSquare' :: Integral a => a -> Bool
isSquare' n
    | isPossibleSquare n
    , (_, 0) <- integerSquareRootRem' n = True
    | otherwise                         = False

-- | Test whether a non-negative number may be a square.
--   Non-negativity is not checked, passing negative arguments may
--   cause any kind of havoc.
--
--   First the remainder modulo 256 is checked (that can be calculated
--   easily without division and eliminates about 82% of all numbers).
--   After that, the remainders modulo 9, 25, 7, 11 and 13 are tested
--   to eliminate altogether about 99.436% of all numbers.
--
--   This is the test used by 'exactSquareRoot'. For large numbers,
--   the slower but more discriminating test 'isPossibleSquare2' is
--   faster.
{-# SPECIALISE isPossibleSquare :: Int -> Bool,
                                   Word -> Bool,
                                   Integer -> Bool,
                                   Natural -> Bool
  #-}
isPossibleSquare :: Integral a => a -> Bool
isPossibleSquare n
  =  V.unsafeIndex sr256 ((fromIntegral n) .&. 255)
  && V.unsafeIndex sr693 (fromIntegral (n `rem` 693))
  && V.unsafeIndex sr325 (fromIntegral (n `rem` 325))

-- | Test whether a non-negative number may be a square.
--   Non-negativity is not checked, passing negative arguments may
--   cause any kind of havoc.
--
--   First the remainder modulo 256 is checked (that can be calculated
--   easily without division and eliminates about 82% of all numbers).
--   After that, the remainders modulo several small primes are tested
--   to eliminate altogether about 99.98954% of all numbers.
--
--   For smallish to medium sized numbers, this hardly performs better
--   than 'isPossibleSquare', which uses smaller arrays, but for large
--   numbers, where calculating the square root becomes more expensive,
--   it is much faster (if the vast majority of tested numbers aren't squares).
{-# SPECIALISE isPossibleSquare2 :: Int -> Bool,
                                    Word -> Bool,
                                    Integer -> Bool,
                                    Natural -> Bool
  #-}
isPossibleSquare2 :: Integral a => a -> Bool
isPossibleSquare2 n
  =  V.unsafeIndex sr256  ((fromIntegral n) .&. 255)
  && V.unsafeIndex sr819  (fromIntegral (n `rem` 819))
  && V.unsafeIndex sr1025 (fromIntegral (n `rem` 1025))
  && V.unsafeIndex sr2047 (fromIntegral (n `rem` 2047))
  && V.unsafeIndex sr4097 (fromIntegral (n `rem` 4097))
  && V.unsafeIndex sr341  (fromIntegral (n `rem` 341))

-----------------------------------------------------------------------------
--  Auxiliary Stuff

-- Make an array indicating whether a remainder is a square remainder.
sqRemArray :: Int -> V.Vector Bool
sqRemArray md = runST $ do
  ar <- MV.replicate md False
  let !stop = (md `quot` 2) + 1
      fill k
        | k < stop  = MV.unsafeWrite ar ((k*k) `rem` md) True >> fill (k+1)
        | otherwise = return ()
  MV.unsafeWrite ar 0 True
  MV.unsafeWrite ar 1 True
  fill 2
  V.unsafeFreeze ar

sr256 :: V.Vector Bool
sr256 = sqRemArray 256

sr819 :: V.Vector Bool
sr819 = sqRemArray 819

sr4097 :: V.Vector Bool
sr4097 = sqRemArray 4097

sr341 :: V.Vector Bool
sr341 = sqRemArray 341

sr1025 :: V.Vector Bool
sr1025 = sqRemArray 1025

sr2047 :: V.Vector Bool
sr2047 = sqRemArray 2047

sr693 :: V.Vector Bool
sr693 = sqRemArray 693

sr325 :: V.Vector Bool
sr325 = sqRemArray 325

-- Specialisations for Int, Word, and Integer

-- For @n <= 2^64@, the result of
--
-- > truncate (sqrt $ fromIntegral n)
--
-- is never too small and never more than one too large.
-- The multiplication doesn't overflow for 32 or 64 bit Ints.
isqrtInt' :: Int -> Int
isqrtInt' n
    | n < r*r   = r-1
    | otherwise = r
      where
        !r = (truncate :: Double -> Int) . sqrt $ fromIntegral n
-- With -O2, that should be translated to the below
{-
isqrtInt' n@(I# i#)
    | r# *# r# ># i#            = I# (r# -# 1#)
    | otherwise                 = I# r#
      where
        !r# = double2Int# (sqrtDouble# (int2Double# i#))
-}

-- Same for Word.
isqrtWord :: Word -> Word
isqrtWord n
    | n < (r*r)
#if WORD_SIZE_IN_BITS == 64
      || r == 4294967296
-- Double interprets values near maxBound as 2^64, we don't have that problem for 32 bits
#endif
                = r-1
    | otherwise = r
      where
        !r = (fromIntegral :: Int -> Word) . (truncate :: Double -> Int) . sqrt $ fromIntegral n

{-# INLINE isqrtInteger #-}
isqrtInteger :: Integer -> Integer
isqrtInteger = fst . karatsubaSqrt
