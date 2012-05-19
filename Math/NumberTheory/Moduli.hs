-- |
-- Module:      Math.NumberTheory.Moduli
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Miscellaneous functions related to modular arithmetic.
--
{-# LANGUAGE CPP, BangPatterns #-}
module Math.NumberTheory.Moduli
    ( -- * Functions with input check
      jacobi
    , invertMod
    , powerMod
    , powerModInteger
      -- * Unchecked functions
    , jacobi'
    , powerMod'
    , powerModInteger'
    ) where

#include "MachDeps.h"

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Array.Base (unsafeAt)

import Math.NumberTheory.Utils (shiftToOddCount)

-- | Invert a number relative to a modulus.
--   If @number@ and @modulus@ are coprime, the result is
--   @Just inverse@ where
--
-- >    (number * inverse) `mod` (abs modulus) == 1
-- >    0 <= inverse < abs modulus
--
--   unless @modulus == 0@ and @abs number == 1@, in which case the
--   result is @Just number@.
--   If @gcd number modulus > 1@, the result is @Nothing@.
invertMod :: Integer -> Integer -> Maybe Integer
invertMod k 0 = if k == 1 || k == (-1) then Just k else Nothing
invertMod k m = wrap $ go False 1 0 m' k'
  where
    m' = abs m
    k' | r < 0     = r+m'
       | otherwise = r
         where
           r = k `rem` m'
    wrap x = case (x*k') `rem` m' of
               1 -> Just x
               _ -> Nothing
    -- Calculate modular inverse of k' modulo m' by continued fraction expansion
    -- of m'/k', say [a_0,a_1,...,a_s]. Let the convergents be p_j/q_j.
    -- Starting from j = -2, the arguments of go are
    -- (p_j/q_j) > m'/k', p_{j+1}, p_j, and n, d with n/d = [a_{j+2},...,a_s].
    -- Since m'/k' = p_s/q_s, and p_j*q_{j+1} - p_{j+1}*q_j = (-1)^(j+1), we have
    -- p_{s-1}*k' - q_{s-1}*m' = (-1)^s * gcd m' k', so if the inverse exists,
    -- it is either p_{s-1} or -p_{s-1}, depending on whether s is even or odd.
    go !b _ po _ 0 = if b then po else (m'-po)
    go b !pn po n d = case n `quotRem` d of
                        (q,r) -> go (not b) (q*pn+po) pn d r

-- | Jacobi symbol of two numbers.
--   The \"denominator\" must be odd and positive, this condition is checked.
--
--   If both numbers have a common prime factor, the result
--   is @0@, otherwise it is &#177;1.
{-# SPECIALISE jacobi :: Integer -> Integer -> Int,
                         Int -> Int -> Int,
                         Word -> Word -> Int
  #-}
jacobi :: (Integral a, Bits a) => a -> a -> Int
jacobi a b
  | b < 0       = error "Math.NumberTheory.Moduli.jacobi: negative denominator"
  | evenI b     = error "Math.NumberTheory.Moduli.jacobi: even denominator"
  | b == 1      = 1
  | a == 0      = 0
  | a == 1      = 1
  | otherwise   = jacobi' a b   -- b odd, > 1, a neither 0 or 1

-- Invariant: b > 1 and odd
-- | Jacobi symbol of two numbers without validity check of
--   the \"denominator\".
{-# SPECIALISE jacobi' :: Integer -> Integer -> Int,
                          Int -> Int -> Int,
                          Word -> Word -> Int
  #-}
jacobi' :: (Integral a, Bits a) => a -> a -> Int
jacobi' a b
  | a == 0      = 0
  | a == 1      = 1
  | a < 0       = let n | rem4 b == 1 = 1
                        | otherwise   = -1
                      -- Blech, minBound may pose problems
                      (z,o) = shiftToOddCount (abs $ toInteger a)
                      s | evenI z || unsafeAt jac2 (rem8 b) == 1 = n
                        | otherwise                              = (-n)
                  in s*jacobi' (fromInteger o) b
  | a >= b      = case a `rem` b of
                    0 -> 0
                    r -> jacPS 1 r b
  | evenI a     = case shiftToOddCount a of
                    (z,o) -> let r = 2 - (rem4 o .&. rem4 b)
                                 s | evenI z || unsafeAt jac2 (rem8 b) == 1 = r
                                   | otherwise                              = (-r)
                             in jacOL s b o
  | otherwise   = case rem4 a .&. rem4 b of
                    3 -> jacOL (-1) b a
                    _ -> jacOL 1 b a

-- numerator positive and smaller than denominator
{-# SPECIALISE jacPS :: Int -> Integer -> Integer -> Int,
                        Int -> Int -> Int -> Int,
                        Int -> Word -> Word -> Int
  #-}
jacPS :: (Integral a, Bits a) => Int -> a -> a -> Int
jacPS !j a b
  | evenI a     = case shiftToOddCount a of
                    (z,o) | evenI z || unsafeAt jac2 (rem8 b) == 1 ->
                              jacOL (if rem4 o .&. rem4 b == 3 then (-j) else j) b o
                          | otherwise ->
                              jacOL (if rem4 o .&. rem4 b == 3 then j else (-j)) b o
  | otherwise   = jacOL (if rem4 a .&. rem4 b == 3 then (-j) else j) b a

-- numerator odd, positive and larger than denominator
{-# SPECIALISE jacOL :: Int -> Integer -> Integer -> Int,
                        Int -> Int -> Int -> Int,
                        Int -> Word -> Word -> Int
  #-}
jacOL :: (Integral a, Bits a) => Int -> a -> a -> Int
jacOL !j a b
  | b == 1    = j
  | otherwise = case a `rem` b of
                 0 -> 0
                 r -> jacPS j r b

-- | Modular power.
--
-- > powerMod base exponent modulus
--
--   calculates @(base ^ exponent) \`mod\` modulus@ by repeated squaring and reduction.
--   If @exponent < 0@ and @base@ is invertible modulo @modulus@, @(inverse ^ |exponent|) \`mod\` modulus@
--   is calculated. This function does some input checking and sanitation before calling the unsafe worker.
{-# SPECIALISE powerMod :: Integer -> Int -> Integer -> Integer,
                           Integer -> Word -> Integer -> Integer
  #-}
{-# RULES
"powerMod/Integer" powerMod = powerModInteger
  #-}
powerMod :: (Integral a, Bits a) => Integer -> a -> Integer -> Integer
powerMod base expo md
  | md == 0     = base ^ expo
  | md' == 1    = 0
  | expo == 0   = 1
  | bse' == 1   = 1
  | expo < 0    = case invertMod bse' md' of
                    Just i  -> powerMod' i (negate expo) md'
                    Nothing -> error "Math.NumberTheory.Moduli.powerMod: Base isn't invertible with respect to modulus"
  | bse' == 0   = 0
  | otherwise   = powerMod' bse' expo md'
    where
      md' = abs md
      bse' = if base < 0 || md' <= base then base `mod` md' else base

-- | Modular power worker without input checking.
--   Assumes all arguments strictly positive and modulus greater than 1.
{-# SPECIALISE powerMod' :: Integer -> Int -> Integer -> Integer,
                            Integer -> Word -> Integer -> Integer
  #-}
{-# RULES
"powerMod'/Integer" powerMod' = powerModInteger'
  #-}
powerMod' :: (Integral a, Bits a) => Integer -> a -> Integer -> Integer
powerMod' base expo md = go expo 1 base
  where
    go 1 !a !s  = (a*s) `rem` md
    go e a s
      | testBit e 0 = go (e `shiftR` 1) ((a*s) `rem` md) ((s*s) `rem` md)
      | otherwise   = go (e `shiftR` 1) a ((s*s) `rem` md)

-- | Specialised version of 'powerMod' for 'Integer' exponents.
--   Reduces the number of shifts of the exponent since shifting
--   large 'Integer's is expensive. Call this function directly
--   if you don't want or can't rely on rewrite rules.
powerModInteger :: Integer -> Integer -> Integer -> Integer
powerModInteger base ex mdl
  | mdl == 0    = base ^ ex
  | mdl' == 1   = 0
  | ex == 0     = 1
  | ex < 0      = case invertMod bse' mdl' of
                    Just i  -> powerModInteger' i (negate ex) mdl'
                    Nothing -> error "Math.NumberTheory.Moduli.powerMod: Base isn't invertible with respect to modulus"
  | bse' == 0   = 0
  | bse' == 1   = 1
  | otherwise   = powerModInteger' bse' ex mdl'
    where
      mdl' = abs mdl
      bse' = if base < 0 || mdl' <= base then base `mod` mdl' else base

-- | Specialised worker without input checks. Makes the same assumptions
--   as the general version 'powerMod''.
powerModInteger' :: Integer -> Integer -> Integer -> Integer
powerModInteger' base expo md = go w1 1 base e1
  where
    w1 = fromInteger expo
    e1 = expo `shiftR` 64
#if WORD_SIZE_IN_BITS == 32
  -- Shifting large Integers is expensive, hence we reduce the
  -- number of shifts by processing in 64-bit chunks.
  -- On 32-bit systems, every testBit on a Word64 would be a C-call,
  -- thus it is faster to split each Word64 into the constituent 32-bit
  -- Words and process those separately.
  -- The code becomes ugly, unfortunately.
    go :: Word64 -> Integer -> Integer -> Integer -> Integer
    go !w !a !s 0  = end a s w
    go w a s e = inner1 a s 0
      where
        wl :: Word
        !wl = fromIntegral w
        wh :: Word
        !wh = fromIntegral (w `shiftR` 32)
        inner1 !au !sq 32 = inner2 au sq 0
        inner1 au sq i
          | testBit wl i = inner1 ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
          | otherwise    = inner1 au ((sq*sq) `rem` md) (i+1)
        inner2 !au !sq 32 = go (fromInteger e) au sq (e `shiftR` 64)
        inner2 au sq i
          | testBit wh i = inner2 ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
          | otherwise    = inner2 au ((sq*sq) `rem` md) (i+1)
    end !a !s w
      | wh == 0   = fin a s wl
      | otherwise = innerE a s 0
        where
          wl :: Word
          !wl = fromIntegral w
          wh :: Word
          !wh = fromIntegral (w `shiftR` 32)
          innerE !au !sq 32 = fin au sq wh
          innerE au sq i
            | testBit wl i = innerE ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
            | otherwise    = innerE au ((sq*sq) `rem` md) (i+1)
    fin :: Integer -> Integer -> Word -> Integer
    fin !a !s 1 = (a*s) `rem` md
    fin a s w
      | testBit w 0 = fin ((a*s) `rem` md) ((s*s) `rem` md) (w `shiftR` 1)
      | otherwise   = fin a ((s*s) `rem` md) (w `shiftR` 1)

#else
  -- WORD_SIZE_IN_BITS == 64, otherwise things wouldn't compile anyway
  -- Shorter code since we need not split each 64-bit word.
    go :: Word -> Integer -> Integer -> Integer -> Integer
    go !w !a !s 0  = end a s w
    go w a s e = inner a s 0
      where
        inner !au !sq 64 = go (fromInteger e) au sq (e `shiftR` 64)
        inner au sq i
          | testBit w i = inner ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
          | otherwise   = inner au ((sq*sq) `rem` md) (i+1)
    end !a !s 1 = (a*s) `rem` md
    end a s w
      | testBit w 0 = end ((a*s) `rem` md) ((s*s) `rem` md) (w `shiftR` 1)
      | otherwise   = end a ((s*s) `rem` md) (w `shiftR` 1)

#endif

-- Utilities

-- For large Integers, going via Int is much faster than bit-fiddling
-- on the Integer, so we do that.
{-# SPECIALISE evenI :: Integer -> Bool,
                        Int -> Bool,
                        Word -> Bool
  #-}
evenI :: (Integral a, Bits a) => a -> Bool
evenI n = fromIntegral n .&. 1 == (0 :: Int)

{-# SPECIALISE rem4 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem4 :: (Integral a, Bits a) => a -> Int
rem4 n = fromIntegral n .&. 3

{-# SPECIALISE rem8 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem8 :: (Integral a, Bits a) => a -> Int
rem8 n = fromIntegral n .&. 7

jac2 :: UArray Int Int
jac2 = array (0,7) [(0,0),(1,1),(2,0),(3,-1),(4,0),(5,-1),(6,0),(7,1)]
