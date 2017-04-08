{-# LANGUAGE BangPatterns #-}

module Math.NumberTheory.Moduli.Jacobi
  ( jacobi
  , jacobi'
  ) where

import Data.Array.Unboxed
import Data.Bits

import Math.NumberTheory.Unsafe
import Math.NumberTheory.Utils

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
  | otherwise   = jacobi' a b   -- b odd, > 1

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

-- Utilities

-- For large Integers, going via Int is much faster than bit-fiddling
-- on the Integer, so we do that.
{-# SPECIALISE evenI :: Integer -> Bool,
                        Int -> Bool,
                        Word -> Bool
  #-}
evenI :: Integral a => a -> Bool
evenI n = fromIntegral n .&. 1 == (0 :: Int)

{-# SPECIALISE rem4 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem4 :: Integral a => a -> Int
rem4 n = fromIntegral n .&. 3

{-# SPECIALISE rem8 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem8 :: Integral a => a -> Int
rem8 n = fromIntegral n .&. 7

jac2 :: UArray Int Int
jac2 = array (0,7) [(0,0),(1,1),(2,0),(3,-1),(4,0),(5,-1),(6,0),(7,1)]
