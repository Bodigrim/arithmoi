-- |
-- Module:      Math.NumberTheory.Recurrences.Linear
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Efficient calculation of linear recurrent sequences, including Fibonacci and Lucas sequences.

{-# LANGUAGE BangPatterns #-}

module Math.NumberTheory.Recurrences.Linear
  ( factorial
  , factorialFactors
  , fibonacci
  , fibonacciPair
  , lucas
  , lucasPair
  , generalLucas
  ) where

import Data.Bits
import Numeric.Natural
import Math.NumberTheory.Primes

-- | Infinite zero-based table of factorials.
--
-- >>> take 5 factorial
-- [1,1,2,6,24]
--
-- The time-and-space behaviour of 'factorial' is similar to described in
-- "Math.NumberTheory.Recurrences.Bilinear#memory".
factorial :: (Num a, Enum a) => [a]
factorial = scanl (*) 1 [1..]
{-# SPECIALIZE factorial :: [Int]     #-}
{-# SPECIALIZE factorial :: [Word]    #-}
{-# SPECIALIZE factorial :: [Integer] #-}
{-# SPECIALIZE factorial :: [Natural] #-}

-- | Prime factors of a factorial.
--
-- prop> factorialFactors n == factorise (factorial !! n)
--
-- >>> factorialFactors 10
-- [(Prime 2,8),(Prime 3,4),(Prime 5,2),(Prime 7,1)]
factorialFactors :: Word -> [(Prime Word, Word)]
factorialFactors n
  | n < 2
  = []
  | otherwise
  = map (\p -> (p, mult (unPrime p))) [minBound .. precPrime n]
  where
    mult :: Word -> Word
    mult p = go np np
      where
        np = n `quot` p
        go !acc !x
          | x >= p = let xp = x `quot` p in go (acc + xp) xp
          | otherwise = acc

-- | @'fibonacci' k@ calculates the @k@-th Fibonacci number in
--   /O/(@log (abs k)@) steps. The index may be negative. This
--   is efficient for calculating single Fibonacci numbers (with
--   large index), but for computing many Fibonacci numbers in
--   close proximity, it is better to use the simple addition
--   formula starting from an appropriate pair of successive
--   Fibonacci numbers.
fibonacci :: Num a => Int -> a
fibonacci = fst . fibonacciPair
{-# SPECIALIZE fibonacci :: Int -> Int     #-}
{-# SPECIALIZE fibonacci :: Int -> Word    #-}
{-# SPECIALIZE fibonacci :: Int -> Integer #-}
{-# SPECIALIZE fibonacci :: Int -> Natural #-}

-- | @'fibonacciPair' k@ returns the pair @(F(k), F(k+1))@ of the @k@-th
--   Fibonacci number and its successor, thus it can be used to calculate
--   the Fibonacci numbers from some index on without needing to compute
--   the previous. The pair is efficiently calculated
--   in /O/(@log (abs k)@) steps. The index may be negative.
fibonacciPair :: Num a => Int -> (a, a)
fibonacciPair n
  | n < 0     = let (f,g) = fibonacciPair (-(n+1)) in if testBit n 0 then (g, -f) else (-g, f)
  | n == 0    = (0, 1)
  | otherwise = look (finiteBitSize (0 :: Word) - 2)
    where
      look k
        | testBit n k = go (k-1) 0 1
        | otherwise   = look (k-1)
      go k g f
        | k < 0       = (f, f+g)
        | testBit n k = go (k-1) (f*(f+shiftL1 g)) ((f+g)*shiftL1 f + g*g)
        | otherwise   = go (k-1) (f*f+g*g) (f*(f+shiftL1 g))
{-# SPECIALIZE fibonacciPair :: Int -> (Int, Int)         #-}
{-# SPECIALIZE fibonacciPair :: Int -> (Word, Word)       #-}
{-# SPECIALIZE fibonacciPair :: Int -> (Integer, Integer) #-}
{-# SPECIALIZE fibonacciPair :: Int -> (Natural, Natural) #-}

-- | @'lucas' k@ computes the @k@-th Lucas number. Very similar
--   to @'fibonacci'@.
lucas :: Num a => Int -> a
lucas = fst . lucasPair
{-# SPECIALIZE lucas :: Int -> Int     #-}
{-# SPECIALIZE lucas :: Int -> Word    #-}
{-# SPECIALIZE lucas :: Int -> Integer #-}
{-# SPECIALIZE lucas :: Int -> Natural #-}

-- | @'lucasPair' k@ computes the pair @(L(k), L(k+1))@ of the @k@-th
--   Lucas number and its successor. Very similar to @'fibonacciPair'@.
lucasPair :: Num a => Int -> (a, a)
lucasPair n
  | n < 0     = let (f,g) = lucasPair (-(n+1)) in if testBit n 0 then (-g, f) else (g, -f)
  | n == 0    = (2, 1)
  | otherwise = look (finiteBitSize (0 :: Word) - 2)
    where
      look k
        | testBit n k = go (k-1) 0 1
        | otherwise   = look (k-1)
      go k g f
        | k < 0       = (shiftL1 g + f,g+3*f)
        | otherwise   = go (k-1) g' f'
          where
            (f',g')
              | testBit n k = (shiftL1 (f*(f+g)) + g*g,f*(shiftL1 g + f))
              | otherwise   = (f*(shiftL1 g + f),f*f+g*g)
{-# SPECIALIZE lucasPair :: Int -> (Int, Int)         #-}
{-# SPECIALIZE lucasPair :: Int -> (Word, Word)       #-}
{-# SPECIALIZE lucasPair :: Int -> (Integer, Integer) #-}
{-# SPECIALIZE lucasPair :: Int -> (Natural, Natural) #-}

-- | @'generalLucas' p q k@ calculates the quadruple @(U(k), U(k+1), V(k), V(k+1))@
--   where @U(i)@ is the Lucas sequence of the first kind and @V(i)@ the Lucas
--   sequence of the second kind for the parameters @p@ and @q@, where @p^2-4q /= 0@.
--   Both sequences satisfy the recurrence relation @A(j+2) = p*A(j+1) - q*A(j)@,
--   the starting values are @U(0) = 0, U(1) = 1@ and @V(0) = 2, V(1) = p@.
--   The Fibonacci numbers form the Lucas sequence of the first kind for the
--   parameters @p = 1, q = -1@ and the Lucas numbers form the Lucas sequence of
--   the second kind for these parameters.
--   Here, the index must be non-negative, since the terms of the sequence for
--   negative indices are in general not integers.
generalLucas :: Num a => a -> a -> Int -> (a, a, a, a)
generalLucas p q k
  | k < 0       = error "generalLucas: negative index"
  | k == 0      = (0,1,2,p)
  | otherwise   = look (finiteBitSize (0 :: Word) - 2)
    where
      look i
        | testBit k i   = go (i-1) 1 p p q
        | otherwise     = look (i-1)
      go i un un1 vn qn
        | i < 0         = (un, un1, vn, p*un1 - shiftL1 (q*un))
        | testBit k i   = go (i-1) (un1*vn-qn) ((p*un1-q*un)*vn - p*qn) ((p*un1 - (2*q)*un)*vn - p*qn) (qn*qn*q)
        | otherwise     = go (i-1) (un*vn) (un1*vn-qn) (vn*vn - 2*qn) (qn*qn)
{-# SPECIALIZE generalLucas :: Int     -> Int     -> Int -> (Int, Int, Int, Int)                 #-}
{-# SPECIALIZE generalLucas :: Word    -> Word    -> Int -> (Word, Word, Word, Word)             #-}
{-# SPECIALIZE generalLucas :: Integer -> Integer -> Int -> (Integer, Integer, Integer, Integer) #-}
{-# SPECIALIZE generalLucas :: Natural -> Natural -> Int -> (Natural, Natural, Natural, Natural) #-}

shiftL1 :: Num a => a -> a
shiftL1 n = n + n
