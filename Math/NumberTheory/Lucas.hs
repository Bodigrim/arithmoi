-- |
-- Module:      Math.NumberTheory.Lucas
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Efficient calculation of Lucas sequences.
{-# LANGUAGE CPP #-}
module Math.NumberTheory.Lucas
  ( fibonacci
  , fibonacciPair
  , lucas
  , lucasPair
  , generalLucas
  ) where

#include "MachDeps.h"

import Data.Bits

-- | @'fibonacci' k@ calculates the @k@-th Fibonacci number in
--   /O/(@log (abs k)@) steps. The index may be negative. This
--   is efficient for calculating single Fibonacci numbers (with
--   large index), but for computing many Fibonacci numbers in
--   close proximity, it is better to use the simple addition
--   formula starting from an appropriate pair of successive
--   Fibonacci numbers.
fibonacci :: Int -> Integer
fibonacci = fst . fibonacciPair

-- | @'fibonacciPair' k@ returns the pair @(F(k), F(k+1))@ of the @k@-th
--   Fibonacci number and its successor, thus it can be used to calculate
--   the Fibonacci numbers from some index on without needing to compute
--   the previous. The pair is efficiently calculated
--   in /O/(@log (abs k)@) steps. The index may be negative.
fibonacciPair :: Int -> (Integer, Integer)
fibonacciPair n
  | n < 0     = let (f,g) = fibonacciPair (-(n+1)) in if testBit n 0 then (g, -f) else (-g, f)
  | n == 0    = (0, 1)
  | otherwise = look (WORD_SIZE_IN_BITS - 2)
    where
      look k
        | testBit n k = go (k-1) 0 1
        | otherwise   = look (k-1)
      go k g f
        | k < 0       = (f, f+g)
        | testBit n k = go (k-1) (f*(f+shiftL g 1)) ((f+g)*shiftL f 1 + g*g)
        | otherwise   = go (k-1) (f*f+g*g) (f*(f+shiftL g 1))

-- | @'lucas' k@ computes the @k@-th Lucas number. Very similar
--   to @'fibonacci'@.
lucas :: Int -> Integer
lucas = fst . lucasPair

-- | @'lucasPair' k@ computes the pair @(L(k), L(k+1))@ of the @k@-th
--   Lucas number and its successor. Very similar to @'fibonacciPair'@.
lucasPair :: Int -> (Integer, Integer)
lucasPair n
  | n < 0     = let (f,g) = lucasPair (-(n+1)) in if testBit n 0 then (-g, f) else (g, -f)
  | n == 0    = (2, 1)
  | otherwise = look (WORD_SIZE_IN_BITS - 2)
    where
      look k
        | testBit n k = go (k-1) 0 1
        | otherwise   = look (k-1)
      go k g f
        | k < 0       = (shiftL g 1 + f,g+3*f)
        | otherwise   = go (k-1) g' f'
          where
            (f',g')
              | testBit n k = (shiftL (f*(f+g)) 1 + g*g,f*(shiftL g 1 + f))
              | otherwise   = (f*(shiftL g 1 + f),f*f+g*g)


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
generalLucas :: Integer -> Integer -> Int -> (Integer, Integer, Integer, Integer)
generalLucas p q k
  | k < 0       = error "generalLucas: negative index"
  | k == 0      = (0,1,2,p)
  | otherwise   = look (WORD_SIZE_IN_BITS - 2)
    where
      look i
        | testBit k i   = go (i-1) 1 p p q
        | otherwise     = look (i-1)
      go i un un1 vn qn
        | i < 0         = (un, un1, vn, p*un1 - shiftL (q*un) 1)
        | testBit k i   = go (i-1) (un1*vn-qn) ((p*un1-q*un)*vn - p*qn) ((p*un1 - (2*q)*un)*vn - p*qn) (qn*qn*q)
        | otherwise     = go (i-1) (un*vn) (un1*vn-qn) (vn*vn - 2*qn) (qn*qn)
