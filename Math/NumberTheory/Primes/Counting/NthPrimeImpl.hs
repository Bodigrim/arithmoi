-- |
-- Module:      Math.NumberTheory.Primes.Counting.Impl
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Find the @n@-th prime.
--
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Math.NumberTheory.Primes.Counting.NthPrimeImpl
    ( nthPrime
    , nthPrimeMaxArg
    ) where

import Math.NumberTheory.Primes.Sieve.Eratosthenes
                             ( PrimeSieve(..), primeSieve
                             , SieveCfg(..), defaultCfg, baseFor
                             , nthPrimeAfterCfg, nthPrimeAtOrBeforeCfg )
import Math.NumberTheory.Primes.Sieve.WheelIndexing (ndxToPrm)
import Math.NumberTheory.Primes.Counting.Approximate (nthPrimeApprox)
import Math.NumberTheory.Primes.Counting.CountingImpl (primeCount)
import Math.NumberTheory.Primes.Types

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bit (Bit(..), nthBitIndex, countBits)
import qualified Data.Vector.Unboxed as U

--------------------------------------------------------------------------------
--                                Nth Prime                                   --
--------------------------------------------------------------------------------

-- | The largest argument 'nthPrime' accepts: the number of primes up to @2^64 - 1@,
--   so that @'nthPrime' 'nthPrimeMaxArg' == 2^64 - 59@, the largest prime that fits in 64 bits.
--   Larger arguments are an error.
nthPrimeMaxArg :: Int
nthPrimeMaxArg = 425656284035217743

-- | @'nthPrime' n@ calculates the @n@-th prime. Numbering of primes is
--   @1@-based, so @'nthPrime' 1 == 2@.
--
--   The argument must be strictly positive and at most 'nthPrimeMaxArg'.
--
--   Small arguments are answered by a plain sieve.  For larger ones the prime is estimated by
--   inverting Riemann's R function (within about 70 sieve pages of 15.7 million values for every
--   argument in range), 'primeCount' is applied to the estimate, and the remaining distance is
--   sieved forwards or backwards, so the time is that of one 'primeCount' plus a few seconds.
--   Space is dominated by the base primes up to the square root of the result:
--   about 1 GB for results near @2^64@, about 4 bytes per prime below the square root.
nthPrime :: Int -> Prime Integer
nthPrime 1 = Prime 2
nthPrime 2 = Prime 3
nthPrime 3 = Prime 5
nthPrime 4 = Prime 7
nthPrime 5 = Prime 11
nthPrime 6 = Prime 13
nthPrime n
    | n < 1
    = error "Prime indexing starts at 1"
    | n < 200000
    = Prime $ countToNth (n - 3) [primeSieve (p0 + p0 `quot` 32 + 37)]
    | n > nthPrimeMaxArg
    = error $ "nthPrime: index " ++ show n ++ " is too large to handle (the maximum is "
              ++ show nthPrimeMaxArg ++ ", the number of primes below 2^64)"
    | otherwise
    = Prime $ nthPrimeLarge (toInteger n)
      where
        p0 = nthPrimeApprox (toInteger n)

--------------------------------------------------------------------------------
--                                The Works                                   --
--------------------------------------------------------------------------------

-- Largest value the sieve handles.
maxValue :: Integer
maxValue = 2 ^ (64 :: Int) - 1

-- Eight pages (4 MB) per segment, eight segments per group: a group of 64 pages is about
-- 1 billion values, which covers the expected distance from the estimate in one go.
nthCfg :: SieveCfg
nthCfg = SieveCfg 8 8 (cfgLargeFrom defaultCfg)

-- The n-th prime for n > 6 (in practice n >= 200000), n <= nthPrimeMaxArg.
nthPrimeLarge :: Integer -> Integer
nthPrimeLarge n
  | c < n     = nthPrimeAfterCfg nthCfg base est (n - c)             -- the prime is above the estimate
  | otherwise = nthPrimeAtOrBeforeCfg nthCfg base est (c - n + 1)    -- the prime is at or below it
  where
    est  = max 2 (min maxValue (nthPrimeEstimateR n))    -- the estimate can reach 2^64 near the top
    c    = primeCount est
    base = baseFor (min maxValue (est + 2 ^ (34 :: Int)))

-- Riemann R and its inverse.  R(x) follows pi(x) to about sqrt x / ln x with no systematic bias
-- (li(x) is biased high by about the same amount), so solving R(y) = n gives an estimate of the
-- n-th prime that is within tens of pages of the prime up to 2^64.  Plain Double arithmetic is
-- accurate enough (the error is about a thousand values).

-- zeta(s), s >= 2, to Double accuracy: the sum of the first 59 terms (smallest first) and the
-- Euler-Maclaurin tail from 60 on.
zetaD :: Int -> Double
zetaD s = sum [ m ** negate sD | m <- [59, 58 .. 1 :: Double] ]
          + mm ** (1 - sD) / (sD - 1) + mm ** negate sD / 2 + sD * mm ** (negate sD - 1) / 12
  where
    sD = fromIntegral s :: Double
    mm = 60 :: Double

zetaTab :: UArray Int Double
zetaTab = listArray (2, 61) [ zetaD s | s <- [2 .. 61] ]

zetaAt :: Int -> Double
zetaAt s | s > 61    = 1.0    -- 1 + 2^-62 is 1 in Double
         | otherwise = zetaTab ! s

-- Gram's series  R(x) = 1 + sum_{k>=1} (ln x)^k / (k * k! * zeta(k+1)),  x > 1.
-- All terms are positive, so there is no cancellation.
riemannR :: Double -> Double
riemannR x = go 1 1 1
  where
    l = log x
    go :: Int -> Double -> Double -> Double
    go !k !p !tot
      | k > 400   = tot
      | otherwise =
          let !p'   = p * l / fromIntegral k
              !t    = p' / (fromIntegral k * zetaAt (k + 1))
              !tot' = tot + t
          in if fromIntegral k > l && t < tot' * 1e-17 then tot' else go (k + 1) p' tot'

-- Estimate of the n-th prime (n >= 6): the solution of R(y) = n by Newton's method.
nthPrimeEstimateR :: Integer -> Integer
nthPrimeEstimateR n = truncate (newton (0 :: Int) y0)
  where
    nD = fromInteger n :: Double
    y0 = nD * (log nD + log (log nD))
    newton :: Int -> Double -> Double
    newton !i !y
      | i >= 30                = y
      | abs step <= 1e-13 * y  = y'
      | otherwise              = newton (i + 1) y'
      where
        step = (riemannR y - nD) * log y
        y'   = max 2 (y - step)

--------------------------------------------------------------------------------
-- Small arguments

-- find the n-th clear bit (a survivor, i.e. a prime) in a list of PrimeSieves,
-- aka find the (n+3)-rd prime
countToNth :: Int -> [PrimeSieve] -> Integer
countToNth !_ [] = error "countToNth: Prime stream ended prematurely"
countToNth !n (PS v0 bs : more) = case nthBitIndex (Bit False) n bs of
  Just i  -> v0 + toInteger (ndxToPrm i)
  Nothing -> countToNth (n - (U.length bs - countBits bs)) more
