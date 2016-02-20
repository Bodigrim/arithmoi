-- |
-- Module:      Math.NumberTheory.Primes.Counting.Approximate
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: portable
--
-- Approximations to the number of primes below a limit and the
-- n-th prime.
--
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Counting.Approximate
    ( approxPrimeCount
    , approxPrimeCountOverestimateLimit
    , nthPrimeApprox
    , nthPrimeApproxUnderestimateLimit
    ) where

-- For prime p = 3742914359 we have
--   approxPrimeCount p = 178317879
--         primeCount p = 178317880

-- | Following property holds:
--
-- > approxPrimeCount n >= primeCount n || n >= approxPrimeCountOverestimateLimit
approxPrimeCountOverestimateLimit :: Integral a => a
approxPrimeCountOverestimateLimit = 3742914359

-- | @'approxPrimeCount' n@ gives an
--   approximation of the number of primes not exceeding
--   @n@. The approximation is fairly good for @n@ large enough.
approxPrimeCount :: Integral a => a -> a
approxPrimeCount = truncate . max 0 . appi . fromIntegral

-- | Following property holds:
--
-- > nthPrimeApprox n <= nthPrime n || n >= nthPrimeApproxUnderestimateLimit
nthPrimeApproxUnderestimateLimit :: Integer
nthPrimeApproxUnderestimateLimit = 1000000000000

-- | @'nthPrimeApprox' n@ gives an
--   approximation to the n-th prime. The approximation
--   is fairly good for @n@ large enough.
nthPrimeApprox :: Integer -> Integer
nthPrimeApprox = max 1 . truncate . nthApp . fromIntegral . max 3

-- Basically the approximation of the prime count by Li(x),
-- adjusted to give close but slightly too high estimates
-- in the interesting range. The constants are empirically
-- determined.
appi :: Double -> Double
appi x = y - y/300000 + 7*ll
  where
    y = x*l*(1+l*(1+l*h))
    w = log x
    l = 1/w
    ll = log w
    h | x < 10000000    = 2.5625
      | x < 50000000    = 2.5
      | x < 120000000   = 617/256
      | otherwise       = 2.0625 + l*(3+ll*l*(13.25+ll*l*57.75))

-- Basically an approximation to the inverse of Li(x), with
-- empirically determined constants to get close results
-- in the interesting range.
nthApp :: Double -> Double
nthApp x = a
  where
    l  = log x
    ll = log l
    li = 1/l
    l2 = ll*ll
    a  = x*(l+ll-1+li*(ll-2-li*(ll*(0.3+li*(1+0.02970812*l2*l2*l2*li))
                + 8.725*(ll-2.749)*(ll-3.892)*li))) + l*ll + 35
