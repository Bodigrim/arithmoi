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
module Math.NumberTheory.Primes.Counting.Approximate
    ( approxPrimeCount
    , nthPrimeApprox
    ) where

-- | @'approxPrimeCount' n@ gives (for @n > 0@) an
--   approximation of the number of primes not exceeding
--   @n@. The approximation is fairly good for @n@ large enough.
--   The number of primes should be slightly overestimated
--   (so it is suitable for allocation of storage) and is
--   never underestimated for @n <= 10^12@.
approxPrimeCount :: Integral a => a -> a
approxPrimeCount = truncate . appi . fromIntegral

-- | @'nthPrimeApprox' n@ gives (for @n > 0@) an
--   approximation to the n-th prime. The approximation
--   is fairly good for @n@ large enough. Dual to
--   @'approxPrimeCount'@, this estimate should err
--   on the low side (and does for @n < 10^12@).
nthPrimeApprox :: Integral a => a -> a
nthPrimeApprox = max 1 . truncate . nthApp . fromIntegral

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
