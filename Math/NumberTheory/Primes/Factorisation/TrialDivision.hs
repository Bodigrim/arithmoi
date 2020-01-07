-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.TrialDivision
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Factorisation and primality testing using trial division.
--
-- Used to create and check certificates.
-- Currently not exposed because it's not that useful, is it?
-- But the trial...To functions are exported from other modules.
{-# LANGUAGE BangPatterns #-}
module Math.NumberTheory.Primes.Factorisation.TrialDivision
    ( trialDivisionWith
    , trialDivisionTo
    , trialDivisionPrimeTo
    ) where

import Math.NumberTheory.Roots
import Math.NumberTheory.Primes.Sieve.Atkin
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Utils

-- | Factorise an 'Integer' using a given list of numbers considered prime.
--   If the list is not a list of primes containing all relevant primes, the
--   result could be surprising.
trialDivisionWith :: [Integer] -> Integer -> [(Integer, Word)]
trialDivisionWith prs n
    | n < 0     = trialDivisionWith prs (-n)
    | n == 0    = error "trialDivision of 0"
    | n == 1    = []
    | otherwise = go n (integerSquareRoot n) prs
      where
        go !m !r (p:ps)
            | r < p = [(m,1)]
            | otherwise =
                case splitOff p m of
                  (0,_) -> go m r ps
                  (k,q) -> (p,k) : if q == 1
                                     then []
                                     else go q (integerSquareRoot q) ps
        go m _ _    = [(m,1)]

-- | @'trialDivisionTo' bound n@ produces a factorisation of @n@ using the
--   primes @<= bound@. If @n@ has prime divisors @> bound@, the last entry
--   in the list is the product of all these. If @n <= bound^2@, this is a
--   full factorisation, but very slow if @n@ has large prime divisors.
trialDivisionTo :: Int -> Integer -> [(Integer, Word)]
trialDivisionTo bd
    | bd < 100  = trialDivisionTo 100
    | otherwise = trialDivisionWith (map (toInteger . unPrime) (atkinFromTo 0 bd))

-- | Check whether a number is coprime to all of the numbers in the list
--   (assuming that list contains only numbers > 1 and is ascending).
trialDivisionPrimeWith :: [Integer] -> Integer -> Bool
trialDivisionPrimeWith prs n
    | n < 0     = trialDivisionPrimeWith prs (-n)
    | n < 2     = False
    | otherwise = go n (integerSquareRoot n) prs
      where
        go !m !r (p:ps) = r < p || m `rem` p /= 0 && go m r ps
        go _ _ _ = True

-- | @'trialDivisionPrimeTo' bound n@ tests whether @n@ is coprime to all primes @<= bound@.
--   If @n <= bound^2@, this is a full prime test, but very slow if @n@ has no small prime divisors.
trialDivisionPrimeTo :: Int -> Integer -> Bool
trialDivisionPrimeTo bd
    | bd < 100      = trialDivisionPrimeTo 100
    | otherwise     = trialDivisionPrimeWith (map (toInteger . unPrime) (atkinFromTo 0 bd))
