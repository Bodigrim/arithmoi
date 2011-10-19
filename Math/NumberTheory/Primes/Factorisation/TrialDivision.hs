-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.TrialDivision
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Factorisation and primality testing using trial division.
--
-- Used to create and check certificates.
{-# LANGUAGE BangPatterns #-}
module Math.NumberTheory.Primes.Factorisation.TrialDivision
    ( trialDivisionWith
    , trialDivisionTo
    , trialDivisionPrimeWith
    , trialDivisionPrimeTo
    ) where

import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Utils

trialDivisionWith :: [Integer] -> Integer -> [(Integer,Int)]
trialDivisionWith prs n
    | n < 0     = trialDivisionWith prs (-n)
    | n == 0    = error "trialDivision of 0"
    | n == 1    = []
    | otherwise = go n (integerSquareRoot' n) prs
      where
        go !m !r (p:ps)
            | r < p     = [(m,1)]
            | otherwise =
                case splitOff p m of
                  (0,_) -> go m r ps
                  (k,q) -> (p,k) : if q == 1
                                     then []
                                     else go q (integerSquareRoot' q) ps
        go m _ _    = [(m,1)]

trialDivisionTo :: Integer -> Integer -> [(Integer,Int)]
trialDivisionTo bd = trialDivisionWith (primeList $ primeSieve bd)

trialDivisionPrimeWith :: [Integer] -> Integer -> Bool
trialDivisionPrimeWith prs n
    | n < 0     = trialDivisionPrimeWith prs (-n)
    | n < 2     = False
    | otherwise = go n (integerSquareRoot' n) prs
      where
        go !m !r (p:ps) = r < p || m `rem` p /= 0 && go m r ps
        go _ _ _ = True

trialDivisionPrimeTo :: Integer -> Integer -> Bool
trialDivisionPrimeTo bd = trialDivisionPrimeWith (primeList $ primeSieve bd)
