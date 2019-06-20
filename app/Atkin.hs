module Main where

import Math.NumberTheory.Primes.Sieve

atkin :: (Int, Int) -> Int
atkin (p, q) = sum $ atkinPrimeList $ atkinSieve p q

main :: IO ()
main = print $ atkin (10000000000,100000000)
