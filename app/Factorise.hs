module Main where

-- import System.Environment

import Math.NumberTheory.Primes

n1 :: Integer
n1 = 100000000000000000039 * 10000000000000000000000013

n2 :: Integer
n2 = 1000000000000000000000000000057 * 1000000000000000000013471284999

main :: IO ()
main = do
  -- x : _ <- getArgs
  print $ factorise $ n1 -- 2 ^ (2 ^ read x) + 1
