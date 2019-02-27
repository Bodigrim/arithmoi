module Main where

import Math.NumberTheory.Primes
import Math.NumberTheory.Recurrences.Bilinear (binomialDiagonal)
import Data.Time.Clock
import Data.List
import Data.Maybe

-- Implementation by https://github.com/CarlEdman
foo4 :: [a] -> [[[a]]]
foo4 = scanl' go [[]] . zip (map binomialDiagonal [0..]) . init . tails
  where
    go p (bs, as) = [q : qs| (q, b) <- zip as bs, qs <- take b p]

-- List of primes must be strictly ascending.
countCoprimes :: Integral a => [Prime a] -> a -> a
countCoprimes [] n = n
countCoprimes ps n
  = sum
  $ zipWith (*) (iterate negate 1)
  $ map (sum . map (n `quot`))
  $ ([1] :)
  $ takeWhile (not . null)
  $ map (filter (<= n))
  $ map (map $ product . map unPrime)
  $ map cutoff
  $ tail
  $ foo4 ps
  where
    cutoff [] = []
    cutoff xss@(xs : _) = takeWhile predicate xss
      where
        lim = n `quot` product (map unPrime $ tail xs)
        predicate [] = True
        predicate (x : _) = unPrime x <= lim

foo5 :: b -> (a -> b -> Maybe b) -> [a] -> [[b]]
foo5 empty cons = scanl' go [empty] . zip (map binomialDiagonal [0..]) . init . tails
  where
    go p (bs, as) = catMaybes [ q `cons` qs | (q, b) <- zip as bs, qs <- take b p]

-- List of primes must be strictly ascending.
countCoprimes2 :: (Show a, Integral a) => [Prime a] -> a -> a
countCoprimes2 [] n = n
countCoprimes2 ps n
  = sum
  $ zipWith (*) (iterate negate 1)
  $ map (sum . map (n `quot`))
  $ takeWhile (not . null)
  $ foo5 1 cons
  $ takeWhile (\p -> unPrime p <= n) ps
  where
    cons x y = if xy > n then Nothing else Just xy
      where
        xy = unPrime x * y

limit :: Int
limit = 1000000

main :: IO ()
main = do
  t0 <- getCurrentTime
  print $ countCoprimes [nextPrime 5, nextPrime 17 ..] limit
  t1 <- getCurrentTime
  putStrLn "countCoprimes"
  print $ diffUTCTime t1 t0

  t0' <- getCurrentTime
  print $ countCoprimes2 [nextPrime 5, nextPrime 17 ..] limit
  t1' <- getCurrentTime
  putStrLn "countCoprimes2"
  print $ diffUTCTime t1' t0'

  t0'' <- getCurrentTime
  print $ length $ filter (\n -> all (\(p, _) -> unPrime p `mod` 12 /= 5) (factorise n)) [1..limit]
  t1'' <- getCurrentTime
  putStrLn "filter . factorise"
  print $ diffUTCTime t1'' t0''

