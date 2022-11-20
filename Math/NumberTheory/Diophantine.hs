-- Module for Diophantine Equations and related functions

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Math.NumberTheory.Diophantine
  ( cornacchiaPrimitive
  , cornacchia
  , LinearSolution (..)
  , linear
  , runLinearSolution
  )
where

import           Math.NumberTheory.Moduli.Sqrt  ( sqrtsModFactorisation )
import           Math.NumberTheory.Primes       ( factorise
                                                , unPrime
                                                , UniqueFactorisation
                                                )
import           Math.NumberTheory.Roots        ( integerSquareRoot )
import           Math.NumberTheory.Utils.FromIntegral

import           Control.Monad                  (guard)
import           Data.Euclidean                 (gcdExt)

-- | See `cornacchiaPrimitive`, this is the internal algorithm implementation
-- | as described at https://en.wikipedia.org/wiki/Cornacchia%27s_algorithm 
cornacchiaPrimitive' :: Integer -> Integer -> [(Integer, Integer)]
cornacchiaPrimitive' d m = concatMap
  (findSolution . head . dropWhile (\r -> r * r >= m) . gcdSeq m)
  roots
 where
  roots = filter (<= m `div` 2) $ sqrtsModFactorisation (m - d) (factorise m)
  gcdSeq a b = a : gcdSeq b (mod a b)
  -- If s = sqrt((m - r*r) / d) is an integer then (r, s) is a solution
  findSolution r = [ (r, s) | rem1 == 0 && s * s == s2 ]
   where
    (s2, rem1) = divMod (m - r * r) d
    s          = integerSquareRoot s2

-- | Finds all primitive solutions (x,y) to the diophantine equation 
-- |    x^2 + d*y^2 = m
-- | when 1 <= d < m and gcd(d,m)=1
-- | Given m is square free these are all the positive integer solutions
cornacchiaPrimitive :: Integer -> Integer -> [(Integer, Integer)]
cornacchiaPrimitive d m
  | not (1 <= d && d < m) = error "precondition failed: 1 <= d < m"
  | gcd d m /= 1          = error "precondition failed: d and m coprime"
  |
  -- If d=1 then the algorithm doesn't generate symmetrical pairs 
    d == 1                = concatMap genPairs solutions
  | otherwise             = solutions
 where
  solutions = cornacchiaPrimitive' d m
  genPairs (x, y) = if x == y then [(x, y)] else [(x, y), (y, x)]

-- Find numbers whose square is a factor of the input
squareFactors :: UniqueFactorisation a => a -> [a]
squareFactors = foldl squareProducts [1] . factorise
 where
  squareProducts acc f = [ a * b | a <- acc, b <- squarePowers f ]
  squarePowers (p, a) = map (unPrime p ^) [0 .. wordToInt a `div` 2]

-- | Finds all positive integer solutions (x,y) to the
-- | diophantine equation:
-- |    x^2 + d*y^2 = m
-- | when 1 <= d < m and gcd(d,m)=1
cornacchia :: Integer -> Integer -> [(Integer, Integer)]
cornacchia d m
  | not (1 <= d && d < m) = error "precondition failed: 1 <= d < m"
  | gcd d m /= 1 = error "precondition failed: d and m coprime"
  | otherwise = concatMap solve $ filter ((> d) . snd) candidates
 where
  candidates = map (\sf -> (sf, m `div` (sf * sf))) (squareFactors m)
  solve (sf, m') = map (\(x, y) -> (x * sf, y * sf)) (cornacchiaPrimitive d m')

----

-- | A solution to a linear equation
data LinearSolution a = LS { base1,base2,scale1,scale2 :: a }
  deriving
    ( Show, Eq, Ord
    )

-- | Solves a linear diophantine equation
-- |   ax + by = c
-- | where `x` and `y` are unknown
linear :: _ => a -> a -> a -> Maybe (LinearSolution a)
linear a b c =
    LS x y v u <$ guard (b /= 0 && q == 0)
  where
    (d, e) = gcdExt a b
    (h, q) = divMod c d
    f      = div (a*e-d) (-b)
    (x, y) = (e*h, f*h)
    (u, v) = (quot a d, quot b d)

-- | Produces an unique solution given any
-- | arbitrary number k
runLinearSolution :: _ => LinearSolution a -> a -> (a, a)
runLinearSolution (LS x y v u) k =
  ( x + k*v, y - k*u )

