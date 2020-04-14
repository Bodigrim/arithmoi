-- Module for Diophantine Equations and related functions

module Math.NumberTheory.Diophantine
  ( cornacchiaPrimitive
  , cornacchia
  )
where

import           Math.NumberTheory.Moduli.Sqrt  ( sqrtsModFactorisation )
import           Math.NumberTheory.Primes       ( factorise
                                                , unPrime
                                                , UniqueFactorisation
                                                )
import           Math.NumberTheory.Roots        ( integerSquareRoot )
import           Math.NumberTheory.Utils.FromIntegral

-- | Finds all primitive solutions (x,y) to the diophantine equation 
-- |    x^2 + d*y^2 = m
-- | when 1 <= d < m and d,m are coprime.
-- | Given m is square free these are all the solutions
cornacchiaPrimitive :: Integer -> Integer -> [(Integer, Integer)]
cornacchiaPrimitive d m
  | not (1 <= d && d < m && gcd d m == 1) = error "pre-conditions not satisfied"
  | otherwise = concatMap
    (findSolution . head . dropWhile (\r -> r * r >= m) . gcdSeq)
    roots
 where
  roots =
    filter (<=m `div` 2) $ sqrtsModFactorisation (m - d) (factorise m)
  gcdSeq = go m where go a b = a : go b (mod a b)
  -- If s = sqrt((m - r*r) / d) is an integer then (r, s) is a solution
  findSolution r = [ (r, s) | rem1 == 0 && s * s == s2 ]
   where
    (s2, rem1) = divMod (m - r * r) d
    s          = integerSquareRoot s2

squareFactors :: UniqueFactorisation a => a -> [a]
squareFactors = foldl squareProducts [1] . factorise
 where
  squareProducts acc f = [ a * b | a <- acc, b <- squarePowers f ]
  squarePowers (p, a) = map (unPrime p ^) [0 .. wordToInt a `div` 2]

-- | Finds all solutions (x,y) to the diophantine equation 
-- |    x^2 + d*y^2 = m
-- | when 1 <= d < m and d,m are coprime.
cornacchia :: Integer -> Integer -> [(Integer, Integer)]
cornacchia d m
  | not (1 <= d && d < m && gcd d m == 1) = error "pre-conditions not satisfied"
  | otherwise = [ s | sf <- squareFactors m, sf <= bound, s <- solve sf ]
 where
  bound = integerSquareRoot (m `div` d)
  solve sf = map (\(x, y) -> (x * sf, y * sf))
    $ cornacchiaPrimitive d (m `div` (sf * sf))
