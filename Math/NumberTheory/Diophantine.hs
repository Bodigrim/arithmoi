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

-- | See `cornacchiaPrimitive`, this has the additional constraint of coprime input
cornacchiaPrimitive' :: Integer -> Integer -> [(Integer, Integer)]
cornacchiaPrimitive' d m =
  concatMap
    (findSolution . head . dropWhile (\r -> r * r >= m) . gcdSeq m)
    roots
 where
  roots =
    filter (<= m `div` 2) $ sqrtsModFactorisation (m - d) (factorise m)
  gcdSeq a b = a : gcdSeq b (mod a b)
  -- If s = sqrt((m - r*r) / d) is an integer then (r, s) is a solution
  findSolution r = [ (r, s) | rem1 == 0 && s * s == s2 ]
   where
    (s2, rem1) = divMod (m - r * r) d
    s          = integerSquareRoot s2

-- | Finds all primitive solutions (x,y) to the diophantine equation 
-- |    x^2 + d*y^2 = m
-- | when 1 <= d < m. Given m is square free these are all the solutions
cornacchiaPrimitive :: Integer -> Integer -> [(Integer, Integer)]
cornacchiaPrimitive d m
  | not (1 <= d && d < m) = error "pre-conditions not satisfied"
  | gRoot * gRoot /= g || not (1 <= d' && d' < m') = []
  | otherwise = map (\(x, y) -> (gRoot * x, y)) (cornacchiaPrimitive' d' m')
 where
  g     = gcd d m
  gRoot = integerSquareRoot g
  d'    = d `div` g
  m'    = m `div` g

-- Find numbers whose square is a factor of the input
squareFactors :: UniqueFactorisation a => a -> [a]
squareFactors = foldl squareProducts [1] . factorise
 where
  squareProducts acc f = [ a * b | a <- acc, b <- squarePowers f ]
  squarePowers (p, a) = map (unPrime p ^) [0 .. wordToInt a `div` 2]

-- | Finds all solutions (x,y) to the diophantine equation 
-- |    x^2 + d*y^2 = m
-- | when 1 <= d < m
cornacchia :: Integer -> Integer -> [(Integer, Integer)]
cornacchia d m
  | not (1 <= d && d < m) = error "pre-conditions not satisfied"
  | otherwise = concatMap solve $ filter ((>d) . snd) candidates
 where
  candidates = map (\sf -> (sf, m `div` (sf * sf))) (squareFactors m)
  solve (sf, m') = map (\(x, y) -> (x * sf, y * sf)) (cornacchiaPrimitive d m')
