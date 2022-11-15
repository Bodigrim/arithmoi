-- Module for Diophantine Equations and related functions

{-# LANGUAGE RecordWildCards, BlockArguments, TypeApplications, PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Math.NumberTheory.Diophantine
  ( cornacchiaPrimitive
  , cornacchia
  , Linear (..)
  , LinearSolution (..)
  , solveLinear
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

-- | A linear diophantine equation `ax + by = c`
-- | where `x` and `y` are unknown
data Linear a = Lin { a,b,c :: a }
  deriving
    ( Show, Eq, Ord
    )

-- | A solution to a linear equation
data LinearSolution a = LS { x,v,y,u :: a }
  deriving
    ( Show, Eq, Ord
    )

-- | Produces an unique solution given any
-- | arbitrary number k
runLinearSolution :: _ => LinearSolution b -> b -> (b, b)
runLinearSolution LS {..} k =
  ( x + k*v, y - k*u )

solveLinear :: _ => Linear a -> Maybe (LinearSolution a)
solveLinear Lin {..} =
    LS {..} <$ guard @Maybe do b /= 0 && q == 0
  where
    (d, e) = gcdExt a b
    (h, q) = divMod c d
    f      = div (a*e-d) (-b)
    (x, y) = (e*h, f*h)
    (u, v) = (quot a d, quot b d)

----

-- https://en.wikipedia.org/wiki/Ku%E1%B9%AD%E1%B9%ADaka
--
-- "Find an integer such that it leaves a remainder of 'r1' when
-- divided by 'a' and a remainder of 'r2' when divided by 'b'
--
data Kuṭṭaka i = Kuṭṭaka { r1,a,r2,b :: i }
  deriving Show

kuṭṭakaLinear :: _ => Kuṭṭaka i -> Linear i
kuṭṭakaLinear Kuṭṭaka{..}
  = Lin {..}
  where
    c = r2 - r1

----

{-
   You are standing on a pogo stick at the surface of a circle of
   circumferance C. A distance D away from you is a piece of candy.
   Your pogo stick can only jump jumps of some integer length L.
   How many hops H does it take to land on the candy?

   Examples:
     (C=10, D=1, L=2) -> no solution
     (C=23, D=3, L=5) -> 19 hops

   D = H*L (mod C)
   D = H*L - T*C
-}

data Pogo i
   = Pogo { l :: i   -- Jump length
          , d :: i   -- Initial distance to the candy
          , c :: i } -- Circle circumference
   deriving
     ( Show, Eq, Ord
     )

pogoLinear Pogo {l=a, c=b, d=c} =
  Lin {..}

solvePogo p@Pogo {..} = do
  let l = pogoLinear p
  s <- solveLinear l
  let (h,_) = runLinearSolution s 0
  pure $ mod h c

----

-- i*s1 == i*s2 + o  (mod c)
--
-- This is like Pogo, but the piece of candy is also moving
--
data Race a = Race
  { c  :: a -- Length of the cycle
  , s1 :: a -- Speed itterator 1
  , s2 :: a -- Speed itterator 2
  , o  :: a -- Offset itterator 2
  }
  deriving Show

raceLinear Race {..} =
  Lin { b=c, c=o, a=s1-s2 }

