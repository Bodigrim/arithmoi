module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , SBMatrix(..)
  , linearSolve
  ) where

import Data.Semigroup ()
import System.Random
import qualified Data.List as L
import qualified Data.IntSet as S
import qualified Data.IntMap as I

-- Sparse Binary Vector
data SBVector = SBVector S.IntSet deriving (Show)

-- Vectors form a group (in particular a monoid) under addition.
instance Semigroup SBVector where
  SBVector v1 <> SBVector v2 = SBVector ((v1 S.\\ v2) <> (v2 S.\\ v1))

instance Monoid SBVector where
  mempty = SBVector S.empty

-- Sparse Binary Matrix
data SBMatrix = SBMatrix (I.IntMap SBVector) deriving (Show)

mult :: SBMatrix -> SBVector -> SBVector
mult (SBMatrix matrix) (SBVector vector) = foldMap (matrix I.!) (S.toList vector)

-- Multiplication of matrices
instance Semigroup SBMatrix where
  m1 <> (SBMatrix m2) = SBMatrix (I.map (m1 `mult`) m2)

size :: SBMatrix -> Int
size (SBMatrix m) = I.size m

randomSublist :: [Int] -> Double -> Int -> [Int]
randomSublist list sparsity gen = map (list !!) randomIndices
  where
    randomIndices = L.take numberElems (randomRs (0, l - 1) seed)
    seed = mkStdGen gen
    numberElems = floor $ sparsity * fromIntegral l
    l = length list

linearSolve :: SBMatrix -> [SBVector]
linearSolve matrix@(SBMatrix m) = firstStep matrix z
  where
    -- Rows of z are indexed by the columns of matrix
    z = SBVector (S.fromList $ randomSublist (I.keys m) 0.4 43081)

-- The input is a matrix B and a random vector z
firstStep :: SBMatrix -> SBVector -> [SBVector]
firstStep matrix z = map (x `mult`) matrixPowers
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z
    x = SBMatrix (foldr (\p acc -> I.insert p (SBVector (S.singleton 1)) acc) initialMap randomPrimes)
     -- This should be replaced by factorBase = [nextPrime 2..precPrime b]
    randomPrimes = randomSublist primes 0.4 19485
    initialMap = I.fromList ([(p, SBVector S.empty) | p <- primes])
    primes = [1..5]

-- secondStep :: 


-- let matrix = SBMatrix (I.fromList [(1, SBVector (S.fromList [1])), (2, SBVector (S.fromList [2])), (3, SBVector (S.fromList [3])), (4, SBVector (S.fromList [4])), (5, SBVector (S.fromList [5]))])
