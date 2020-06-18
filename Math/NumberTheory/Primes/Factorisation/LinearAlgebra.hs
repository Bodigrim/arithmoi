module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , SBMatrix(..)
  , linearSolve
  , firstStep
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

-- The input is a matrix B and a random vector z
firstStep :: SBMatrix -> SBVector -> [SBVector]
firstStep m z = map (x `mult`) matrixPowers
  where
    matrixPowers = L.iterate (m `mult`) z
    x = SBMatrix (I.singleton 1 (SBVector S.empty)) -- Random matrix of size 1 * (largest Prime)

linearSolve :: SBMatrix -> Int -> [SBVector]
linearSolve m t = [z]
  where
    seed = mkStdGen 68431698431
    z = SBVector (S.fromList (L.take 100 (randomRs (0,t) seed)))
    -- firstStep m z
