module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  (
  ) where

import Data.Semigroup ()
import Data.List
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
  m1 <> (SBMatrix m2) = SBMatrix (I.map (mult m1) m2)

size :: SBMatrix -> Int
size (SBMatrix m) = I.size m

-- -- The input is a matrix B and a random vector z
-- firstStep :: SBMatrix -> SBVector -> [SBMatrix]
-- firstStep matrix z = map (x *) matrixPowers
--   where
--     matrixPowers = L.iterate
--     x = -- Random vector
--     l = 2 * size matrix
