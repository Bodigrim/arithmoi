module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , SBMatrix(..)
  , mult
  , size
  , linearSolve
  , testLinearSolver
  ) where

import qualified Data.List as L
import qualified Data.IntSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Monad.ST
import Data.Maybe
import Debug.Trace
import Data.Semigroup()
import System.Random
import System.IO.Unsafe
import GHC.Clock
import Data.Bit
import Data.Bits
import Data.Semigroup
import Data.Foldable

-- Sparse Binary Vector
newtype SBVector = SBVector
  { unSBVector :: S.IntSet
  } deriving (Show, Eq)

-- Vectors form a group (in particular a monoid) under addition.
instance Semigroup SBVector where
  SBVector v1 <> SBVector v2 = SBVector ((v1 S.\\ v2) <> (v2 S.\\ v1))

instance Monoid SBVector where
  mempty = SBVector mempty

dot :: SBVector -> SBVector -> Bit
dot (SBVector v1) (SBVector v2) = Bit (even (S.size (v1 `S.intersection` v2)))

-- Sparse Binary Matrix
newtype SBMatrix = SBMatrix (V.Vector SBVector) deriving (Show)

mult :: SBMatrix -> SBVector -> SBVector
mult m@(SBMatrix matrix) (SBVector vector) = runST $ do
  -- it would be better to cache maximal length somewhere
  -- let len = getMax (fromJust (foldMap (fmap (Max . fst) . S.maxView . unSBVector) matrix)) + 1
  vs <- MU.new $ size m
  traverse_ (traverse_ (unsafeFlipBit vs) . S.toList . unSBVector . (matrix V.!)) (S.toList vector)
  ws <- U.unsafeFreeze vs
  pure $ SBVector $ S.fromDistinctAscList $ listBits ws
-- mult (SBMatrix matrix) (SBVector vector) = foldMap (matrix V.!) (S.toList vector)

size :: SBMatrix -> Int
size (SBMatrix m) = V.length m

linearSolve :: SBMatrix -> SBVector
linearSolve matrix = linearSolveHelper 1 matrix randomVectors 0 0
  where
    -- Make sure random vectors are not empty. The floating point number is the sparsity of the random vectors
    randomVectors = getRandomVectors [0..(size matrix - 1)] 0.1 $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

linearSolveHelper :: F2Poly -> SBMatrix -> [SBVector] -> Int -> Int -> SBVector
linearSolveHelper previousPoly matrix (z : x : otherVecs) counter totalCounter
  | potentialSolution == mempty && totalCounter > 99 = error "Fail"
  | potentialSolution == mempty && counter <= 9      = trace ("Counter: " ++ show (counter + 1) ++ "\nTotal Counter: " ++ show (totalCounter + 1)) $ linearSolveHelper potentialMinPoly matrix (z : otherVecs) (counter + 1) (totalCounter + 1)
  | potentialSolution == mempty && counter > 9       = trace ("Counter: " ++ show (counter + 1) ++ "\nTotal Counter: " ++ show (totalCounter + 1)) $ linearSolveHelper 1 matrix otherVecs 0 (totalCounter + 1)
  -- This is a good solution.
  | otherwise                                        = trace ("Counter: " ++ show (counter + 1) ++ "\nTotal Counter: " ++ show (totalCounter + 1)) $ potentialSolution
  where
    potentialSolution = findSolution singularities matrix almostZeroVector
    almostZeroVector = evaluate matrix z reducedMinPoly
    (singularities, reducedMinPoly) = L.break (== Bit True) (U.toList $ unF2Poly potentialMinPoly)
    -- lowest common multiple of previousPoly and candidateMinPoly
    potentialMinPoly = lcm previousPoly candidateMinPoly
    candidateMinPoly = findCandidatePoly matrix z x

findSolution :: [Bit] -> SBMatrix -> SBVector -> SBVector
findSolution [] _ _ = mempty
findSolution (_ : xs) matrix vector
  | result == mempty = vector
  | otherwise        = findSolution xs matrix result
  where
    result = matrix `mult` vector

-- This routine takes a polynomial p, a matrix A and a vector w and
-- returns p(A)w. It assumes the first coefficient of p is non zero,
-- in particular that it is non empty. This makes the implementation
-- easier as there is no need to write the identity matrix.
evaluate :: SBMatrix -> SBVector -> [Bit] -> SBVector
evaluate matrix w = foldr (\coeff acc -> (matrix `mult` acc) <> (if unBit coeff then w else mempty)) mempty

findCandidatePoly :: SBMatrix -> SBVector -> SBVector -> F2Poly
findCandidatePoly matrix z x = berlekampMassey dim errorPoly randomSequence
  where
    randomSequence = generateData matrix z x
    errorPoly = fromInteger (1 `shiftL` (2 * dim)) :: F2Poly
    dim = size matrix

berlekampMassey :: Int -> F2Poly -> F2Poly -> F2Poly
berlekampMassey dim = go 1 0
  where
    -- Is there a better way to implement recursion in this situation?
    go :: F2Poly -> F2Poly -> F2Poly ->F2Poly -> F2Poly
    go oneBefore twoBefore a b
      | U.length (unF2Poly b) <= dim = oneBefore
      -- Updated value is given by @twoBefore - oneBefore * q@
      | otherwise                    = go (twoBefore - oneBefore * q) oneBefore b r
        where
          (q, r) = quotRem a b

-- The input is a matrix B and a random vector z
generateData :: SBMatrix -> SBVector -> SBVector -> F2Poly
generateData matrix z x = toF2Poly $ U.fromList $ reverse $ map (x `dot`) matrixPowers
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z

-- Infinite lists of random vectors
getRandomVectors :: [Int] -> Double -> StdGen -> [SBVector]
getRandomVectors rows sparsity gen = go randomEntries
  where
    randomEntries = zip (cycle rows) (randomRs (0, 1) gen)
    go :: [(Int, Double)] -> [SBVector]
    go list = newVector : go backOfList
      where
        newVector = SBVector (S.fromList listOfEntries)
        listOfEntries = fmap fst $ filter (\(_, rDouble) -> rDouble < sparsity) frontOfList
        (frontOfList, backOfList) = L.splitAt (length rows) list

-- Input number of columns of matrix and sparsity coefficient. It returns a random matrix.
testLinearSolver :: Int -> Double -> Bool
testLinearSolver dim sparsity = mat `mult` sol == mempty
  where
    sol = linearSolve mat
    mat = SBMatrix (V.fromList listOfColumns)
    -- -2 is arbitrary. It means that the number of rows is at most one less than
    -- the number of columns
    listOfColumns = L.take dim $ getRandomVectors [0..(dim - 2)] sparsity $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

-- 1) Investigate rare cases where code does not terminates.
-- 2) Implement dense matrices rather than sparse.
-- 3) See if using direct computations in BerlekampMassey makes it faster.
