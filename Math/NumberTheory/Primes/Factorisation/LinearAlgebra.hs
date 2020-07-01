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
import qualified Data.IntMap as I
import qualified Data.Vector.Unboxed as U
import Debug.Trace
import Data.Semigroup()
import System.Random
import System.IO.Unsafe
import GHC.Clock
import Data.Bit
import Data.Bits

-- Sparse Binary Vector
newtype SBVector = SBVector
  { set :: S.IntSet
  } deriving (Show, Eq)

-- Vectors form a group (in particular a monoid) under addition.
instance Semigroup SBVector where
  SBVector v1 <> SBVector v2 = SBVector ((v1 S.\\ v2) <> (v2 S.\\ v1))

instance Monoid SBVector where
  mempty = SBVector mempty

dot :: SBVector -> SBVector -> Bit
dot (SBVector v1) (SBVector v2) = if even (S.size (v1 `S.intersection` v2)) then Bit True else Bit False
-- Sparse Binary Matrix
newtype SBMatrix = SBMatrix (I.IntMap SBVector) deriving (Show)

mult :: SBMatrix -> SBVector -> SBVector
mult (SBMatrix matrix) (SBVector vector) = foldMap (matrix I.!) (S.toList vector)

size :: SBMatrix -> Int
size (SBMatrix m) = I.size m

linearSolve :: SBMatrix -> SBVector
linearSolve matrix = linearSolveHelper 1 matrix randomVectors
  where
    -- Make sure random vectors are not empty.
    -- Make sure the rows have correct index
    randomVectors = getRandomVectors [1..(size matrix - 1)] (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))

linearSolveHelper :: F2Poly -> SBMatrix -> [SBVector] -> SBVector
linearSolveHelper previousPoly matrix (z : x : otherVecs)
  | potentialSolution == mempty = trace ("Bad z: " ++ show z ++ "\nBad x: " ++ show x) $ linearSolveHelper 1 matrix otherVecs
  -- This is a good solution.
  | otherwise                   = potentialSolution
  where
    potentialSolution = findSolution singularities matrix almostZeroVector
    almostZeroVector = evaluate matrix z reducedMinPoly
    (singularities, reducedMinPoly) = L.break (== Bit True) (U.toList $ unF2Poly candidateMinPoly)
    -- lowest common multiple of previousPoly and candidateMinPoly CHECK
    -- potentialMinPoly = lcm previousPoly candidateMinPoly
    candidateMinPoly = findCandidatePoly matrix z x

findSolution :: [Bit] -> SBMatrix -> SBVector -> SBVector
findSolution [] _ _ = mempty
findSolution (_ : xs) matrix vector = if result == mempty then vector else findSolution xs matrix result
  where
    result = matrix `mult` vector

-- This routine takes a polynomial p, a matrix A and a vector w and
-- returns p(A)w. It assumes the first coefficient of p is non zero,
-- in particular that it is non empty. This makes the implementation
-- easier as there is no need to write the identity matrix.
evaluate :: SBMatrix -> SBVector -> [Bit] -> SBVector
evaluate matrix w = foldr (\coeff acc -> (matrix `mult` acc) <> (if coeff == Bit True then w else mempty)) mempty

findCandidatePoly :: SBMatrix -> SBVector -> SBVector -> F2Poly
findCandidatePoly matrix z x = berlekampMassey dim errorPoly randomSequence
  where
    randomSequence = generateData matrix z x
    errorPoly = fromInteger (1 `shiftL` (2*dim)) :: F2Poly
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
generateData matrix z x = toF2Poly $ U.fromList $ reverse $ traceShowId $ map (x `dot`) matrixPowers
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z
    -- Make sure x is not empty.
    -- Check matrix indices
    -- trace ("Size of x: " ++ (show ((I.foldr (\entry acc -> acc + (if entry == mempty then 0 else 1)) 0 ix) :: Int))) $

-- Infinite lists of random vectors
getRandomVectors :: [Int] -> StdGen -> [SBVector]
getRandomVectors rows gen = go randomEntries
  where
    randomEntries = zip (cycle rows) (randoms gen)
    go :: [(Int, Bool)] -> [SBVector]
    go list = newVector : go backOfList
      where
        newVector = SBVector (S.fromList listOfEntries)
        listOfEntries = fmap fst $ filter snd frontOfList
        (frontOfList, backOfList) = L.splitAt (length rows) list

-- Informal way of testing large matrices
-- Input number of columns of matrix and sparsity coefficient
testLinearSolver :: Int -> Double -> [Int]
testLinearSolver dim sparsity = S.toList . set $ linearSolve matrix
  where
    numberOfRows = dim - 1
    randomColumn :: StdGen -> [Int]
    randomColumn gen = fmap fst $ filter (\(_, rDouble) -> rDouble < sparsity) $ zip [1..numberOfRows] (randomRs (0, 1) gen)
    listMatrix = snd $ L.mapAccumR choose (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))) [1..dim]
    choose :: StdGen -> Int -> (StdGen, (Int, SBVector))
    choose seed index = (mkStdGen (fst (random seed)), (index, SBVector (S.fromList (randomColumn seed))))
    matrix = SBMatrix (I.fromList listMatrix)
    -- trace ("Number of on-zero entries: " ++ show (foldr (\vec acc -> acc + (S.size (set vec))) 0 (fmap snd listMatrix))) $

-- 1) Correct random matrix
-- 2) Correct random initial vectors
-- 3) Think about matrix indices
-- 4) Facilitate for monoids

-----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------

-- module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
--   ( SBVector(..)
--   , SBMatrix(..)
--   , mult
--   , linearSolve
--   , testLinearSolver
--   ) where
--
-- import Data.Semigroup()
-- import System.Random
-- import Debug.Trace
-- import qualified Data.List as L
-- import qualified Data.Vector.Unboxed as U
-- import qualified Data.IntSet as S
-- import qualified Data.IntMap as I
-- import System.IO.Unsafe
-- import GHC.Clock
-- import Data.Bit
-- import Data.Bits
--
-- -- Sparse Binary Vector
-- newtype SBVector = SBVector
--   { set :: S.IntSet
--   } deriving (Show, Eq)
--
-- -- Vectors form a group (in particular a monoid) under addition.
-- instance Semigroup SBVector where
--   SBVector v1 <> SBVector v2 = SBVector ((v1 S.\\ v2) <> (v2 S.\\ v1))
--
-- instance Monoid SBVector where
--   mempty = SBVector mempty
--
-- -- Sparse Binary Matrix
-- newtype SBMatrix = SBMatrix (I.IntMap SBVector) deriving (Show)
--
-- mult :: SBMatrix -> SBVector -> SBVector
-- mult (SBMatrix matrix) (SBVector vector) = foldMap (matrix I.!) (S.toList vector)
--
-- size :: SBMatrix -> Int
-- size (SBMatrix m) = I.size m
--
-- linearSolve :: SBMatrix -> SBVector
-- -- If (length singularities > 0), there is no guarantee that the algorithm is
-- -- going to work. To have absolute certainty, one would need to check that the
-- -- polynomial annihilates the sequence matrix ^ k `mult` z, however this seems
-- -- expensive and, in my view, it is quicker to find another solution.
-- linearSolve matrix@(SBMatrix m) = trace ("Is estimate of minimal polynomial good? " ++ show (length singularities > 0)) $ findSolution singularities almostZeroVector
--   where
--     z = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
--     randomSequence = generateData matrix z
--     dim = size matrix
--     errorPoly = fromInteger (1 `shiftL` (2*dim)) :: F2Poly
--     -- Is it better to convert to a list straight away?
--     minPoly = berlekampMassey dim errorPoly randomSequence
--     -- Highest n such that x^n | minPoly
--     (singularities, reducedMinPoly) = L.break (== Bit True) (U.toList $ unF2Poly minPoly)
--     -- If @singularities@ has positive length, then a generic w should work.
--     -- It should be changed until one is reached.
--     almostZeroVector = evaluate matrix z reducedMinPoly
--     -- This can be made clearer. The solution is always found at the very last
--     -- iteration (when xs == []).
--     findSolution :: [Bit] -> SBVector -> SBVector
--     findSolution [] _ = error "Linear Algebra failed."
--     findSolution (_ : xs) vector = if result == mempty then vector else findSolution xs result
--       where
--         result = matrix `mult` vector
--
-- -- The input is a matrix B and a random vector z
-- generateData :: SBMatrix -> SBVector -> F2Poly
-- generateData matrix z = toF2Poly $ U.fromList $ reverse $ map (\v -> Bit (not $ S.null (set (x `mult` v)))) matrixPowers
--   where
--     matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z
--     x = SBMatrix (foldr (\p acc -> I.insert p (SBVector (S.singleton 1)) acc) initialMap randomPrimes)
--     randomPrimes = randomSublist primes (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))
--     initialMap = I.fromList ([(p, mempty) | p <- primes])
--     -- This should be replaced by factorBase = [nextPrime 2..precPrime b]
--     -- This assumes rows are indexed in the same way as columns are.
--     primes = [1..(size matrix)]
--
-- randomSublist :: [Int] -> StdGen -> [Int]
-- randomSublist list gen = fmap fst $ filter snd $ zip list (randoms gen)
--
-- berlekampMassey :: Int -> F2Poly -> F2Poly -> F2Poly
-- berlekampMassey dim = go 1 0
--   where
--     -- Is there a better way to implement recursion in this situation?
--     go :: F2Poly -> F2Poly -> F2Poly ->F2Poly -> F2Poly
--     go oneBefore twoBefore a b
--       | U.length (unF2Poly b) <= dim = oneBefore
--       -- Updated value is given by @twoBefore - oneBefore * q@
--       | otherwise                    = go (twoBefore - oneBefore * q) oneBefore b r
--         where
--           (q, r) = quotRem a b
--
-- -- This routine takes a polynomial p, a matrix A and a vector w and
-- -- returns p(A)w. It assumes the first coefficient of p is non zero,
-- -- in particular that it is non empty. This makes the implementation
-- -- easier as there is no need to write the identity matrix.
-- evaluate :: SBMatrix -> SBVector -> [Bit] -> SBVector
-- evaluate matrix w = foldr (\coeff acc -> (matrix `mult` acc) <> (if coeff == Bit True then w else mempty)) mempty
--
-- -- Informal way of testing large matrices
-- -- Input number of columns of matrix and sparsity coefficient
-- testLinearSolver :: Int -> Double -> [Int]
-- testLinearSolver dim sparsity = S.toList . set $ linearSolve matrix
--   where
--     -- 5 is arbitraty
--     numberOfRows = dim - 5
--     randomColumn :: StdGen -> [Int]
--     randomColumn gen = fmap fst $ filter (\(_, rDouble) -> rDouble < sparsity) $ zip [1..numberOfRows] (randomRs (0, 1) gen)
--     listMatrix = map (\index -> (index, SBVector (S.fromList $ randomColumn (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))))) [1..dim]
--     matrix = SBMatrix (I.fromList listMatrix)

-- randomMatrix :: Int -> Double -> SBMatrix
-- randomMatrix dim sparsity = SBMatrix (I.fromList listMatrix)
--   where
--     listMatrix = map (\index -> (index, SBVector (S.fromList $ randomColumn (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))))) [1..dim]
--     randomColumn :: StdGen -> [Int]
--     randomColumn gen = fmap fst $ filter (\(_, rDouble) -> rDouble < sparsity) $ zip [1..numberOfRows] (randomRs (0, 1) gen)
--     numberOfRows = dim - 1
--
-- randomVector :: Int -> SBVector
-- randomVector dim = SBVector (S.fromList $ randomSublist [1..dim] (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
