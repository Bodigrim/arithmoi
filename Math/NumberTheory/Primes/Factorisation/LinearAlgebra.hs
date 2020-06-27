module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , SBMatrix(..)
  , mult
  , linearSolve
  , testLinearSolver
  ) where

import Data.Semigroup()
import System.Random
import Debug.Trace
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import qualified Data.IntSet as S
import qualified Data.IntMap as I
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

-- Sparse Binary Matrix
newtype SBMatrix = SBMatrix (I.IntMap SBVector) deriving (Show)

mult :: SBMatrix -> SBVector -> SBVector
mult (SBMatrix matrix) (SBVector vector) = foldMap (matrix I.!) (S.toList vector)

size :: SBMatrix -> Int
size (SBMatrix m) = I.size m

linearSolve :: SBMatrix -> SBVector
linearSolve matrix@(SBMatrix m) = trace ("Is estimate of minimal polynomial good? " ++ show (length singularities > 0)) $ findSolution singularities almostZeroVector
  where
    z = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
    randomSequence = generateData matrix z
    dim = size matrix
    errorPoly = fromInteger (1 `shiftL` (2*dim)) :: F2Poly
    -- Is it better to convert to a list straight away?
    minPoly = berlekampMassey dim errorPoly randomSequence
    -- Highest n such that x^n | minPoly
    (singularities, reducedMinPoly) = L.break (== Bit True) (U.toList $ unF2Poly minPoly)
    -- If @singularities@ has positive length, then a generic w should work.
    -- It should be changed until one is reached.
    w = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
    almostZeroVector = evaluate reducedMinPoly matrix w
    findSolution :: [Bit] -> SBVector -> SBVector
    findSolution [] _ = error "Linear Algebra failed."
    findSolution (_ : xs) vector = if result == mempty then vector else findSolution xs result
      where
        result = matrix `mult` vector

-- The input is a matrix B and a random vector z
generateData :: SBMatrix -> SBVector -> F2Poly
generateData matrix z = toF2Poly $ U.fromList $ reverse $ map (\v -> Bit (not $ S.null (set (x `mult` v)))) matrixPowers
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z
    x = SBMatrix (foldr (\p acc -> I.insert p (SBVector (S.singleton 1)) acc) initialMap randomPrimes)
    randomPrimes = randomSublist primes (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))
    initialMap = I.fromList ([(p, mempty) | p <- primes])
    -- This should be replaced by factorBase = [nextPrime 2..precPrime b]
    -- This assumes rows are indexed in the same way as columns are.
    primes = [1..(size matrix)]

randomSublist :: [Int] -> StdGen -> [Int]
randomSublist list gen = fmap fst $ filter snd $ zip list (randoms gen)

berlekampMassey :: Int -> F2Poly -> F2Poly -> F2Poly
berlekampMassey dim = go 1 0
  where
    -- Is there a better way to implement recursion in this situation?
    go :: F2Poly -> F2Poly -> F2Poly ->F2Poly -> F2Poly
    go oneBefore twoBefore a b
      | U.length (unF2Poly b) <= dim = oneBefore
      -- Updated value is given by @twoBefore - oneBefore * q@
      | otherwise                        = go (twoBefore - oneBefore * q) oneBefore b r
        where
          (q, r) = quotRem a b

-- This routine takes a polynomial p, a matrix A and a vector w and
-- returns p(A)w. It assumes the first coefficient of p is non zero,
-- in particular that it is non empty. This makes the implementation
-- easier as there is no need to write the identity matrix.
evaluate :: [Bit] -> SBMatrix -> SBVector -> SBVector
evaluate polynomial matrix w = go (tail polynomial) w w
  where
    go :: [Bit] -> SBVector -> SBVector -> SBVector
    go newPoly result previousVector = case null newPoly of
      True  -> result
      False -> go (tail nonZeroPart) newResult newVector
      where
        (zeroPart, nonZeroPart) = L.break (== Bit True) newPoly
        newVector = foldl (\acc _ -> matrix `mult` acc) (matrix `mult` previousVector) zeroPart
        newResult = result <> newVector

-- Informal way of testing large matrices
-- Input number of columns of matrix and sparsity coefficient
testLinearSolver :: Int -> Double -> [Int]
testLinearSolver dim sparsity = S.toList . set $ linearSolve matrix
  where
    -- 5 is arbitraty
    numberOfRows = dim - 5
    randomColumn :: StdGen -> [Int]
    randomColumn gen = fmap fst $ filter (\(_, rDouble) -> rDouble < sparsity) $ zip [1..numberOfRows] (randomRs (0, 1) gen)
    listMatrix = map (\index -> (index, SBVector (S.fromList $ randomColumn (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))))) [1..dim]
    matrix = SBMatrix (I.fromList listMatrix)

-- IMPROVEMENTS
-- 1) Once we find a good estimate for the minimum polynomial, try different random vectors w.
-- 2) Try different x and z and take lcm of polynomials obtained.
-- 3) The algorithm is quite slow. Probably it's good to speed up matrix multiplication. Maybe I.! is inefficient.
