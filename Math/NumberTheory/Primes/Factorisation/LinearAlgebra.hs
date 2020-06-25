module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , SBMatrix(..)
  , testMatrix
  , mult
  , linear --linearSolve
  ) where

import Data.Semigroup
import System.Random
import Debug.Trace
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.IntSet as S
import qualified Data.IntMap as I
import System.IO.Unsafe
import GHC.Clock
import Data.Bit
import qualified Data.Semiring as S

-- Sparse Binary Vector
newtype SBVector = SBVector
  { set :: S.IntSet
  } deriving (Show)

-- Vectors form a group (in particular a monoid) under addition.
instance Semigroup SBVector where
  SBVector v1 <> SBVector v2 = SBVector ((v1 S.\\ v2) <> (v2 S.\\ v1))

instance Monoid SBVector where
  mempty = SBVector mempty

-- Sparse Binary Matrix
newtype SBMatrix = SBMatrix (I.IntMap SBVector) deriving (Show)

mult :: SBMatrix -> SBVector -> SBVector
mult (SBMatrix matrix) (SBVector vector) = foldMap (matrix I.!) (S.toList vector)

-- Multiplication of matrices
instance Num SBMatrix where
  (+) (SBMatrix m1) (SBMatrix m2) = SBMatrix (I.unionWith (<>) m1 m2)
  (*) m1 (SBMatrix m2) = SBMatrix (I.map (m1 `mult`) m2)
  negate = id

instance Semigroup SBMatrix where
  (<>) = (*)

size :: SBMatrix -> Int
size (SBMatrix m) = I.size m

randomSublist :: [Int] -> StdGen -> [Int]
randomSublist list gen = fmap fst $ filter snd $ zip list (randoms gen)

-- linearSolve :: SBMatrix -> SBVector
-- linearSolve matrix@(SBMatrix m) = findSolution $ almostZeroMatrix `mult` w
--   where
--     -- Rows of z are indexed by the columns of matrix
--     z = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
--     randomSequence = generateData matrix z
--     -- The order of the arguments is exchanged
--     minPoly = snd (gcdExt randomSequence (SBPolynomial (True : (L.replicate (2 * (size matrix)) False))))
--     reducedMinPoly = SBPolynomial (L.dropWhileEnd (not . id) (polynomial minPoly))
--     almostZeroMatrix = evaluate reducedMinPoly matrix
--     w = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
--     findSolution :: SBVector -> SBVector
--     findSolution v
--       | set (matrix `mult` v) == mempty = v
--       | otherwise                       = findSolution $ matrix `mult` vList

linear :: SBMatrix -> F2Poly
linear matrix@(SBMatrix m) = minPoly
  where
    z = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
    randomSequence = generateData matrix z
    errorPoly = toF2Poly $ U.update (U.replicate (2 * (size matrix) + 1) (Bit False)) (U.singleton (2 * (size matrix), Bit True))
    minPoly = snd $ gcdExt randomSequence errorPoly

-- The input is a matrix B and a random vector z
generateData :: SBMatrix -> SBVector -> F2Poly
generateData matrix z = toF2Poly $ traceShowId $ U.fromList $ reverse $ map (\v -> Bit (not $ S.null (set (x `mult` v)))) matrixPowers
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z
    x = SBMatrix (foldr (\p acc -> I.insert p (SBVector (S.singleton 1)) acc) initialMap randomPrimes)
    randomPrimes = randomSublist primes (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))
    initialMap = I.fromList ([(p, mempty) | p <- primes])
    -- This should be replaced by factorBase = [nextPrime 2..precPrime b]
    -- This assumes rows are indexed in the same way as columns are.
    primes = [1..(size matrix)]

testMatrix = SBMatrix (I.fromList [(1, SBVector (S.fromList [4,6])), (2, SBVector (S.fromList [1,3])), (3, SBVector (S.fromList [3,7,8])), (4, SBVector (S.fromList [2,4,7])), (5, SBVector (S.fromList [2])), (6, SBVector (S.fromList [5,8])), (7, SBVector (S.fromList [1,4,5])), (8, SBVector (S.fromList [3,7])), (9, SBVector (S.fromList [2,5,6])), (10, SBVector (S.fromList [1,8]))])

-- evaluate :: F2Poly -> SBMatrix -> SBMatrix
-- evaluate (SBPolynomial x) matrix = foldr (\i acc -> (stimes i matrix) + acc) (SBMatrix I.empty) listOfIndices
--   where
--     listOfIndices = map (\i -> (length x) - 1 - i) $ L.findIndices id x
