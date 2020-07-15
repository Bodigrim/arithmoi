{-# LANGUAGE TypeFamilies #-}

module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , DBVector(..)
  , SBMatrix(..)
  , isNull
  , dot
  , mult
  , size
  , linearSolve
  , testLinearSolver
  , getRandomSBVectors
  ) where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
-- import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Monad.ST
import Debug.Trace
import Data.Semigroup()
import System.Random
import System.IO.Unsafe
import GHC.Clock
import Data.Bit
import Data.Bits
import Data.Foldable

-- Sparse Binary Vector
newtype SBVector = SBVector
  { unSBVector :: U.Vector Int
  } deriving (Show)

-- Dense Binary Vector
newtype DBVector = DBVector
  { unDBVector :: U.Vector Bit
  } deriving (Show, Eq)

-- Sparse Binary Matrix
newtype SBMatrix = SBMatrix
  { unSBMatrix :: V.Vector SBVector
  } deriving (Show)

instance Semigroup DBVector where
  DBVector v1 <> DBVector v2 = DBVector $ zipBits xor v1 v2

isNull :: DBVector -> Bool
isNull (DBVector v) = null $ listBits v

dot :: DBVector -> DBVector -> Bit
dot (DBVector v1) (DBVector v2) = Bit $ odd . countBits $ zipBits (.&.) v1 v2

-- mult :: SBMatrix -> DBVector -> DBVector
-- mult (SBMatrix matrix) (DBVector vector) = runST $ do
--   -- let len = getMax (fromJust (foldMap (fmap (Max . fst) . S.maxView . unSBVector) matrix)) + 1
--   -- vs <- MU.new (size m)
--   -- traceShowM (size m, len)
--   vs <- MU.new (U.length vector)
--   traverse_ (traverse_ (unsafeFlipBit vs) . S.toList . unSBVector . (matrix V.!)) (S.toList vector)
--   ws <- U.unsafeFreeze vs
--   pure $ DBVector ws

mult :: SBMatrix -> DBVector -> DBVector
mult (SBMatrix matrix) (DBVector vector) = runST $ do
  vs <- MU.new (U.length vector)
  traverse_ (traverse_ (unsafeFlipBit vs) . U.toList . unSBVector . (matrix V.!)) (listBits vector)
  ws <- U.unsafeFreeze vs
  pure $ DBVector ws

-- -- traverse_ = flip forM_ (S.toList vector) $ \column ->
-- --
-- -- forM_ (S.toList vector) $ \columnIndex ->
-- --   forM_ (S.toList $ unSBVector (matrix V.! columnIndex)) $ \i ->
-- --     unsafeFlipBit vs i

-- traverse_ (traverse_ (unsafeFlipBit vs) . S.toList . unSBVector . (matrix V.!)) (S.toList vector)

size :: SBMatrix -> Int
size (SBMatrix m) = V.length m

linearSolve :: SBMatrix -> DBVector
linearSolve matrix = linearSolveHelper 1 matrix randomVectors 1
  where
    -- Make sure random vectors are not empty. The floating point number is the density of the random vectors
    randomVectors = getRandomDBVectors (size matrix) 0.5 $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

linearSolveHelper :: F2Poly -> SBMatrix -> [DBVector] -> Int -> DBVector
linearSolveHelper _ _ [] _ = error "No random vectors."
linearSolveHelper previousPoly matrix (x : otherVecs) counter
  | isNull potentialSolution && counter > 100 = trace ("Fail: " ++ show matrix) $ error "Incorrect algorithm."
  | isNull potentialSolution                  = trace ("Counter: " ++ show counter) $ linearSolveHelper potentialMinPoly matrix otherVecs (counter + 1)
  -- This is a good solution.
  | otherwise                                 = trace ("Counter: " ++ show counter) $ potentialSolution
  where
    potentialSolution = findSolution singularities matrix almostZeroVector
    almostZeroVector = evaluate matrix z reducedMinPoly
    (singularities, reducedMinPoly) = L.break (== Bit True) (U.toList $ unF2Poly potentialMinPoly)
    -- lowest common multiple of previousPoly and candidateMinPoly
    potentialMinPoly = lcm previousPoly candidateMinPoly
    candidateMinPoly = findCandidatePoly matrix z x
    -- z has to be picked outside of the image of the matrix.
    -- This works only because we know that the last row of the matrix is zero.
    -- To solve a general singular square matrix, this won't work.
    -- To deal with general case, it is desirable to wirte both variants.
    z = DBVector $ U.update (U.replicate (size matrix) (Bit False)) $ U.singleton (size matrix - 1, Bit True)

findSolution :: [Bit] -> SBMatrix -> DBVector -> DBVector
-- It is ideal to define a family of parameters indexed by the length of the vector.
-- This is then an abelian group. In this workaround, @DBVector (U.empty)@ can
-- be used as it is not summed to any other element.
findSolution [] _ _ = DBVector (U.empty)
findSolution (_ : xs) matrix vector
  | isNull result = vector
  | otherwise     = findSolution xs matrix result
  where
    result = matrix `mult` vector

-- This routine takes a polynomial p, a matrix A and a vector w and
-- returns p(A)w. It assumes the first coefficient of p is non zero,
-- in particular that it is non empty. This makes the implementation
-- easier as there is no need to write the identity matrix.
evaluate :: SBMatrix -> DBVector -> [Bit] -> DBVector
-- Here @DBVector (U.empty)@ cannot be used since summing it with any other
-- vector gives the empty vector again.
evaluate matrix w = foldr (\coeff acc -> (matrix `mult` acc) <> (if unBit coeff then w else zero)) zero
  where zero = DBVector $ U.replicate (size matrix) (Bit False)

findCandidatePoly :: SBMatrix -> DBVector -> DBVector -> F2Poly
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
generateData :: SBMatrix -> DBVector -> DBVector -> F2Poly
generateData matrix z x = toF2Poly $ U.fromList $ reverse $ map (x `dot`) matrixPowers
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z

-- Infinite lists of random DBVectors.
getRandomDBVectors :: Int -> Double -> StdGen -> [DBVector]
getRandomDBVectors numberOfColumns density gen = go $ randomRs (0, 1) gen
  where
    go :: [Double] -> [DBVector]
    go list = newVector : go backOfList
      where
        newVector = DBVector $ U.fromList $ map (\d -> Bit (d > density)) frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfColumns list

-- Input number of columns of matrix and sparsity coefficient. It returns a random matrix.
testLinearSolver :: Int -> Double -> Bool
testLinearSolver dim density = isNull $ mat `mult` sol
  where
    sol = linearSolve mat
    mat = SBMatrix (V.fromList listOfColumns)
    -- -2 is arbitrary. It means that the number of rows is at most one less than
    -- the number of columns
    listOfColumns = L.take dim $ getRandomSBVectors (dim - 1) density $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

-- Infinite lists of random SBVectors. Only used for testing.
getRandomSBVectors :: Int -> Double -> StdGen -> [SBVector]
getRandomSBVectors numberOfRows density gen = go randomEntries
  where
    randomEntries = zip (cycle [0..(numberOfRows - 1)]) (randomRs (0, 1) gen)
    go :: [(Int, Double)] -> [SBVector]
    go list = newVector : go backOfList
      where
        newVector = SBVector (U.fromList listOfEntries)
        listOfEntries = fmap fst $ filter (\(_, rDouble) -> rDouble < density) frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfRows list

-- 1) Investigate rare cases where code does not terminates.
-- 2) Implement dense matrices rather than sparse.
-- 3) See if using direct computations in BerlekampMassey makes it faster.
