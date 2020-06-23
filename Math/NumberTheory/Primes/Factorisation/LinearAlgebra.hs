module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , SBMatrix(..)
  , SBPolynomial(..)
  , testMatrix
  , mult
  , linearSolve
  ) where

import Prelude hiding (quotRem)
import Data.Semigroup
import System.Random
import Debug.Trace
import qualified Data.List as L
import qualified Data.IntSet as S
import qualified Data.IntMap as I
import System.IO.Unsafe
import GHC.Clock
import Data.Euclidean
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

-- The first element of the list is the leading coefficient assumed
-- to be non-zero. 0 = False, 1 = True.
newtype SBPolynomial = SBPolynomial
  { polynomial :: [Bool]
  } deriving (Show)

instance Eq SBPolynomial where
  (SBPolynomial x) == (SBPolynomial y) = (L.dropWhile (not . id) x) == (L.dropWhile (not . id) y)

instance Num SBPolynomial where
    (+) p@(SBPolynomial x) q@(SBPolynomial y)
      | difference == 0 = SBPolynomial (zipWith (/=) x y)
      | difference > 0  = p + (SBPolynomial (L.replicate difference (False) ++ y))
      | difference < 0  = (SBPolynomial (L.replicate difference (False) ++ x)) + q
      where
        difference = length x - length y
    -- Not very efficient
    (*) (SBPolynomial []) _ = SBPolynomial []
    (*) _ (SBPolynomial []) = SBPolynomial []
    (*) (SBPolynomial (xh : xs)) (SBPolynomial y) = if xh then SBPolynomial (y ++ (L.replicate (length xs) False)) + nextLine else nextLine
      where
        nextLine = (SBPolynomial xs) * (SBPolynomial y)
    negate = id

instance S.Semiring SBPolynomial where
    plus          = (+)
    times         = (*)
    zero          = SBPolynomial []
    one           = SBPolynomial [True]

instance S.Ring SBPolynomial where
    negate = negate

instance GcdDomain SBPolynomial

-- In repeating quotRem, the leading coefficient may be zero. This can create
-- confusion. Edge cases can be handled better.
instance Euclidean SBPolynomial where
  degree (SBPolynomial []) = fromIntegral (0 :: Int)
  degree (SBPolynomial x) = fromIntegral $ length (L.dropWhile (not . id) x)
  quotRem _ (SBPolynomial []) = error "Division by zero."
  quotRem (SBPolynomial []) _ = (SBPolynomial [], SBPolynomial [])
  quotRem p@(SBPolynomial x@(xh : xs)) q@(SBPolynomial y@(yh : ys))
    | not yh              = quotRem p (SBPolynomial (L.dropWhile (not . id) y))
    | length x < length y = (SBPolynomial [], SBPolynomial (L.dropWhile (not . id) x))
    | otherwise           = (SBPolynomial (xh : polynomial (fst (quotRem rs q))), snd (quotRem rs q))
    where
      -- Is there a better way to achieve this?
      rs = SBPolynomial (if xh then zipWith (/=) xs (ys ++ (repeat False)) else xs)

evaluate :: SBPolynomial -> SBMatrix -> SBMatrix
evaluate (SBPolynomial x) matrix = foldr (\i acc -> (stimes i matrix) + acc) (SBMatrix I.empty) listOfIndices
  where
    listOfIndices = map (\i -> (length x) - 1 - i) $ L.findIndices id x

randomSublist :: [Int] -> StdGen -> [Int]
randomSublist list gen = traceShowId $ fmap fst $ filter snd $ traceShowId $ zip list (randoms gen)

linearSolve :: SBMatrix -> SBVector
linearSolve matrix@(SBMatrix m) = findSolution $ almostZeroMatrix `mult` w
  where
    -- Rows of z are indexed by the columns of matrix
    z = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
    randomSequence = generateData matrix z
    -- The order of the arguments is exchanged
    minPoly = snd (gcdExt randomSequence (SBPolynomial (True : (L.replicate (2 * (size matrix)) False))))
    reducedMinPoly = SBPolynomial (L.dropWhileEnd (not . id) (polynomial minPoly))
    almostZeroMatrix = evaluate reducedMinPoly matrix
    w = SBVector (S.fromList $ randomSublist (I.keys m) (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec))))
    findSolution :: SBVector -> SBVector
    findSolution v
      | set (matrix `mult` v) == mempty = v
      | otherwise                       = findSolution $ matrix `mult` v

-- The input is a matrix B and a random vector z
generateData :: SBMatrix -> SBVector -> SBPolynomial
generateData matrix z = SBPolynomial (map (\v -> not $ S.null (set (x `mult` v))) matrixPowers)
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z
    x = SBMatrix (foldr (\p acc -> I.insert p (SBVector (S.singleton 1)) acc) initialMap randomPrimes)
    randomPrimes = randomSublist primes (mkStdGen (fromIntegral (unsafePerformIO getMonotonicTimeNSec)))
    initialMap = I.fromList ([(p, mempty) | p <- primes])
    -- This should be replaced by factorBase = [nextPrime 2..precPrime b]
    -- This assumes rows are indexed in the same way as columns are.
    primes = [1..(size matrix)]

testMatrix = SBMatrix (I.fromList [(1, SBVector (S.fromList [1,2])), (2, SBVector (S.fromList [2,3])), (3, SBVector (S.fromList [1,3]))])
