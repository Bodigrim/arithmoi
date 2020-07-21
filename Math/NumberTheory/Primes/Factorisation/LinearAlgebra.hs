{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , DBVector(..)
  , SBMatrix(..)
  , dot
  , mult
  , size
  , linearSolve
  , testLinearSolver
  , testGauss
  ) where

import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed.Mutable.Sized as SMU
import Control.Monad.ST
import Debug.Trace
import Data.Semigroup()
import System.Random
import System.IO.Unsafe
import GHC.Clock
import Data.Bit
import Data.Bits
import Data.Foldable
import GHC.TypeNats hiding (Mod)
import Data.Mod.Word
import Unsafe.Coerce
import Data.Maybe
import Data.Proxy

import qualified Data.IntSet as S

-- Sparse Binary Vector
newtype SBVector (k :: Nat) = SBVector { unSBVector :: U.Vector (Mod k) }

-- data SomeSBVector where
--   SomeSBVector :: KnownNat k => SBVector k -> SomeSBVector

-- Dense Binary Vector
newtype DBVector (k :: Nat) = DBVector { unDBVector :: SU.Vector k Bit }
  deriving (Eq, Show)

-- Sparse Binary Matrix
newtype SBMatrix (k :: Nat) = SBMatrix { unSBMatrix :: SV.Vector k (SBVector k) }

instance KnownNat k => Semigroup (DBVector k) where
  DBVector v1 <> DBVector v2 = DBVector $ SU.withVectorUnsafe (zipBits xor (SU.fromSized v1)) v2

instance KnownNat k => Monoid (DBVector k) where
  mempty = DBVector $ SU.replicate (Bit False)

listBits' :: KnownNat k => DBVector k -> [Mod k]
listBits' = unsafeCoerce listBits

flipBit' :: KnownNat k => SMU.MVector k s Bit -> Mod k -> ST s ()
flipBit' = unsafeCoerce (unsafeFlipBit :: MU.MVector s Bit -> Int -> ST s ())

index' :: KnownNat k => SBMatrix k -> Mod k -> SBVector k
index' = unsafeCoerce SV.index

-- isNull :: DBVector -> Bool
-- isNull (DBVector v) = null $ listBits v

-- dot :: DBVector -> DBVector -> Bit
-- dot (DBVector v1) (DBVector v2) = Bit $ odd . countBits $ zipBits (.&.) v1 v2

dot :: KnownNat k => DBVector k -> DBVector k -> Bit
dot (DBVector v1) (DBVector v2) = Bit $ odd . countBits $ zipBits (.&.) (SU.fromSized v1) (SU.fromSized v2)

mult :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k
mult matrix vector = runST $ do
  vs <- SMU.new
  traverse_ (traverse_ (flipBit' vs) . U.toList . unSBVector . (matrix `index'`)) $ listBits' vector
  ws <- SU.unsafeFreeze vs
  pure $ DBVector ws

-- -- traverse_ = flip forM_ (S.toList vector) $ \column ->
-- --
-- -- forM_ (S.toList vector) $ \columnIndex ->
-- --   forM_ (S.toList $ unSBVector (matrix V.! columnIndex)) $ \i ->
-- --     unsafeFlipBit vs i

-- traverse_ (traverse_ (unsafeFlipBit vs) . S.toList . unSBVector . (matrix V.!)) (S.toList vector)

size :: KnownNat k => SBMatrix k -> Int
size (SBMatrix m) = SV.length m

linearSolve :: KnownNat k => SBMatrix k -> DBVector k
linearSolve matrix = linearSolveHelper 1 matrix randomVectors 1
  where

    -- The floating point number is the density of the random vectors
    randomVectors = getRandomDBVectors (size matrix) 0.1 $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

linearSolveHelper :: KnownNat k => F2Poly -> SBMatrix k -> [DBVector k] -> Int -> DBVector k
linearSolveHelper _ _ [] _ = error "Not enough random vectors"
linearSolveHelper _ _ (_ : []) _ = error "Not enough random vectors"
linearSolveHelper previousPoly matrix (z : x : otherVecs) counter
--  | potentialSolution == mempty && counter > 100 = error "Incorrect algorithm." --trace ("Fail: " ++ show matrix)
  | potentialSolution == mempty && counter <= 5 = linearSolveHelper potentialMinPoly matrix (z : otherVecs) (counter + 1)
  -- Change vector z
  | potentialSolution == mempty && counter > 5  = linearSolveHelper 1 matrix otherVecs 1
  -- This is a good solution.
  | otherwise                                    = trace ("Counter: " ++ show counter) potentialSolution
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
    -- z = DBVector $ SU.update (SU.replicate (Bit False)) $ SU.singleton (size matrix - 1, Bit True)

findSolution :: KnownNat k => [Bit] -> SBMatrix k -> DBVector k -> DBVector k
-- It is ideal to define a family of parameters indexed by the length of the vector.
-- This is then an abelian group. In this workaround, @DBVector (U.empty)@ can
-- be used as it is not summed to any other element.
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
evaluate :: KnownNat k => SBMatrix k -> DBVector k -> [Bit] -> DBVector k
-- Here @DBVector (U.empty)@ cannot be used since summing it with any other
-- vector gives the empty vector again.
evaluate matrix w = foldr (\coeff acc -> (matrix `mult` acc) <> (if unBit coeff then w else mempty)) mempty

findCandidatePoly :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k -> F2Poly
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
generateData :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k -> F2Poly
generateData matrix z x = toF2Poly $ U.fromList $ reverse $ map (x `dot`) matrixPowers
  where
    matrixPowers = L.take (2 * size matrix) $ L.iterate (matrix `mult`) z

-- Infinite lists of random DBVectors.
getRandomDBVectors :: KnownNat k => Int -> Double -> StdGen -> [DBVector k]
getRandomDBVectors numberOfColumns density gen = go $ randomRs (0, 1) gen
  where
    go :: KnownNat k => [Double] -> [DBVector k]
    go list = newVector `seq` newVector : go backOfList
      where
        newVector = DBVector $ fromJust $ SU.fromList $ map (\d -> Bit (d > density)) frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfColumns list

-- Input number of columns of matrix and sparsity coefficient. It returns a random matrix.
testLinearSolver :: Int -> Double -> Bool
testLinearSolver dim density = case someNatVal (fromIntegral dim) of
  SomeNat (_ :: Proxy dim) -> let sol :: DBVector dim = linearSolve mat in
    mat `mult` sol == mempty
      where
        mat = SBMatrix $ fromJust $ SV.fromList listOfColumns
        -- -2 is arbitrary. It means that the number of rows is at most one less than
        -- the number of columns
        listOfColumns = L.take dim $ getRandomSBVectors (dim - 2) density $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

getRandomSBVectors :: KnownNat k => Int -> Double -> StdGen -> [SBVector k]
getRandomSBVectors numberOfRows density gen = go randomEntries
  where
    randomEntries = map (< density) $ randomRs (0, 1) gen
    go :: KnownNat k => [Bool] -> [SBVector k]
    go list = newVector `seq` (newVector : go backOfList)
      where
        newVector = SBVector (U.fromList listOfEntries)
        listOfEntries = map fst $ filter snd $ zip [minBound..maxBound] frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfRows list


--------------------------------------------------------------------------------

testGauss :: Int -> Double -> Bool
testGauss dim density = sol /= S.empty
  where
    sol = gaussianElimination mat
    mat = map (\(a, b) -> (S.singleton a, b)) $ zip [0..] listOfColumns
    listOfColumns = L.take dim $ getRandomIntSets (dim - 2) density $ mkStdGen $ fromIntegral $ unsafePerformIO getMonotonicTimeNSec

gaussianElimination :: [(S.IntSet, S.IntSet)] -> S.IntSet
gaussianElimination [] = S.empty
gaussianElimination (p@(indices, pivotFact) : xs) = case S.minView pivotFact of
  Just (pivot, _) -> gaussianElimination $ map (\q@(_, fact) -> if pivot `S.member` fact then add p q else q) xs
  Nothing    -> indices
  where
    add (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), (u S.\\ v) <> (v S.\\ u))

getRandomIntSets :: Int -> Double -> StdGen -> [S.IntSet]
getRandomIntSets numberOfRows density gen = go randomEntries
  where
    randomEntries = map (< density) $ randomRs (0, 1) gen
    go :: [Bool] -> [S.IntSet]
    go list = newVector `seq` (newVector : go backOfList)
      where
        newVector = S.fromList listOfEntries
        listOfEntries = map fst $ filter snd $ zip [0..] frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfRows list
