module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import qualified Data.Vector as V
import qualified Data.IntSet as S
import qualified Math.NumberTheory.Primes.IntSet as SP
import Control.Monad.ST
import Test.Tasty
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

-- Solves a matrix by finding a basis for its kernel. Checks whether each
-- basis element is an actual solution for the matrix.
-- This solves via mutable vectors.
linearSolverV :: [[Prime Int]] -> Bool
linearSolverV listMatrix = runST $ do
    let s = length listMatrix
        matrix = V.fromList (map SP.fromList listMatrix)
        indexedMatrix = V.zip (V.generate s S.singleton) matrix
    indexedMatrixM <- V.thaw indexedMatrix
    gaussianEliminationM indexedMatrixM
    indexedMatrixF <- V.unsafeFreeze indexedMatrixM

    let solutionBasis = V.mapMaybe (\(is,_) -> if S.null is then Nothing else Just is) indexedMatrixF
        listOfSolutions = V.toList solutionBasis

    pure (checkSolutionsV matrix listOfSolutions)

-- This checks through all the vector in the basis of the kernel.
checkSolutionsV :: V.Vector SP.PrimeIntSet -> [S.IntSet] -> Bool
checkSolutionsV _ [] = True
checkSolutionsV matrix (x:xs) = isZero && checkSolutionsV matrix xs
    where
        isZero = combination == SP.empty
        combination = S.foldr add SP.empty x
        add columnIndex = xor (matrix V.! columnIndex)
        xor u v = (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u)

-- Same as before but uses immutable lists.
linearSolverL :: [[Prime Int]] -> Bool
linearSolverL listMatrix = checkSolutionsL matrix solutionBasis
    where
        s = length listMatrix
        matrix = map SP.fromList listMatrix
        indexedMatrix = zip (map S.singleton [0..(s - 1)]) matrix
        solutionBasis = gaussianElimination indexedMatrix

checkSolutionsL :: [SP.PrimeIntSet] -> [S.IntSet] -> Bool
checkSolutionsL _ [] = True
checkSolutionsL matrix (x:xs) = isZero && checkSolutionsL matrix xs
    where
        isZero = combination == SP.empty
        combination = S.foldr add SP.empty x
        add columnIndex = xor (matrix !! columnIndex)
        xor u v = (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u)

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ testSmallAndQuick "Linear algebra with vectors" linearSolverV
  , testSmallAndQuick "Linear algebra with lists" linearSolverL
  ]
