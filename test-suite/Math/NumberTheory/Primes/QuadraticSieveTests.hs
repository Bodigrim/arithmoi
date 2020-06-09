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
linearSolver :: [[Prime Int]] -> Bool
linearSolver listMatrix = runST $ do
    let s = length listMatrix
        matrix = V.fromList (map SP.fromList listMatrix)
        indexedMatrix = V.zip (V.generate s S.singleton) matrix
    indexedMatrixM <- V.thaw indexedMatrix
    gaussianEliminationM indexedMatrixM
    indexedMatrixF <- V.unsafeFreeze indexedMatrixM

    let someSolutions = V.mapMaybe (\(is,_) -> if S.null is then Nothing else Just is) indexedMatrixF
        listOfSolutions = V.toList someSolutions

    pure (checkSolutions matrix listOfSolutions)

-- This checks through all the vector in the basis of the kernel.
checkSolutions :: V.Vector SP.PrimeIntSet -> [S.IntSet] -> Bool
checkSolutions _ [] = True
checkSolutions matrix (x:xs) = isZero && checkSolutions matrix xs
    where
        isZero = combination == SP.empty
        combination = S.foldr add SP.empty x
        add columnIndex = xor (matrix V.! columnIndex)
        xor u v = (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u)

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ testSmallAndQuick "Linear algebra" linearSolver
  ]
