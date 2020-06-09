module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve
import Control.Monad.ST
import qualified Data.IntSet as S
import qualified Math.NumberTheory.Primes.IntSet as SP
import qualified Data.Vector as V

linearSolver :: [[Prime Int]] -> Bool
linearSolver listMatrix = runST $ do
    let s = length listMatrix
        matrix = V.fromList (map SP.fromList listMatrix)
        indexedMatrix = V.zip (V.generate s (S.singleton . id)) matrix
    indexedMatrixM <- V.thaw indexedMatrix
    gaussianEliminationM indexedMatrixM
    indexedMatrixF <- V.unsafeFreeze indexedMatrixM

    let someSolutions = V.mapMaybe select indexedMatrixF
        select (is, _) = case (S.null is) of
            True  -> Nothing
            False -> Just is
        listOfSolutions = V.toList someSolutions

    pure (checkSolutions matrix listOfSolutions)

checkSolutions :: V.Vector SP.PrimeIntSet -> [S.IntSet] -> Bool
checkSolutions _ [] = True
checkSolutions matrix (x:xs) = isZero && (checkSolutions matrix xs)
    where
        isZero = (combination == SP.empty)
        combination = S.foldr add SP.empty x
        add columnIndex acc = xor (matrix V.! columnIndex) acc
        xor u v = (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u)

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ testSmallAndQuick "Linear algebra" linearSolver
  ]
