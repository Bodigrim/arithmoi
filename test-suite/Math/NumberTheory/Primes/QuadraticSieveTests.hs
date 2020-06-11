module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

-- import qualified Data.IntSet as S
-- import qualified Math.NumberTheory.Primes.IntSet as SP
import Test.Tasty
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

-- -- Solves a matrix by finding a basis for its kernel. Checks whether each
-- -- basis element is an actual solution for the matrix.
-- -- This solves via mutable vectors.linearSolverL :: [[Prime Int]] -> Bool
-- linearSolver :: [[Prime Int]] -> Bool
-- linearSolver listMatrix = checkSolutions matrix solutionBasis
--     where
--         s = length listMatrix
--         matrix = map SP.fromList listMatrix
--         indexedMatrix = zip (map S.singleton [0..(s - 1)]) matrix
--         solutionBasis = gaussianElimination indexedMatrix
--
-- -- This checks through all the vector in the basis of the kernel.
-- checkSolutions :: [SP.PrimeIntSet] -> [S.IntSet] -> Bool
-- checkSolutions _ [] = True
-- checkSolutions matrix (x:xs) = isZero && checkSolutions matrix xs
--     where
--         isZero = combination == mempty
--         combination = S.foldr add mempty x
--         add columnIndex = xor (matrix !! columnIndex)
--         xor u v = (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u)

quadraticRelation :: Prime Integer -> Prime Integer -> Bool
quadraticRelation primeP primeQ = (p == 2) || (q == 2) || (p == q) || foldr checkQuadratic True (quadraticSieve (p * q) 1000 2000)
    where
        checkQuadratic (x, y) acc = (x ^ (2 :: Int) - y ^ (2 :: Int)) `mod` (p * q) == 0 && acc
        p = unPrime primeP
        q = unPrime primeQ

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ --testSmallAndQuick "Linear algebra" linearSolver
    testSmallAndQuick "Factoring" quadraticRelation
  ]
