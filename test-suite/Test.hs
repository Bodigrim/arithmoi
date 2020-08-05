import Test.Tasty
import Test.Tasty.Ingredients.Rerun

import qualified Math.NumberTheory.EuclideanTests as Euclidean

import qualified Math.NumberTheory.Recurrences.PentagonalTests as RecurrencesPentagonal
import qualified Math.NumberTheory.Recurrences.BilinearTests as RecurrencesBilinear
import qualified Math.NumberTheory.Recurrences.LinearTests as RecurrencesLinear

import qualified Math.NumberTheory.Moduli.ChineseTests as ModuliChinese
import qualified Math.NumberTheory.Moduli.ClassTests as ModuliClass
import qualified Math.NumberTheory.Moduli.CubicSymbolTests as ModuliCubic
import qualified Math.NumberTheory.Moduli.DiscreteLogarithmTests as ModuliDiscreteLogarithm
import qualified Math.NumberTheory.Moduli.EquationsTests as ModuliEquations
import qualified Math.NumberTheory.Moduli.JacobiTests as ModuliJacobi
import qualified Math.NumberTheory.Moduli.PrimitiveRootTests as ModuliPrimitiveRoot
import qualified Math.NumberTheory.Moduli.SingletonTests as ModuliSingleton
import qualified Math.NumberTheory.Moduli.SqrtTests as ModuliSqrt

import qualified Math.NumberTheory.MoebiusInversionTests as MoebiusInversion

import qualified Math.NumberTheory.PrefactoredTests as Prefactored

import qualified Math.NumberTheory.PrimesTests as Primes
import qualified Math.NumberTheory.Primes.CountingTests as Counting
import qualified Math.NumberTheory.Primes.FactorisationTests as Factorisation
import qualified Math.NumberTheory.Primes.LinearAlgebraTests as LinearAlgebra
import qualified Math.NumberTheory.Primes.QuadraticSieveTests as QuadraticSieve
import qualified Math.NumberTheory.Primes.SequenceTests as Sequence
import qualified Math.NumberTheory.Primes.SieveTests as Sieve
import qualified Math.NumberTheory.Primes.TestingTests as Testing

import qualified Math.NumberTheory.EisensteinIntegersTests as Eisenstein

import qualified Math.NumberTheory.GaussianIntegersTests as Gaussian

import qualified Math.NumberTheory.ArithmeticFunctionsTests as ArithmeticFunctions
import qualified Math.NumberTheory.ArithmeticFunctions.InverseTests as Inverse
import qualified Math.NumberTheory.ArithmeticFunctions.MertensTests as Mertens
import qualified Math.NumberTheory.ArithmeticFunctions.SieveBlockTests as SieveBlock
import qualified Math.NumberTheory.UniqueFactorisationTests as UniqueFactorisation
import qualified Math.NumberTheory.CurvesTests as Curves
import qualified Math.NumberTheory.SmoothNumbersTests as SmoothNumbers

import qualified Math.NumberTheory.Zeta.RiemannTests as Riemann
import qualified Math.NumberTheory.Zeta.DirichletTests as Dirichlet

import qualified Math.NumberTheory.DirichletCharactersTests as DirichletChar

import qualified Math.NumberTheory.RootsOfUnityTests as RootsOfUnity

import qualified Math.NumberTheory.DiophantineTests as Diophantine

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "All"
  [ Euclidean.testSuite
  , testGroup "Recurrences"
    [ RecurrencesPentagonal.testSuite
    , RecurrencesLinear.testSuite
    , RecurrencesBilinear.testSuite
    ]
  , testGroup "Moduli"
    [ ModuliChinese.testSuite
    , ModuliClass.testSuite
    , ModuliCubic.testSuite
    , ModuliDiscreteLogarithm.testSuite
    , ModuliEquations.testSuite
    , ModuliJacobi.testSuite
    , ModuliPrimitiveRoot.testSuite
    , ModuliSingleton.testSuite
    , ModuliSqrt.testSuite
    ]
  , MoebiusInversion.testSuite
  , Prefactored.testSuite
  , testGroup "Primes"
    [ Primes.testSuite
    , Counting.testSuite
    , Factorisation.testSuite
    , LinearAlgebra.testSuite
    , QuadraticSieve.testSuite
    , Sequence.testSuite
    , Sieve.testSuite
    , Testing.testSuite
    ]
  , Eisenstein.testSuite
  , Gaussian.testSuite
  , testGroup "ArithmeticFunctions"
    [ ArithmeticFunctions.testSuite
    , Inverse.testSuite
    , Mertens.testSuite
    , SieveBlock.testSuite
    ]
  , UniqueFactorisation.testSuite
  , Curves.testSuite
  , SmoothNumbers.testSuite
  , Diophantine.testSuite
  , testGroup "Zeta"
    [ Riemann.testSuite
    , Dirichlet.testSuite
    ]
  , DirichletChar.testSuite
  , RootsOfUnity.testSuite
  ]
