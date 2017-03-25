import Test.Tasty

import qualified Math.NumberTheory.GCDTests as GCD
import qualified Math.NumberTheory.GCD.LowLevelTests as GCDLowLevel

import qualified Math.NumberTheory.Recurrencies.BilinearTests as RecurrenciesBilinear
import qualified Math.NumberTheory.Recurrencies.LinearTests as RecurrenciesLinear

import qualified Math.NumberTheory.ModuliTests as Moduli

import qualified Math.NumberTheory.MoebiusInversionTests as MoebiusInversion
import qualified Math.NumberTheory.MoebiusInversion.IntTests as MoebiusInversionInt

import qualified Math.NumberTheory.Powers.CubesTests as Cubes
import qualified Math.NumberTheory.Powers.FourthTests as Fourth
import qualified Math.NumberTheory.Powers.GeneralTests as General
import qualified Math.NumberTheory.Powers.SquaresTests as Squares

import qualified Math.NumberTheory.PrimesTests as Primes
import qualified Math.NumberTheory.Primes.CountingTests as Counting
import qualified Math.NumberTheory.Primes.HeapTests as Heap
import qualified Math.NumberTheory.Primes.SieveTests as Sieve

import qualified Math.NumberTheory.GaussianIntegersTests as Gaussian

import qualified Math.NumberTheory.ArithmeticFunctionsTests as ArithmeticFunctions
import qualified Math.NumberTheory.UniqueFactorisationTests as UniqueFactorisation
import qualified Math.NumberTheory.ZetaTests as Zeta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ testGroup "Powers"
    [ Cubes.testSuite
    , Fourth.testSuite
    , General.testSuite
    , Squares.testSuite
    ]
  , testGroup "GCD"
    [ GCD.testSuite
    , GCDLowLevel.testSuite
    ]
  , testGroup "Recurrencies"
    [ RecurrenciesLinear.testSuite
    , RecurrenciesBilinear.testSuite
    ]
  , testGroup "Moduli"
    [ Moduli.testSuite
    ]
  , testGroup "MoebiusInversion"
    [ MoebiusInversion.testSuite
    , MoebiusInversionInt.testSuite
    ]
  , testGroup "Primes"
    [ Primes.testSuite
    , Counting.testSuite
    , Heap.testSuite
    , Sieve.testSuite
    ]
  , testGroup "Gaussian"
    [ Gaussian.testSuite
    ]
  , testGroup "ArithmeticFunctions"
    [ ArithmeticFunctions.testSuite
    ]
  , testGroup "UniqueFactorisation"
    [ UniqueFactorisation.testSuite
    ]
  , testGroup "Zeta"
    [ Zeta.testSuite
    ]
  ]
